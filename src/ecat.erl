%%#!/usr/bin/env escript
-module(ecat).
-export([main/1]).

-define(BUFSIZ, 1024).

usage() ->
	io:format("usage: ecat [-entuv][-B size][file ...]~n"),
	io:format("-B size\t\tread buffer size; default 1024~n"),
	io:format("-e\t\tshow end of line as $, implies -v~n"),
	io:format("-n\t\tnumbers the output lines~n"),
	io:format("-t\t\tshow tabs as ^I, implies -v~n"),
	io:format("-u\t\tunbuffered output~n"),
	io:format("-v\t\tshow non-printable control characters ^X~n"),
	halt(2).

main(Args) ->
	case egetopt:parse(Args, [
		{ $B, param, read_size },
		{ $e, flag, show_eol },
		{ $n, flag, line_numbers },
		{ $t, flag, show_tab },
		%% Doesn't appear possible to support in Erlang
		{ $u, flag, unbuffered },
		{ $v, flag, show_cntl }
	]) of
	{ok, Options, ArgsN} ->
		process(Options, ArgsN);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.

process(Opts, []) ->
	io:setopts(standard_io, [binary]),
	cat(standard_io, which_putline(Opts));
process(Opts, Files) ->
	process_files(Opts, Files).

process_files(_Opts, []) ->
	ok;
process_files(Opts, [Filename | Rest]) ->
	try
		process_file(Opts, Filename)
	catch
		throw:{error, Reason} ->
			io:format(standard_error, "ecat: ~s: ~s~n", [Filename, str:error(Reason)]),
			halt(1)
	end,
	process_files(Opts, Rest).

process_file(Opts, Filename) ->
	Size = proplists:get_value(read_size, Opts, ?BUFSIZ),
	case file:open(Filename, [read, binary, {read_ahead, Size}]) of
	{ok, Fp} ->
		cat(Fp, which_putline(Opts)),
		file:close(Fp);
	Error ->
		throw(Error)
	end.

cat(Fp, Putline) ->
	cat(Fp, Putline, 1).
cat(Fp, Putline, Lineno) ->
	case file:read_line(Fp) of
	eof ->
		ok;
	{ok, Line} ->
		Putline(Line, Lineno),
		cat(Fp, Putline, Lineno+1);
	{error, Reason} ->
		throw({error, Reason})
	end.

putch(<<>>, _, _) ->
	ok;
putch(<<Octet:8, Rest/binary>>, Tab, Eol) ->
	if
	Octet == 9 ->
		case Tab of
		true ->
			file:write(standard_io, <<"^I">>);
		false ->
			file:write(standard_io, <<$\t:8>>)
		end;
	Octet == 10 ->
		case Eol of
		true ->
			file:write(standard_io, <<$$>>);
		false ->
			ok
		end,
		file:write(standard_io, <<Octet:8>>);
	Octet == 127 ->
		file:write(standard_io, <<"^?">>);
	Octet > 127 ->
		file:write(standard_io, <<"M-">>),
		putch(<<(Octet-128):8>>, Tab, Eol);
	Octet >= 32 ->
		file:write(standard_io, <<Octet:8>>);
	Octet < 32 ->
		file:write(standard_io, <<$^, (Octet+$@):8>>)
	end,
	putch(Rest, Tab, Eol).

which_putch(Opts) ->
	Eol = proplists:get_value(show_eol, Opts, false),
	Tab = proplists:get_value(show_tab, Opts, false),
	case {proplists:get_value(show_cntl, Opts, false), Tab, Eol} of
	{false, false, false} ->
		fun (<<>>) ->
			ok;
		(Octets) ->
			file:write(standard_io, Octets)
		end;
	_ ->
		fun (Octets) ->
			putch(Octets, Tab, Eol) end
	end.

putline(Line, Lineno, Putch) ->
	io:format("~6B  ", [Lineno]),
	Putch(Line).

which_putline(Opts) ->
	Putch = which_putch(Opts),
	case proplists:get_value(line_numbers, Opts, false) of
	true ->
		fun (Line, Lineno) ->
			putline(Line, Lineno, Putch)
		end;
	false ->
		fun (Line, _Lineno) ->
			Putch(Line)
		end
	end.
