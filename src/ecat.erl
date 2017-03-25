%%#!/usr/bin/env escript
-module(ecat).
-export([main/1, cat/2]).

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
		%% Doesn't appear possible to support -u in Erlang.
		{ $u, flag, unbuffered },
		{ $v, flag, show_control }
	]) of
	{ok, Options, ArgsN} ->
		process(Options, ArgsN);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.

process(Opts, []) ->
	io:setopts(standard_io, [binary]),
	cat(standard_io, Opts);
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
		cat(Fp, Opts),
		file:close(Fp);
	Error ->
		throw(Error)
	end.

cat(Fp, Opts) ->
	cat(Fp, which_putline(Opts), 1).
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

show(Octet, Tab, Eol) ->
	if
	Octet == 9 ->
		case Tab of
		true ->
			<<"^I">>;
		false ->
			<<Octet:8>>
		end;
	Octet == 10 ->
		case Eol of
		true ->
			<<$$, Octet:8>>;
		false ->
			<<Octet:8>>
		end;
	Octet == 127 ->
		<<"^?">>;
	Octet > 127 ->
		<<"M-", (show(Octet - 128, Tab, Eol))/binary>>;
	Octet >= 32 ->
		<<Octet:8>>;
	Octet < 32 ->
		<<$^, (Octet + $@):8>>
	end.

putch(<<>>, _, _) ->
	ok;
putch(<<Octet:8, Rest/binary>>, Tab, Eol) ->
	file:write(standard_io, show(Octet, Tab, Eol)),
	putch(Rest, Tab, Eol).

which_putch(Opts) ->
	Eol = proplists:get_value(show_eol, Opts, false),
	Tab = proplists:get_value(show_tab, Opts, false),
	case {proplists:get_value(show_control, Opts, false), Tab, Eol} of
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
