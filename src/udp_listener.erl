%% Copyright (c) 2011 Peter Lemenkov.
%%
%% The MIT License
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%

-module(udp_listener).
-author('lemenkov@gmail.com').

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

start(Args) ->
	gen_server:start({local, udp_listener}, ?MODULE, Args, []).
start_link(Args) ->
	gen_server:start_link({local, udp_listener}, ?MODULE, Args, []).

init ([{I0, I1, I2, I3, I4, I5, I6, I7} = IPv6, Port]) when
	is_integer(I0), I0 >= 0, I0 < 65535,
	is_integer(I1), I1 >= 0, I1 < 65535,
	is_integer(I2), I2 >= 0, I2 < 65535,
	is_integer(I3), I3 >= 0, I3 < 65535,
	is_integer(I4), I4 >= 0, I4 < 65535,
	is_integer(I5), I5 >= 0, I5 < 65535,
	is_integer(I6), I6 >= 0, I6 < 65535,
	is_integer(I7), I7 >= 0, I7 < 65535 ->
	process_flag(trap_exit, true),
	{ok, Fd} = gen_udp:open(Port, [{ip, IPv6}, {active, true}, binary, inet6]),
	error_logger:warning_msg("UDP listener: started at [~s:~w]~n", [inet_parse:ntoa(IPv6), Port]),
	{ok, Fd};
init ([{I0, I1, I2, I3} = IPv4, Port]) when
	is_integer(I0), I0 >= 0, I0 < 256,
	is_integer(I1), I1 >= 0, I1 < 256,
	is_integer(I2), I2 >= 0, I2 < 256,
	is_integer(I3), I3 >= 0, I3 < 256 ->
	process_flag(trap_exit, true),
	{ok, Fd} = gen_udp:open(Port, [{ip, IPv4}, {active, true}, binary]),
	error_logger:warning_msg("UDP listener: started at [~s:~w]~n", [inet_parse:ntoa(IPv4), Port]),
	{ok, Fd}.

handle_call(Call, _From, State) ->
	error_logger:error_msg("UDP listener: strange call: ~p~n", [Call]),
	{stop, {error, {unknown_call, Call}}, State}.

handle_cast(Cast, State) ->
	error_logger:error_msg("UDP listener: strange cast: ~p~n", [Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

% Fd from which message arrived must be equal to Fd from our state
handle_info({udp, Fd, Ip, Port, Msg}, Fd) ->
	Begin = {MegaSecs, Secs, MicroSecs} = os:timestamp(),
	spawn(fun() ->
		error_logger:info_msg("UDP listener: command ~s recv from ~s:~b at ~f~n", [<< <<X>> || <<X>> <= Msg, X /= 0, X /= $\n>>, inet_parse:ntoa(Ip), Port, MegaSecs*1000000+Secs+MicroSecs/1000000]),
		Data = gen_server:call(testkv, {rawdata, Msg}),
		gen_udp:send(Fd, Ip, Port, Data),
		End = {MegaSecs1, Secs1, MicroSecs1} = os:timestamp(),
		error_logger:info_msg("UDP listener: reply ~s sent to ~s:~b at ~f (elapsed time: ~b microsec)~n", [Data, inet_parse:ntoa(Ip), Port, MegaSecs1*1000000+Secs1+MicroSecs1/1000000, timer:now_diff(End, Begin)])
	end),
	{noreply, Fd};

handle_info(Info, State) ->
	error_logger:error_msg("UDP listener: strange info: ~p~n", [Info]),
	{stop, {error, {unknown_info, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, Fd) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	gen_udp:close(Fd),
	error_logger:warning_msg("UDP listener: terminated due to reason: ~p (allocated ~b bytes)", [Reason, Bytes]).
