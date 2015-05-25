%% Copyright (c) 2015 Peter Lemenkov.
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

-module(testkv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring childrens
-define(CHILD(I), {I, {I, start_link, []}, transient, 5000, worker, [I]}).
-define(CHILD(I, P), {I, {I, start_link, [P]}, transient, 5000, worker, [I]}).
-define(CHILD(I, P, F), {I, {I, F, [P]}, transient, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Ip = {127,0,0,1},
	Port = 8080,

	UdpIp = Ip,
	TcpIp = Ip,

	UdpPort = Port + 1,
	TcpPort = Port + 2,
	HttpPort = Port + 0,

	UdpListener = ?CHILD(udp_listener, [UdpIp, UdpPort]),
	TcpListener = ?CHILD(tcp_listener, [TcpIp, TcpPort]),
	HttpListener = ?CHILD(mochiweb_http, [{loop, {http_listener, dispatch}}, {port, HttpPort}, {name, http_listener}], start),

	StorageProcessor = ?CHILD(testkv),
	StorageHandler = ?CHILD(storage),

	{ok, { {one_for_one, 5, 10}, [UdpListener, TcpListener, HttpListener, StorageProcessor, StorageHandler]} }.
