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

-module(http_listener).
-author('lemenkov@gmail.com').

-export([dispatch/1]).

dispatch(Req) ->
	Path = Req:get(path),
	case string:tokens(Path, "/") of
		[K | _ ] ->
			Key = list_to_binary(K),
			dispatch2(Req:get(method), Key, Req);
		_ ->
			error_logger:warning_msg("UNKNOWN: ~p~n", [Path]),
			Req:respond({404, [], "404 Not found\r\n"})
	end.

dispatch2('GET', Key, Req) ->
	error_logger:warning_msg("KEY: ~p~n", [Key]),
	{Code, JSON} = case gen_server:call(testkv, {get, Key}) of
		{error, notfound} -> {404, [{key, Key}, {operation, get}, {status, [{error,notfound}]}]};
		{ok, Val} -> {200, [{key, Key}, {operation, get}, {status, ok}, {val, Val}]}
	end,
	Req:respond({Code, [{"Content-Type", "application/json"}], mochijson2:encode(JSON)});

dispatch2('PUT', Key, Req) ->
	error_logger:warning_msg("PUT: ~p~n", [Key]),
	Body = Req:recv_body(),
	gen_server:call(testkv, {put, {Key, Body}}),
	JSON = [{key, Key}, {operation, put}, {status, ok}],
	Req:respond({200, [{"Content-Type", "application/json"}], mochijson2:encode(JSON)});
dispatch2('POST', Key, Req) ->
	error_logger:warning_msg("POST: ~p~n", [Key]),
	Body = Req:recv_body(),
	gen_server:call(testkv, {put, {Key, Body}}),
	JSON = [{key, Key}, {operation, post}, {status, ok}],
	Req:respond({200, [{"Content-Type", "application/json"}], mochijson2:encode(JSON)});

dispatch2('DELETE', Key, Req) ->
	error_logger:warning_msg("DELETE: ~p~n", [Key]),
	gen_server:call(testkv, {delete, Key}),
	JSON = [{key, Key}, {operation, delete}, {status, ok}],
	Req:respond({200, [{"Content-Type", "application/json"}], mochijson2:encode(JSON)});


dispatch2(Method, Key, Req) ->
	error_logger:warning_msg("UNKNOWN method: ~p~n", [Method]),
	Headers = [{"Allow", "GET,POST,PUT,DELETE"}],
	Req:respond({405, Headers, "405 Method Not Allowed\r\n"}).
