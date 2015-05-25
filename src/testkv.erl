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

-module(testkv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    error_logger:warning_msg("KV processor: started.~n", []),
    {ok, []}.

handle_call({rawdata, Data}, From, State) ->
    spawn(fun() -> parse_raw_request(Data, From) end),
    {noreply, State};

handle_call({put, {Key, Value}}, _From, State) ->
    ets:insert(storage, {Key, Value}),
    {reply, ok, State};

handle_call({get, Key}, _From, State) ->
    error_logger:warning_msg("KV processor: getting ~p~n", [Key]),
    Ret = case  ets:lookup(storage, Key) of
		    [] -> {error, notfound};
		    [{Key, Value} | _ ] -> {ok, Value}
	    end,
    error_logger:warning_msg("KV processor: got ~p~n", [Ret]),
    {reply, Ret, State};

handle_call({delete, Key}, _From, State) ->
    ets:delete(storage, Key),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    error_logger:warning_msg("KV processor: stopped [~w]~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

parse_raw_request(Data, From) ->
    case (catch mochijson2:decode(Data)) of
	    {struct, JsonData} ->
		    parse_json_request(JsonData, From);
	    _ ->
		    gen_server:reply(From, <<"{\"error\":\"nojson\"}">>)
    end.

parse_json_request(JsonData, From) ->
	case proplists:get_value(<<"operation">>, JsonData) of
		<<"get">> -> handle_get(JsonData, From);
		<<"put">> -> handle_put(JsonData, From);
		% We're handling PUT and POST equally
		<<"post">> -> handle_put(JsonData, From);
		<<"delete">> -> handle_delete(JsonData, From);
		_ -> handle_unknown(JsonData, From)
	end.

handle_get(JsonData, From) ->
	case proplists:get_value(<<"key">>, JsonData) of
		undefined ->
			JSON = [{operation, get}, {status, [{error,no_key}]}],
			gen_server:reply(From, mochijson2:encode(JSON));
		Key ->
			JSON = case gen_server:call(testkv, {get, Key}) of
				{error, notfound} -> [{key, Key}, {operation, get}, {status, [{error,notfound}]}];
				{ok, Val} -> [{key, Key}, {operation, get}, {status, ok}, {val, Val}]
			end,
			gen_server:reply(From, mochijson2:encode(JSON))
	end.

handle_put(JsonData, From) ->
	case proplists:get_value(<<"key">>, JsonData) of
		undefined ->
			JSON = [{operation, get}, {status, [{error, no_key}]}],
			gen_server:reply(From, mochijson2:encode(JSON));
		Key ->
			case proplists:get_value(<<"val">>, JsonData) of
				undefined ->
					JSON = [{key, Key}, {operation, get}, {status, [{error, no_val}]}],
					gen_server:reply(From, mochijson2:encode(JSON));
				Val ->
					gen_server:call(testkv, {put, {Key, Val}}),
					JSON = [{key, Key}, {operation, put}, {status, ok}],
					gen_server:reply(From, mochijson2:encode(JSON))
			end
	end.

handle_delete(JsonData, From) ->
	case proplists:get_value(<<"key">>, JsonData) of
		undefined ->
			JSON = [{operation, delete}, {status, [{error,no_key}]}],
			gen_server:reply(From, mochijson2:encode(JSON));
		Key ->
			gen_server:call(testkv, {delete, Key}),
			JSON = [{key, Key}, {operation, delete}, {status, ok}],
			gen_server:reply(From, mochijson2:encode(JSON))
	end.

handle_unknown(_, From) ->
	JSON = [{status, [{error, unknown_command}]}],
	gen_server:reply(From, mochijson2:encode(JSON)).
