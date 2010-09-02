-module(yaws_security_context).
-behavior(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include_lib("yaws_security.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-export([stop/1]).

-record(state, {token}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

stop(Pid) ->
    gen_server:call(Pid, stop).

set(Pid, Token) when is_record(Token, token) ->
    gen_server:call(Pid, {set, Token}).

handle_call({set, Token}, _From, State) ->
    {reply, ok, State#state{token=Token}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
