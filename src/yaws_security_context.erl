-module(yaws_security_context).
-behavior(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include_lib("yaws_security.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-export([stop/1, token_set/2, token_get/1, clear/1, caller_in_role/2, principal/1]).

-record(state, {token}).

% @private
start_link() ->
    gen_server:start_link(?MODULE, [], []).

% @private
init(_Args) ->
    {ok, #state{token=null}}.

stop(Pid) ->
    gen_server:call(Pid, stop).

token_set(Ctx, Token) when is_record(Ctx, context), is_record(Token, token) ->
    token_set(Ctx#context.pid, Token);
token_set(Pid, Token) when is_pid(Pid), is_record(Token, token) ->
    gen_server:call(Pid, {set, Token}).

token_get(Ctx) when is_record(Ctx, context) ->
    token_get(Ctx#context.pid);
token_get(Pid) ->
    gen_server:call(Pid, get).

clear(Ctx) ->
    gen_server:call(Ctx#context.pid, clear).

caller_in_role(Ctx, Role) when is_record(Ctx, context); is_atom(Role) ->
    case gen_server:call(Ctx#context.pid, {caller_in_role, Role}) of
	yes -> ok;
	no  -> throw(forbidden);
	_   -> throw(unauthorized)
    end.

principal(Ctx) ->
    {ok, Token} = token_get(Ctx),
    Token#token.principal.

% @private
caller_in_role(Role, Token, State) when Token =:= null ->
    {reply, {error, notoken}, State};
caller_in_role(Role, Token=#token{authenticated=false}, State) ->
    case yaws_security:authenticate(Token) of
	{ok, NewToken=#token{authenticated=true}} ->
	    caller_in_role(Role, NewToken, State#state{token = NewToken});
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;
caller_in_role(Role, Token=#token{authenticated=true}, State) ->
    case sets:is_element(Role, Token#token.granted_authorities) of
	true ->
	    {reply, yes, State};
	false ->
	    {reply, no, State}
    end.

% @private
handle_call(clear, _From, State) ->
    {reply, ok, State#state{token = null}};

% @private
handle_call({caller_in_role, Role}, _From, State) ->
    caller_in_role(Role, State#state.token, State);

% @private
handle_call({set, Token}, _From, State) ->
    {reply, ok, State#state{token=Token}};

% @private
handle_call(get, _From, State) when State#state.token =/= null ->
    {reply, {ok, State#state.token}, State};
handle_call(get, _From, State) when State#state.token =:= null ->
    {reply, null, State};

% @private
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

% @private
handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

% @private
handle_cast(_Message, State) -> {noreply, State}.

% @private
handle_info(_Info, State) -> {noreply, State}.

% @private
terminate(_Reason, _State) ->
    ok.

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.
