-module(yaws_security).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-export([register_filterchain/2, register_realm/4, resolve_handler/2]).

-record(state, {filterchains, nextid, realms}).
-record(filterchain, {id, filters}).
-record(filter, {type, object}).
-record(functionfilter, {function}).

-record(realm, {path, chain, handler}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_Args) ->
    {ok, #state{filterchains = dict:new(), nextid = 0, realms = dict:new()}}.

register_filterchain(ChainSpec, Options) ->
    gen_server:call(?MODULE, {register_filterchain, ChainSpec, Options}).

register_realm(Path, ChainId, Handler, Options) ->
    gen_server:call(?MODULE, {register_realm, Path, ChainId, Handler, Options}).

resolve_handler(Path, Options) ->
    gen_server:call(?MODULE, {resolve_handler, Path}).

handle_call({register_filterchain, ChainSpec, []}, _From, State) ->

    ChainId = State#state.nextid,
    Filters = [#filter{type=function, object=#functionfilter{function=F}}
	       || {function, F} <- ChainSpec],
    Chain = #filterchain{id = ChainId, filters = Filters},
    UpdateChain = dict:store(ChainId, Chain, State#state.filterchains),

    NewState = State#state{filterchains = UpdateChain, nextid = ChainId+1},
    {reply, {ok, ChainId}, NewState};

handle_call({register_realm, Path, ChainId, {function, Handler}, []},
	    _From, State) ->
    case dict:find(Path, State#state.realms) of
	{ok, _} ->
	    {reply, {error, exists}, State};
	error ->
	    case dict:find(ChainId, State#state.filterchains) of
		{ok, _} ->
		    Realm = #realm{path = Path, chain = ChainId, handler = Handler},
		    Realms = dict:store(Path, Realm, State#state.realms),

		    {reply, ok, State#state{realms = Realms}};
		_ ->
		    {reply, {error, bad_chain_id}, State}
	    end
    end;

handle_call({register_realm, Path, ChainId, Handler, []}, _From, State) ->
    {reply, {error, bad_handler}, State};

handle_call({resolve_handler, Path}, _From, State) ->
    RealmsList = dict:to_list(State#state.realms),

    EvaluatedRealms = [eval_match(Path, X) || {_, X} <- RealmsList],
    find_best_chain(EvaluatedRealms, nomatch, State);

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

find_best_chain([nomatch | T], Best, State) ->
    find_best_chain(T, Best, State);
find_best_chain([Current | T], nomatch, State) ->
    find_best_chain(T, Current, State);
find_best_chain([Current | T], Best, State) ->
    {match, LHS, _} = Current,
    {match, RHS, _} = Best,
    if
	LHS >= RHS ->
	    find_best_chain(T, Current, State);
	true ->
	    find_best_chain(T, Best, State)
    end;
find_best_chain([], nomatch, State) ->
    {reply, {error, notfound}, State};
find_best_chain([], {match, _, Realm}, State) ->
    ChainId = Realm#realm.chain,
    {ok, FilterChain} = dict:find(ChainId, State#state.filterchains),
    Objects = [X#filter.object || X <- FilterChain#filterchain.filters],
    Functions = [{function, X#functionfilter.function} || X <- Objects],
    {reply, {ok, Functions, {function, Realm#realm.handler}}, State}.

eval_match(Path, Realm) ->
    case Index = string:str(Path, Realm#realm.path) of
	1 ->
	    {match, string:len(Realm#realm.path), Realm};
	Val ->
	    nomatch
    end.

%---------------------------------------------------------------------------
% test-harness

myfilter(Arg, Next) ->
    yaws_security_filter:next(Next).

first_handler(Arg) ->
    ok.

second_handler(Arg) ->
    ok.

filter_test() ->
    start_link(0),

    Handler1 = fun(Arg) -> first_handler(Arg) end,
    Handler2 = fun(Arg) -> second_handler(Arg) end,

    {ok, TestChain} = yaws_security:register_filterchain(
			[{function, fun(Arg, Next) -> myfilter(Arg, Next) end}], []),
    ok = yaws_security:register_realm("/good/path",
				      TestChain,
				      {function, Handler1},
				      []
				   ),
    ok = yaws_security:register_realm("/good/path/even/better",
				      TestChain,
				      {function, Handler2},
				      []
				     ),

    {error, exists} = yaws_security:register_realm("/good/path",
						   TestChain,
						   {function, Handler2},
						   []
						  ),
    {error, bad_chain_id} = yaws_security:register_realm("/bogus",
							 badid,
							 {function, Handler1},
							 []
							),
    {error, bad_handler} = yaws_security:register_realm("/bogus",
						       TestChain, bad_handler, []),

    {ok, Chain1, {function, Handler1}} = resolve_handler("/good/path/and/then/some", []),
    {ok, Chain2, {function, Handler2}} = resolve_handler("/good/path/even/better/foo", []),
    ?debugFmt("Chain1: ~p~n", [Chain1]),
    {error, notfound} = resolve_handler("/bad/path", []).
