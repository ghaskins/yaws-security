-module(yaws_security).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").
-include_lib("yaws_security.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-export([register_filterchain/3, register_realm/4,
	 register_provider/2, resolve_handler/2, authenticate/1]).

-record(state, {filterchains, realms, providers}).
-record(filterchain, {id, filters}).

-record(realm, {path, chain, handler}).

% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @private
init(_Args) ->
    {ok, #state{filterchains = dict:new(),
		realms = dict:new(),
		providers = dict:new()
	       }}.

%-----------------------------------------------------------
% @doc registers a new filterchain
% @spec register_filterchain(Id::atom(), ChainSpec::chainspec(), Options::List) -> ok
% where
%   chainspec() = [filterspec()]
%   filterspec() = {function, fun()}
%   List = []						
%
% @end
%-----------------------------------------------------------
register_filterchain(Id, ChainSpec, Options) ->
    gen_server:call(?MODULE, {register_filterchain, Id, ChainSpec, Options}).

register_realm(Path, ChainId, Handler, Options) ->
    gen_server:call(?MODULE, {register_realm, Path, ChainId, Handler, Options}).

register_provider(Types, Provider) ->
    gen_server:call(?MODULE, {register_provider, Types, Provider}).

resolve_handler(Path, Options) ->
    gen_server:call(?MODULE, {resolve_handler, Path, Options}).

authenticate(Token) ->
    gen_server:call(?MODULE, {authenticate, Token}).

% @private
handle_call({register_filterchain, Id, ChainSpec, []}, _From, State) ->
    case dict:find(Id, State#state.filterchains) of
	{ok, _} ->
	    {reply, {error, exists}, State};
	error ->
	    try register_filterchain({Id, ChainSpec, State})
	    catch
		throw:{invalidspec, BadSpec} ->
		    {reply, {error, {invalid_filterspec, BadSpec}}, State}
	    end
    end;

% @private
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

% @private
handle_call({register_realm, Path, ChainId, Handler, []}, _From, State) ->
    {reply, {error, bad_handler}, State};

% @private
handle_call({resolve_handler, Path, []}, _From, State) ->
    RealmsList = dict:to_list(State#state.realms),

    EvaluatedRealms = [eval_match(Path, X) || {_, X} <- RealmsList],
    find_best_chain(EvaluatedRealms, nomatch, State);

% @private
handle_call({register_provider, Types, Provider}, _From, State) ->
    case check_existing_providers(Types, State#state.providers) of
	ok ->
	    install_provider(Types, Provider, State);
	conflict ->
	    {reply, {error, conflict}, State}
    end;

% @private
handle_call({authenticate, Token}, _From, State) ->
    Type = Token#token.type,
    case dict:find(Type, State#state.providers) of
	{ok, Provider} ->
	    case Provider(Token) of
		{ok, NewToken} ->
		    {reply, {ok, NewToken}, State};
		{error, Reason} ->
		    {reply, {error, Reason}, State}
	    end;
	_ ->
	    {reply, {error, unsupported_type}, State}
    end;

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

% @private
check_existing_providers([Type | T], Providers) ->
    case dict:find(Type, Providers) of
	{ok, _} ->
	    conflict;
	error ->
	    check_existing_providers(T, Providers)
    end;
check_existing_providers([], _Providers) ->
    ok.

register_filterchain({Id, ChainSpec, State}) ->
    Filters = [process_filterspec(F, State) || F <- ChainSpec],
    Chain = #filterchain{id = Id, filters = lists:flatten(Filters)},
    FilterChains = dict:store(Id, Chain, State#state.filterchains),

    {reply, ok, State#state{filterchains = FilterChains}}.

% @private
process_filterspec({function, F}, _State) ->
    F;
process_filterspec({chain, ChainId}, State) ->
    case dict:find(ChainId, State#state.filterchains) of
	{ok, Chain} ->
	    [F || F <- Chain#filterchain.filters];
	_ ->
	    throw({invalidspec, {chain, ChainId}})
    end;
process_filterspec(BadSpec, _State) ->
    throw({invalidspec, BadSpec}).

% @private
install_provider([], Provider, State) ->
    {reply, ok, State};
install_provider([Type | T], Provider, State) ->
    Providers = dict:store(Type, Provider, State#state.providers),
    install_provider(T, Provider, State#state{providers = Providers}).

% @private
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
    Functions = [X || X <- FilterChain#filterchain.filters],
    {reply, {ok, Functions, Realm#realm.handler}, State}.

% @private
eval_match(Path, Realm) ->
    case Index = string:str(Path, Realm#realm.path) of
	1 ->
	    {match, string:len(Realm#realm.path), Realm};
	Val ->
	    nomatch
    end.

%---------------------------------------------------------------------------
% test-harness

myfilter(Arg, Ctx) ->
    yaws_security_filterchain:next(Arg, Ctx).

first_handler(Arg, Ctx) ->
    [{status, 200}].

second_handler(Arg, Ctx) ->
    [{status, 404}].

filter_test() ->
    start_link(),

    Handler1 = fun(Arg, Ctx) -> first_handler(Arg, Ctx) end,
    Handler2 = fun(Arg, Ctx) -> second_handler(Arg, Ctx) end,

    % first, create a basic chain
    ok = yaws_security:register_filterchain(
	   mychain,
	   [{function, fun(Arg, Ctx) -> myfilter(Arg, Ctx) end}],
	   []
	  ),

    % now create another that builds on the first
    ok = yaws_security:register_filterchain(
	   mycompositechain,
	   [{chain, mychain},
	    {function, fun(Arg, Ctx) -> myfilter(Arg, Ctx) end}
	   ],
	   []
	  ),

    ok = yaws_security:register_realm("/good/path",
				      mychain,
				      {function, Handler1},
				      []
				   ),
    ok = yaws_security:register_realm("/good/path/even/better",
				      mychain,
				      {function, Handler2},
				      []
				     ),

    {error, exists}
	= yaws_security:register_realm(
	    "/good/path",
	    mychain,
	    {function, Handler2},
	    []
	   ),
    {error, bad_chain_id}
	= yaws_security:register_realm(
	    "/bogus",
	    badid,
	    {function, Handler1},
	    []
	   ),
    {error, bad_handler}
	= yaws_security:register_realm(
	    "/bogus",
	    mychain,
	    bad_handler,
	    []),
    
    {ok, Chain1, Handler1} = resolve_handler("/good/path/and/then/some", []),
    {ok, Chain1, Handler2} = resolve_handler("/good/path/even/better/foo", []),
    {error, notfound} = resolve_handler("/bad/path", []).

bad_filterspec_test() ->
    {error, {invalid_filterspec, garbage}}
	= yaws_security:register_filterchain(
	    badchain,
	    [garbage],
	    []
	   ),

    {error, {invalid_filterspec, {chain, nonexistant}}}
	= yaws_security:register_filterchain(
	    badchain,
	    [{chain, nonexistant}],
	    []
	   ).

%------------
% provider tests

testauth(Token) when is_record(Token, token) ->
    {ok, Token#token{authenticated=true}}.

providers_test() ->
    Provider = fun(Token) -> testauth(Token) end,
    ok = register_provider([foo], Provider),
    {error, conflict} = register_provider([foo, bar], Provider).

					     
    
