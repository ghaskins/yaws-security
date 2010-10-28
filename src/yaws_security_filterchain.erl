-module(yaws_security_filterchain).

-include_lib("eunit/include/eunit.hrl").

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-include_lib("yaws_security.hrl").

-export([out/1, next/2]).

% Standard YAWS callback.  Acts as the packet entry point to yaws_security
out(Arg) -> 
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    case yaws_security:resolve_handler(Path, []) of
	{ok, Chain, Handler, Options} ->
	    process(Arg, Chain, Handler, Options);
	{error, Error} ->
	    [{status, 404}]
    end.

process(Arg, Chain, Handler, Options) ->
    Pid = case yaws_security_context:start_link() of
	      {ok, Pid1} ->  Pid1;
	      Else -> throw({context_failure, Else})
	  end,

    Ctx = #context{pid = Pid, chain = Chain,
		   handler = Handler, options = Options},
    
    try next(Arg, Ctx)
    catch
	throw:unauthorized ->
	    [{status, 401}];
	throw:{login_failed, Error} ->
	    [{status, 403}];
	throw:forbidden ->
	    [{status, 403}]
    after
	yaws_security_context:stop(Pid)
    end.

process_roles(Arg, Ctx, [Role | T]) ->
    yaws_security_context:caller_in_role(Ctx, Role),
    process_roles(Arg, Ctx, T);
process_roles(Arg, Ctx, []) ->
    ok.

process_options(Arg, Ctx, [{caller_in_role, Roles} | T]) ->
    process_roles(Arg, Ctx, Roles);
process_options(Arg, Ctx, []) ->
    ok.

next(Arg, Ctx=#context{chain=[FilterFun | T]}) ->
    FilterFun(Arg, Ctx#context{chain=T});
next(Arg, Ctx=#context{chain=[]}) ->
    HandlerFun = Ctx#context.handler,
    
    process_options(Arg, Ctx, Ctx#context.options),

    HandlerFun(Arg, Ctx).

%----------------------------------------------------------------------

testfilter(Arg, Ctx, State) ->
    ?debugFmt("testfilter: ~p~n", [State]),
    next(Arg, Ctx).

safilter(Arg, Ctx) ->
    case yaws_security_context:token_get(Ctx) of
	{ok, _} ->
	    ok;
	null ->
	    Req = Arg#arg.req,
	    Headers = Arg#arg.headers,
	    case process_header(Headers#headers.other) of
		{ok, Principal} ->
		    Token = #token{type=simple, principal=Principal},
		    ok = yaws_security_context:token_set(Ctx, Token);
		_ -> ok
	    end
    end,
    next(Arg, Ctx).

process_header([{http_header, _Num, "Authentication", _, SaUser} | T]) ->
    {ok, SaUser};
process_header([{http_header, _, _, _, _} | T]) ->
    process_header(T);
process_header([]) ->
    not_found.

saprovider(Token) ->
    ?debugFmt("SIMPLEAUTH: Authenticating: ~p~n", [Token]),
    {ok, Token#token{authenticated=true}}.

testhandler(Arg, Ctx) ->
    ?debugFmt("testhandler: ~p~n", [Ctx]),
    [{status, 200}].

role_map("admin", Roles) ->
    sets:add_element(role_admin, Roles);
role_map(Principal, Roles) ->
    Roles.

userdetails(Token) ->
    ?debugFmt("Userdetails~p~n", [Token]),
    Roles = role_map(Token#token.principal, sets:from_list([role_user])),
    {ok, Token#token{granted_authorities = Roles}}.

rest_test() ->

    application:start(yaws),
    application:start(yaws_security),

    Handler = fun(Arg, Ctx) -> testhandler(Arg, Ctx) end,
    Provider = fun(Token) -> saprovider(Token) end,
    
    ok = yaws_security:register_filterchain(
	   resttest_base_chain,
	   [ {chain, http_session}, 
	     {chain, basic_auth},
	     {function, fun(Arg, Ctx) -> testfilter(Arg, Ctx, a) end}
	   ],
	   []),
    ok = yaws_security:register_filterchain(
	   resttest_chain,
	   [{chain, resttest_base_chain},
	    {function, fun(Arg, Ctx) -> testfilter(Arg, Ctx, b) end},
	    {function, fun(Arg, Ctx) -> safilter(Arg, Ctx) end}],
	   []),
    ok = yaws_security:register_realm("/", resttest_chain,
				      {function, Handler},
				      [{caller_in_role, [role_admin]}]),
    ok = yaws_security:register_provider([simple], Provider),

    ok = yaws_security_basicauth:register_provider(
	   [#basicauth_record{principal="admin",
			      password="admin"
			     },
	    #basicauth_record{principal="user",
			      password="user"
			     }
	   ]
	  ),

    ok = yaws_security:register_userdetails(fun(Token) -> userdetails(Token) end),

    % start an embedded yaws server
    GC = yaws_config:make_default_gconf(false, "test-server"),
    SC = #sconf{
      port = 8000,
      servername = "localhost",
      listen = {0, 0, 0, 0},
      docroot = "/tmp",
      appmods = [{"/", yaws_security_filterchain}]
    },
    case catch yaws_api:setconf(GC, [[SC]]) of
        ok -> {ok, started};
        Error ->
	    throw({yaws_error, Error})
    end,
    
    application:start(inets),
    Url = "http://localhost:8000",
    {ok, {{_, 401, _}, _, _}} =
	http:request(Url),

    {ok, {{_, 200, _}, _, _}} =
	http:request(get, {Url, [{"Authentication", "admin"}]}, [], []),
    
    {ok, {{_, 401, _}, _, _}} =
	http:request(get, {"http://admin:badpass@localhost:8000", []}, [], []),

    {ok, {{_, 403, _}, _, _}} =
	http:request(get, {"http://user:user@localhost:8000", []}, [], []),

    {ok, {{_, 200, _}, _, _}} =
	http:request(get, {"http://admin:admin@localhost:8000", []}, [], []),
    
    ok.
    




