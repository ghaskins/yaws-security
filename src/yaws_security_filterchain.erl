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
	{ok, Chain, Handler} ->
	    process(Arg, Chain, Handler);
	{error, Error} ->
	    [{status, 404}]
    end.

process(Arg, Chain, Handler) ->
    {ok, Pid} = yaws_security_context:start_link(),

    Ctx = #context{pid = Pid, chain = Chain, handler = Handler},
    
    try next(Arg, Ctx) of
	Ret ->
	    Ret
    catch
	throw:unauthorized ->
	    [{status, 401}]
    after
	yaws_security_context:stop(Pid)
    end.

next(Arg, Ctx=#context{chain=[{function, FilterFun} | T]}) ->
    FilterFun(Arg, Ctx#context{chain=T});
next(Arg, Ctx=#context{chain=[], handler={function, HandlerFun}}) ->
    HandlerFun(Arg, Ctx).

%----------------------------------------------------------------------

testfilter(Arg, Ctx, State) ->
    ?debugFmt("testfilter: Ctx: ~p State: ~p~n", [Ctx, State]),
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
    GrantedAuthorities = [role_user],
    ?debugFmt("Authenticating: ~p~n", [Token]),
    {ok, Token#token{
	   authenticated=true,
	   granted_authorities=sets:from_list(GrantedAuthorities)
	  }
    }.

testhandler(Arg, Ctx) ->
    yaws_security_context:caller_in_role(Ctx, role_user),
    ?debugFmt("testhandler: ~p~n", [Ctx]),
    [{status, 200}].

rest_test() ->

    Handler = fun(Arg, Ctx) -> testhandler(Arg, Ctx) end,
    Provider = fun(Token) -> saprovider(Token) end,
    
    {ok, Chain} = yaws_security:register_filterchain(
		    [{function, fun(Arg, Ctx) -> testfilter(Arg, Ctx, a) end},
		     {function, fun(Arg, Ctx) -> testfilter(Arg, Ctx, b) end},
		     {function, fun(Arg, Ctx) -> safilter(Arg, Ctx) end}],
		    []),
    ok = yaws_security:register_realm("/", Chain, {function, Handler}, []),
    ok = yaws_security:register_provider([simple], Provider),

    application:start(yaws),

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
    
    ok.
    




