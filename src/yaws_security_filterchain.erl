-module(yaws_security_filterchain).

-include_lib("eunit/include/eunit.hrl").

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-include_lib("yaws_security.hrl").

-export([out/1]).

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
    next(Arg, Ctx).

next(Arg, Ctx=#context{chain=[{function, FilterFun} | T]}) ->
    FilterFun(Arg, Ctx#context{chain=T});
next(Arg, Ctx=#context{chain=[], handler={function, HandlerFun}}) ->
    HandlerFun(Arg, Ctx).

%----------------------------------------------------------------------

testfilter(Arg, Ctx, State) ->
    ?debugFmt("testfilter: Ctx: ~p State: ~p~n", [Ctx, State]),
    next(Arg, Ctx).

testhandler(Arg, Ctx) ->
    ?debugFmt("testhandler: ~p~n", [Ctx]),
    [{status, 200}].

rest_test() ->

    Handler = fun(Arg, Ctx) -> testhandler(Arg, Ctx) end,
    
    {ok, Chain} = yaws_security:register_filterchain(
		    [{function, fun(Arg, Ctx) -> testfilter(Arg, Ctx, a) end},
		     {function, fun(Arg, Ctx) -> testfilter(Arg, Ctx, b) end}],
		    []),
    ok = yaws_security:register_realm("/", Chain, {function, Handler}, []),

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
    {ok, {{HttpVer, 200, Msg}, Headers, Body}} =
	http:request(get, {Url, []}, [], []),
    
    ok.
    




