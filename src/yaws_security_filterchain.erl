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
    [{status, 400}].

%----------------------------------------------------------------------

testfilter(Arg, Ctx) ->
    %next(Arg, Ctx).
    ok.

testhandler(Arg, Ctx) ->
    ok.

rest_test() ->

    Handler = fun(Arg, Ctx) -> testhandler(Arg, Ctx) end,
    
    {ok, Chain} = yaws_security:register_filterchain(
			[{function, fun(Arg, Ctx) -> testfilter(Arg, Ctx) end}],
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
    {ok, {Proto, Headers, Body}} =
	http:request(get, {Url, []}, [], []),
    
    ok.
    




