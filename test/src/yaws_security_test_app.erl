-module(yaws_security_test_app).
-behavior(application).
-export([start/2, stop/1]).

-include_lib("yaws/include/yaws.hrl").

start(_Type, _StartArgs) ->
    yaws_security_openid:init(),
    yaws_security_openid:register_provider(),

    ok = yaws_security:register_realm("/", openid,
				      {function,
				       fun(Arg, Ctx) -> testhandler(Arg, Ctx) end},
				      []),

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
        Error -> {stop, Error}
    end,
    
    test_server:start_link().

testhandler(Arg, Ctx) ->
    yaws_security_context:caller_in_role(Ctx, role_user),
    [{status, 200}, {html, "Hello, World"}].

stop(_State) -> ok.
