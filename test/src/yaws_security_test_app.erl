-module(yaws_security_test_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    test_server:start_link().

stop(_State) -> ok.
