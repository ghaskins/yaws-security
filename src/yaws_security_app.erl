% @private
-module(yaws_security_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    Ret = yaws_security:start_link(),
    
    yaws_security_basicauth:init(),
    yaws_security_openid:init(),
    http_session:init(),
   
    Ret.

stop(_State) -> ok.
