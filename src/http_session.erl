-module(http_session).

-include_lib("eunit/include/eunit.hrl").

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-include_lib("yaws_security.hrl").

-export([init/0]).

-record(session, {token}).

init() ->
    ok = yaws_security:register_filterchain(
	   http_session,
	   [{function, fun(Arg, Ctx) -> filter(Arg, Ctx) end}],
	   []
	  ).

filter(Arg, Ctx) ->
    Token = yaws_security_context:token_get(Ctx),
    Cookie = util:get_session("ysid", Arg),
    filter(Token, Cookie, Arg, Ctx).

filter({ok, _}, _, Arg, Ctx) ->
    yaws_security_filterchain:next(Arg, Ctx);

filter(null, {error, _}, Arg, Ctx) ->
    Ret = yaws_security_filterchain:next(Arg, Ctx), 
    case yaws_security_context:token_get(Ctx) of
	{ok, Token} ->
	    Session = #session{token = Token},
	    Cookie = yaws_api:new_cookie_session(Session),
	    lists:flatten(Ret, [yaws_api:setcookie("ysid", Cookie, "/")]);
	null ->
	    Ret
    end;

filter(null, {ok, Cookie, Session}, Arg, Ctx) ->
    ok = yaws_security_context:token_set(Ctx, Session#session.token),
    Ret = yaws_security_filterchain:next(Arg, Ctx), 
    case yaws_security_context:token_get(Ctx) of
	{ok, Token} ->
	    yaws_api:replace_cookie_session(Cookie,
					    Session#session{token = Token});
	null ->
	    yaws_api:delete_cookie_session(Cookie)
    end,
    Ret.




