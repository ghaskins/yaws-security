-module(yaws_security_util).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([get_session/2]).

get_session(Name, Arg) ->
    H = Arg#arg.headers,
    case yaws_api:find_cookie_val(Name, H#headers.cookie) of
        Cookie when Cookie /= [] ->
            case yaws_api:cookieval_to_opaque(Cookie) of
                {ok, Session} ->
                    {ok, Cookie, Session};
                {error, {has_session, Session}} ->
                    {ok, Cookie, Session};
                Else ->
                    Else
            end;
        [] ->
            {error, nocookie}
    end.
