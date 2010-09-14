-module(yaws_security_openid).

-include_lib("eunit/include/eunit.hrl").

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-include_lib("yaws_security.hrl").

-export([init/0, register_provider/0]).

-record(openid_token, {dict = eopenid_lib:new(), rawurl}).
-record(session, {orig_arg, dict = eopenid_lib:new()}).

-record(state, {default_authorities, records}).

init() ->
    ok = yaws_security:register_filterchain(
	   openid,
	   [{function, fun(Arg, Ctx) -> openid_filter(Arg, Ctx) end}],
	   []
	  ).

login(Arg) ->
    Session = #session{orig_arg = Arg},
    Cookie = yaws_api:new_cookie_session(Session),
    [login_form(Arg), yaws_api:setcookie("openid.login", Cookie)].

%% This function renders the default login page
login_form(Arg) ->
    {ehtml,
     [{h2, [], "OpenID Login:"},
      {form, [{method, get},
	      {action, "/openid/login"}],
       [
		 {img, [{src, "http://openid.net/images/login-bg.gif"}]},
	{input, [{name, openid.claimed_id},
		 {type, text},
		 {size, "48"}]},
	
	{input, [{type, submit},
		 {value, "Login"}]}
       ]
      }
     ]
    }.

openid_filter(Arg, Ctx) ->
    case yaws_security_context:token_get(Ctx) of
	{ok, _} ->
	    yaws_security_filterchain:next(Arg, Ctx);
	null ->
	    Req = Arg#arg.req,
	    Url = yaws_api:request_url(Arg),
	    openid_filter(Req#http_request.method,
			  string:tokens(Url#url.path, "/"), Arg, Ctx)
    end.

openid_filter(Cmd, ["openid", "login"], Arg, Ctx) ->
    {ok, ClaimedId} = yaws_api:queryvar(Arg, "openid.claimed_id"),
    {ok, Cookie, Session} = util:get_session("openid.login", Arg),

    Url = yaws_api:request_url(Arg),
    RawRoot = yaws_api:format_url(#url{scheme = Url#url.scheme,
				       host = Url#url.host,
				       port = Url#url.port}),
    Root = unicode:characters_to_list(RawRoot),
    Dict0 = eopenid_lib:foldf(
	      [eopenid_lib:in("openid.return_to", Root ++ "/openid/auth"),
	       eopenid_lib:in("openid.trust_root", Root)
	      ], eopenid_lib:new()),
    {ok,Dict1} = eopenid_v1:discover(ClaimedId, Dict0),
    {ok,Dict2} = eopenid_v1:associate(Dict1),
    yaws_api:replace_cookie_session(Cookie, Session#session{dict = Dict2}),

    {ok, Redirect} = eopenid_v1:checkid_setup(Dict2),
    [yaws_api:redirect(Redirect)];

openid_filter(Cmd, ["openid", "auth"], Arg, Ctx) ->
    {ok, Principal} = yaws_api:queryvar(Arg, "openid.identity"),
    {ok, Cookie, Session} = util:get_session("openid.login", Arg),

    RawUrl = unicode:characters_to_list(
	       yaws_api:format_url(yaws_api:request_url(Arg))),
    OpenIdToken = #openid_token{dict = Session#session.dict, rawurl=RawUrl},
    Token = #token{type=openid,
		   principal=Principal,
		   extra=OpenIdToken},
    ok = yaws_security_context:token_set(Ctx, Token),
    yaws_api:delete_cookie_session(Cookie),

    yaws_security_filterchain:next(Session#session.orig_arg, Ctx);

openid_filter(Cmd, Request, Arg, Ctx) -> % catchall
    try yaws_security_filterchain:next(Arg, Ctx)
    catch
	throw:unauthorized ->
	    login(Arg)
    end.

openid_authenticate(Token) ->
    OpenIdToken = Token#token.extra,
    case eopenid_v1:verify_signed_keys(OpenIdToken#openid_token.rawurl, OpenIdToken#openid_token.dict) of
	true ->
	    {ok,
	     Token#token {
	       authenticated=true,
	       granted_authorities=sets:from_list([role_user])
	      }};
	Error ->
	    {error, Error}
    end.

register_provider() ->
    yaws_security:register_provider(
      [openid],
      fun(Token) -> openid_authenticate(Token) end
     ).

