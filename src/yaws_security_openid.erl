-module(yaws_security_openid).

-include_lib("eunit/include/eunit.hrl").

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-include_lib("yaws_security.hrl").

-export([init/0, create_filter/2, register_provider/0]).

-record(openid_token, {dict = eopenid_lib:new(), rawurl}).
-record(session, {orig_url, dict = eopenid_lib:new()}).

-record(state, {default_authorities, records}).

-record(filteroptions, {login_redirect = "/openid/login"}).

init() ->
    create_filter(openid, []).

parse_options([{login_redirect, Redirect} | T], Options) ->
    parse_options(T, Options#filteroptions{login_redirect = Redirect});
parse_options([Option | T], Options) ->
    throw({invalid_option, Option});
parse_options([], Options) ->
    Options.

create_filter(Name, RawOptions) ->
    Options = parse_options(RawOptions, #filteroptions{}),
    ok = yaws_security:register_filterchain(
	   Name,
	   [{function, fun(Arg, Ctx) -> openid_filter(Arg, Ctx, Options) end}],
	   []
	  ).

%% This function renders the default login page
login_form(Arg) ->
    {ehtml,
     [{h2, [], "OpenID Login:"},
      {form, [{method, get},
	      {action, "/openid/submit"}],
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

openid_filter(Arg, Ctx, Options) ->
    Req = Arg#arg.req,
    Url = yaws_api:request_url(Arg),
    Token = yaws_security_context:token_get(Ctx),

    openid_filter(Token, Req#http_request.method,
		  string:tokens(Url#url.path, "/"), Arg, Ctx, Options).

openid_filter(_, 'GET', ["openid", "login"], Arg, Ctx, _) ->
    [login_form(Arg)];

openid_filter({ok, _}, _, _, Arg, Ctx, _) ->
    yaws_security_filterchain:next(Arg, Ctx);    

openid_filter(null, 'GET', ["openid", "submit"], Arg, Ctx, _) ->
    {ok, ClaimedId} = yaws_api:queryvar(Arg, "openid.claimed_id"),
    {Mode, Cookie, Session} = 
	case yaws_security_util:get_session("openid.login", Arg) of 
	    {ok, C, S} -> {found, C, S};
	    _ ->
		% we can get here if the user visited the login URL
		% without being directed there by an access
		% exception.  We do not know what URL they may
		% actually want, so just assume "/" 
		S = #session{orig_url = "/"},
		C = yaws_api:new_cookie_session(S),
		{created, C, S}
	end,
	
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

    case Mode of
	found -> [yaws_api:redirect(Redirect)]; 
	created -> [yaws_api:redirect(Redirect),
		    yaws_api:setcookie("openid.login", Cookie)]
    end;

openid_filter(null, Cmd, ["openid", "auth"], Arg, Ctx, _) ->
    {ok, Principal} = yaws_api:queryvar(Arg, "openid.identity"),
    {ok, Cookie, Session} = yaws_security_util:get_session("openid.login", Arg),

    RawUrl = unicode:characters_to_list(
	       yaws_api:format_url(yaws_api:request_url(Arg))),
    OpenIdToken = #openid_token{dict = Session#session.dict, rawurl=RawUrl},
    Token = #token{type=openid,
		   principal=Principal,
		   extra=OpenIdToken},
    ok = yaws_security_context:token_set(Ctx, Token),
    yaws_api:delete_cookie_session(Cookie),

    yaws_api:redirect(Session#session.orig_url);

openid_filter(null, 'GET', _Request, Arg, Ctx, Options) ->
    try yaws_security_filterchain:next(Arg, Ctx)
    catch
	throw:unauthorized ->
	    Url = yaws_api:request_url(Arg),
	    Session = #session{orig_url = Url#url.path},
	    Cookie = yaws_api:new_cookie_session(Session),
	    [yaws_api:redirect(Options#filteroptions.login_redirect),
	     yaws_api:setcookie("openid.login", Cookie)]
    end;

openid_filter(null, _Cmd, _Request, Arg, Ctx, Options) -> % catchall
    yaws_security_filterchain:next(Arg, Ctx).

openid_authenticate(Token) ->
    OpenIdToken = Token#token.extra,
    case eopenid_v1:verify_signed_keys(OpenIdToken#openid_token.rawurl,
				       OpenIdToken#openid_token.dict) of
	true ->
	    {ok, Token#token{authenticated=true}};
	Error ->
	    {error, Error}
    end.

register_provider() ->
    yaws_security:register_provider(
      [openid],
      fun(Token) -> openid_authenticate(Token) end
     ).

invalidoption_test() ->

    try create_filter(bad_filter, [foo]) of
	_ ->
	    throw(unexpected_success)
    catch
	throw:{invalid_option, foo} ->
	    ok
    end.
