-module(yaws_security_basicauth).

-include_lib("eunit/include/eunit.hrl").

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-include_lib("yaws_security.hrl").

-export([init/0, register_provider/1]).

-record(basicauth_token, {password}).

-record(state, {records}).

init() ->
    ok = yaws_security:register_filterchain(
	   basic_auth,
	   [{function, fun(Arg, Ctx) -> basicauth_filter(Arg, Ctx) end}],
	   []
	  ).

basicauth_filter(Arg, Ctx) ->
    case yaws_security_context:token_get(Ctx) of
	{ok, _} ->
	    ok;
	null ->
	    Req = Arg#arg.req,
	    Headers = Arg#arg.headers,
	    case Headers#headers.authorization of
		{Principal, Password, _} when is_list(Principal) ->
		    BaToken = #basicauth_token{password=Password},
		    Token = #token{type=basic,
				   principal=Principal,
				   extra=BaToken},
		    ok = yaws_security_context:token_set(Ctx, Token);
		Val ->
		    ?debugFmt("BASICAUTH: No AUTH header found: ~p~n", [Val]),
		    ok
	    end
    end,
    yaws_security_filterchain:next(Arg, Ctx).

basicauth_authenticate(Token, State)
  when is_record(State, state) ->
    BaToken = Token#token.extra,
    Password = BaToken#basicauth_token.password,

    case dict:find(Token#token.principal, State#state.records) of
	{ok, Record=#basicauth_record{password = Password}} ->
	    {ok, Token#token{authenticated=true}};
	{ok, _} ->
	    {error, bad_password};
	_ ->
	    {error, user_notfound}
    end.

register_provider(Records) ->
    State = #state{
      records = process_records(Records, dict:new())
     },

    yaws_security:register_provider(
      [basic],
      fun(Token) -> basicauth_authenticate(Token, State) end
     ).

process_records([Record | T], Dict) when is_record(Record, basicauth_record) ->
    NewDict = dict:store(Record#basicauth_record.principal, Record, Dict),
    process_records(T, NewDict);
process_records([], Dict) ->
    Dict.
