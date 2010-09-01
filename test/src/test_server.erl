-module(test_server).
-behaviour(gen_server).

-include_lib("yaws/include/yaws.hrl").

-export([
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([Port]) ->

    GC = yaws_config:make_default_gconf(false, "test-server"),
    SC = #sconf{
      port = Port,
      servername = "localhost",
      listen = {0, 0, 0, 0},
      docroot = "/tmp",
      appmods = [{"/", security_filter}]
    },
    case catch yaws_api:setconf(GC, [[SC]]) of
        ok -> {ok, started};
        Error -> {stop, Error}
    end.

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
