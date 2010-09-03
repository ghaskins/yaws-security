% @private
-module(yaws_security_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    {ok, {{one_for_one, 1, 60},
          [{yaws_security, {yaws_security, start_link, []},
            permanent, brutal_kill, worker, [yaws_security]}
	  ]}}.
