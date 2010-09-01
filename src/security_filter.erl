-module(security_filter).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([out/1]).

out(Arg) ->
    [{status, 404}].

