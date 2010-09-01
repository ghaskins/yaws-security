-module(yaws_security_filterchain).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([out/1]).

out(_Arg) ->
    [{status, 404}].

