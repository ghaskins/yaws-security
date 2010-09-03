
-record(context, {pid, chain, handler}).
-record(token, {type, principal, granted_authorities=sets:new(), authenticated=false, extra}).

-record(basicauth_record, {principal, password, granted_authorities = []}).
