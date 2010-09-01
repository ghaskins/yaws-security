{application, yaws_security_test,
 [{description, "YAWS security package tester"},
  {vsn, "0.1.0"},
  {modules, [__MODULES__]},
  {registered, []},
  {applications, [kernel, stdlib, yaws, yaws_security]},
  {mod, {yaws_security_test_app, []}}
 ]
}.	       
