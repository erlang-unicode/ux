{application, ux,
 [{description, "ux"},
  {vsn, "0.01"},
  {modules, [
    ux,
    ux_app,
    ux_sup,
    ux_web,
    ux_deps
  ]},
  {registered, []},
  {mod, {ux_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
