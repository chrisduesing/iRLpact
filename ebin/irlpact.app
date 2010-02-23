{application, irlpact,
 [{description, "Library used to connect to ICE's iMpact data feed."},
  {vsn, "0.1"},
  {modules, [
    irlpact_app,
    irlpact_sup,
    irlpact_connector
  ]},
  {registered, [irlpact_connector]},
  {mod, {irlpact_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
