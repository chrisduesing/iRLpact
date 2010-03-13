{application, irlpact,
 [{description, "Library used to connect to ICE's iMpact data feed."},
  {vsn, "0.1"},
  {modules, [
    irlpact,
    irlpact_app,
    irlpact_client,
    irlpact_connector,
    irlpact_message,
    irlpact_sup,
    irlpact_util
  ]},
  {registered, [irlpact_connector, irlpact_client]},
  {mod, {irlpact_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
