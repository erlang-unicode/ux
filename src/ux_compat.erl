-module(ux_compat).

-export_type([queue/0, dict/0]).


-ifdef(otp_18).
-type queue() :: queue:queue().
-type dict()  :: dict:dict().
-else.
-type queue() :: erlang:queue().
-type dict()  :: erlang:dict().
-endif.