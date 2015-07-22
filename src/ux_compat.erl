-module(ux_compat).

-export_type([compat_queue/0, compat_dict/0]).


-ifdef(otp_18).
-type compat_queue() :: queue:queue().
-type compat_dict()  :: dict:dict().
-else.
-type compat_queue() :: erlang:queue().
-type compat_dict()  :: erlang:dict().
-endif.