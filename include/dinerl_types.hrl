-type access_key_id() :: string().
-type secret_access_key() :: string().
-type zone() :: string().
-type token() :: string().
-type rfcdate() :: string().
-type endpoint() :: string().

-type clientarguments() :: {access_key_id(), secret_access_key(), zone(), token(), rfcdate(), integer()}.

-type method() :: batch_get_item | get_item | put_item | delete_item |
                  update_item | create_table | list_tables | describe_table |
                  update_table | delete_table | q | scan.

-type result() :: {ok, any()} | {error, string(), string()} | {error, term(), string()}.

-type header() :: {string() | atom(), string()}.
-type headers() :: [header()].
