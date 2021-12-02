-type access_key_id() :: string().
-type secret_access_key() :: string().
-type zone() :: string().
-type token() :: string().
-type rfcdate() :: string().
-type aws_datetime() :: string().
-type endpoint() :: string().
-type field() :: {binary(), binary() | string()}.
-type keyschema_element() :: {binary(), [field()]}.
-type key_element() :: {binary(), [field()]}.
-type keyschema() :: [keyschema_element()].
-type key() :: [key_element()].
-type jsonf() :: any().
-type clientarguments() :: {{access_key_id(), secret_access_key()}, zone(), rfcdate()}.
-type method() ::
    batch_get_item |
    get_item |
    put_item |
    delete_item |
    update_item |
    create_table |
    list_tables |
    describe_table |
    update_table |
    delete_table |
    q |
    scan |
    query_item_20111205 |
    query_item_20120810.
-type result() ::
    {ok, any()} |
    {error, string(), string()} |
    {error, term(), timeout | string()} |
    {error, atom(), any()}.
-type header() :: {string() | atom(), string()}.
-type headers() :: [header()].
-type attribute() :: {string(), {s | n, string()}} | {string(), {ns | ss, [string()]}}.
