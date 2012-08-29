-type access_key_id() :: string().
-type secret_access_key() :: string().
-type zone() :: string().
-type options() :: list().
-type token() :: string().
-type rfcdate() :: string().
-type endpoint() :: string().

-type field() :: {binary(), binary()|string()}.
-type keyschema_element() :: {binary(), [field()]}.

-type key_element() :: {binary(), [field()]}.

-type keyschema() :: [keyschema_element()].
-type key() :: [key_element()].

-type jsonf() :: any().

-type clientarguments() :: {access_key_id(), secret_access_key(), zone(), options(), token(), rfcdate(), integer()}.

-type method() :: batch_get_item | get_item | put_item | delete_item |
                  update_item | create_table | list_tables | describe_table |
                  update_table | delete_table | q | scan.

-type result() :: {ok, any()} | {error, string(), string()} | {error, term(), string()}.

-type header() :: {string() | atom(), string()}.
-type headers() :: [header()].

-type attribute() :: {string(), {s|n, string()}} | {string(), {ns|ss, [string()]}}.


-define(HASHKEYSCHEMA(N, T), {<<"HashKeyElement">>, [{<<"AttributeName">>, N}, {<<"AttributeType">>, T}]}).
-define(RANGEKEYSCHEMA(N, T), {<<"RangeKeyElement">>, [{<<"AttributeName">>, N}, {<<"AttributeType">>, T}]}).

-define(HASHKEY(N, V), {<<"HashKeyElement">>, [V]}).
-define(RANGEKEY(N, V), {<<"RangeKeyElement">>, [V]}).


-define(NONE, <<"NONE">>).
-define(ALL_OLD, <<"ALL_OLD">>).
-define(UPDATED_OLD, <<"UPDATED_OLD">>).
-define(ALL_NEW, <<"ALL_NEW">>).
-define(UPDATED_NEW, <<"UPDATED_NEW">>).


%% Attributes in Responses: {"AttributeName4":{"AttributeType":"AttributeValue"}}
%% Expected Field: {"AttributeName3":{"Value":{"AttributeType":"AttributeValue3"}, "Exists": "true"}}
%% Update Field:   {"AttributeName3":{"Value":{"AttributeType":"AttributeValue3_New"},"Action":"PUT"}}
%% Put Item Field: {"AttributeName1":{"AttributeType1": "AttributeValue1"}}
