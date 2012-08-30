-module(dinerl).
-author('Valentino Volonghi <valentino@adroll.com>').
-define(DINERL_DATA, dinerl_data).
-define(ARGS_KEY, args).

-include("dinerl_types.hrl").

-export([setup/3, setup/4, api/1, api/2, api/3]).

-export([create_table/4, create_table/5, delete_table/1, delete_table/2]).
-export([describe_table/1, describe_table/2, update_table/3, update_table/4]).
-export([list_tables/0, list_tables/1, list_tables/2, put_item/3, put_item/4]).
-export([delete_item/3, delete_item/4, get_item/3, get_item/4]).
-export([update_item/3, update_item/4]).

-export([update_data/4]).

-spec setup(access_key_id(), secret_access_key(), zone()) ->
                   {ok, clientarguments()}.
setup(AccessKeyId, SecretAccessKey, Zone) ->
    setup(AccessKeyId, SecretAccessKey, Zone, [{max_connections, 5000}]).

-spec setup(access_key_id(), secret_access_key(), zone(), options()) ->
                   {ok, clientarguments()} | {error, any()}.
setup(AccessKeyId, SecretAccessKey, Zone, Options) ->
    ets:new(?DINERL_DATA, [named_table, public]),
    case update_data(AccessKeyId, SecretAccessKey, Zone, Options) of
        {ok, ClientArgs} ->
            case timer:apply_interval(1000, ?MODULE, update_data, [AccessKeyId, SecretAccessKey, Zone, Options]) of
                {ok, _TRef} -> {ok, ClientArgs};
                Error -> Error
            end;
        Error -> Error
    end.


-spec api(method()) ->result().
api(Name) ->
    api(Name, {struct, []}).

-spec api(method(), any()) ->result().
api(Name, Body) ->
    api(Name, Body, undefined).

-spec api(method(), any(), integer()) ->result().
api(Name, Body, Timeout) ->
    case catch(ets:lookup_element(?DINERL_DATA, ?ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            {error, missing_credentials, ""};
        {ApiAccessKeyId, ApiSecretAccessKey, Zone, Options, ApiToken, Date, _} ->
            dinerl_client:api(ApiAccessKeyId, ApiSecretAccessKey, Zone,
                              ApiToken, Date, Name, Body, Timeout, Options)
    end.


-spec create_table(string()|binary(), keyschema(), integer(), integer()) -> jsonf().
create_table(Name, Key, ReadsPerSecond, WritesPerSecond) ->
    create_table(Name, Key, ReadsPerSecond, WritesPerSecond, undefined).

-spec create_table(string()|binary(), keyschema(), integer(), integer(), integer()) -> jsonf().
create_table(Name, Key, ReadsPerSecond, WritesPerSecond, Timeout) ->
    api(create_table,
        [{<<"TableName">>, Name},
         {<<"KeySchema">>, Key},
         {<<"ProvisionedThroughput">>, [{<<"ReadsPerSecond">>, ReadsPerSecond},
                                        {<<"WritesPerSecond">>, WritesPerSecond}]}], Timeout).

delete_table(Name) ->
    describe_table(Name, undefined).
delete_table(Name, Timeout) ->
    api(delete_table, [{<<"TableName">>, Name}], Timeout).


describe_table(Name) ->
    describe_table(Name, undefined).
describe_table(Name, Timeout) ->
    api(describe_table, [{<<"TableName">>, Name}], Timeout).



update_table(Name, ReadsPerSecond, WritesPerSecond) ->
    update_table(Name, ReadsPerSecond, WritesPerSecond, undefined).
update_table(Name, ReadsPerSecond, WritesPerSecond, Timeout) ->
    api(update_table, [{<<"TableName">>, Name},
                       {<<"ProvisionedThroughput">>, [{<<"ReadsPerSecond">>, ReadsPerSecond},
                                                      {<<"WritesPerSecond">>, WritesPerSecond}]}],
        Timeout).


list_tables() ->
    list_tables([]).

list_tables(List) ->
    list_tables(List, undefined).
list_tables(List, Timeout) ->
    list_tables(List, [], Timeout).

list_tables([], [], Timeout) ->
    list_tables([], {}, Timeout);
list_tables([], Body, Timeout) ->
    api(list_tables, Body, Timeout);
list_tables([{start_name, Name}|Rest], Acc, Timeout) ->
    list_tables(Rest, [{<<"ExclusiveStartTableName">>, Name}|Acc], Timeout);
list_tables([{limit, N}|Rest], Acc, Timeout) ->
    list_tables(Rest, [{<<"Limit">>, N}|Acc], Timeout).


put_item(Table, Attributes, Options) ->
    put_item(Table, Attributes, Options, undefined).
put_item(Table, Attributes, Options, Timeout) ->
    put_item(Table, Attributes, Options, [], Timeout).

put_item(Table, Attributes, [], PartialBody, Timeout) ->
    api(put_item, [{<<"TableName">>, Table}, {<<"Item">>, Attributes}|PartialBody], Timeout);
put_item(T, A, [{return, all_old}|Rest], Acc, Timeout) ->
    put_item(T, A, Rest, [{<<"ReturnValues">>, ?ALL_OLD}|Acc], Timeout);
put_item(T, A, [{return, none}|Rest], Acc, Timeout) ->
    put_item(T, A, Rest, [{<<"ReturnValues">>, ?NONE}|Acc], Timeout);
put_item(T, A, [{expected, V}|Rest], Acc, Timeout) ->
    put_item(T, A, Rest, [{<<"Expected">>, attr_updates(V, [])}|Acc], Timeout).




delete_item(Table, Key, Options) ->
    delete_item(Table, Key, Options, undefined).
delete_item(Table, Key, Options, Timeout) ->
    delete_item(Table, Key, Options, [], Timeout).

delete_item(Table, Key, [], PartialBody, Timeout) ->
    api(delete_item, [{<<"TableName">>, Table}, {<<"Key">>, Key}|PartialBody], Timeout);
delete_item(T, K, [{return, all_old}|Rest], Acc, Timeout) ->
    delete_item(T, K, Rest, [{<<"ReturnValues">>, ?ALL_OLD}|Acc], Timeout);
delete_item(T, K, [{return, none}|Rest], Acc, Timeout) ->
    delete_item(T, K, Rest, [{<<"ReturnValues">>, ?NONE}|Acc], Timeout);
delete_item(T, K, [{expected, V}|Rest], Acc, Timeout) ->
    delete_item(T, K, Rest, [{<<"Expected">>, attr_updates(V, [])}|Acc], Timeout).




get_item(Table, Key, Options) ->
    get_item(Table, Key, Options, undefined).
get_item(Table, Key, Options, Timeout) ->
    get_item(Table, Key, Options, [], Timeout).

get_item(T, K, [], Acc, Timeout) ->
    api(get_item, [{<<"TableName">>, T}, {<<"Key">>, K}|Acc], Timeout);
get_item(T, K, [{consistent, V}|Rest], Acc, Timeout) ->
    get_item(T, K, Rest, [{<<"ConsistentRead">>, V}|Acc], Timeout);
get_item(T, K, [{attrs, V}|Rest], Acc, Timeout) ->
    get_item(T, K, Rest, [{<<"AttributesToGet">>, V}|Acc], Timeout).




update_item(Table, Key, Options) ->
    update_item(Table, Key, Options, undefined).
update_item(Table, Key, Options, Timeout) ->
    update_item(Table, Key, Options, [], Timeout).

update_item(T, K, [], Acc, Timeout) ->
    api(update_item, [{<<"TableName">>, T}, {<<"Key">>, K}|Acc], Timeout);
update_item(T, K, [{update, AttributeUpdates}|Rest], Acc, Timeout) ->
    update_item(T, K, Rest, [{<<"AttributeUpdates">>, attr_updates(AttributeUpdates, [])}|Acc], Timeout);
update_item(T, K, [{expected, V}|Rest], Acc, Timeout) ->
    update_item(T, K, Rest, [{<<"Expected">>, attr_updates(V, [])}|Acc], Timeout);
update_item(T, K, [{return, none}|Rest], Acc, Timeout) ->
    update_item(T, K, Rest, [{<<"ReturnValues">>, ?NONE}|Acc], Timeout);
update_item(T, K, [{return, all_old}|Rest], Acc, Timeout) ->
    update_item(T, K, Rest, [{<<"ReturnValues">>, ?ALL_OLD}|Acc], Timeout);
update_item(T, K, [{return, updated_old}|Rest], Acc, Timeout) ->
    update_item(T, K, Rest, [{<<"ReturnValues">>, ?UPDATED_OLD}|Acc], Timeout);
update_item(T, K, [{return, all_new}|Rest], Acc, Timeout) ->
    update_item(T, K, Rest, [{<<"ReturnValues">>, ?ALL_NEW}|Acc], Timeout);
update_item(T, K, [{return, updated_new}|Rest], Acc, Timeout) ->
    update_item(T, K, Rest, [{<<"ReturnValues">>, ?UPDATED_NEW}|Acc], Timeout).




scan() ->
    pass.
q() ->
    pass.



%% Internal
%%
%% Every second it updates the Date part of the arguments
%% When within 120 seconds of the expiration of the token instead it refreshes also the token
-spec update_data(access_key_id(), secret_access_key(), zone(), options()) ->
                         {ok, clientarguments()}.
update_data(AccessKeyId, SecretAccessKey, Zone, Options) ->
    case catch(ets:lookup_element(?DINERL_DATA, ?ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            CurrentApiAccessKeyId = "123",
            CurrentApiSecretAccessKey = "123",
            Zone = Zone,
            Options = Options,
            CurrentApiToken = "123",
            CurrentExpirationSeconds = calendar:datetime_to_gregorian_seconds(erlang:universaltime());

        Result ->
            {CurrentApiAccessKeyId,
             CurrentApiSecretAccessKey,
             Zone,
             Options,
             CurrentApiToken,
             _Date,
             CurrentExpirationSeconds} = Result

    end,

    NewDate = httpd_util:rfc1123_date(),
    NowSeconds = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    SecondsToExpire = CurrentExpirationSeconds - NowSeconds,

    case SecondsToExpire < 120 of
        true ->
            NewToken = iam:get_session_token(AccessKeyId, SecretAccessKey),

            ExpirationString = proplists:get_value(expiration, NewToken),
            ApiAccessKeyId = proplists:get_value(access_key_id, NewToken),
            ApiSecretAccessKey = proplists:get_value(secret_access_key, NewToken),
            ApiToken = proplists:get_value(token, NewToken),
            ExpirationSeconds = calendar:datetime_to_gregorian_seconds(iso8601:parse(ExpirationString)),

            NewArgs = {ApiAccessKeyId, ApiSecretAccessKey, Zone, Options, ApiToken, NewDate, ExpirationSeconds};

        false ->
            NewArgs = {CurrentApiAccessKeyId, CurrentApiSecretAccessKey,
                       Zone, Options, CurrentApiToken, NewDate, CurrentExpirationSeconds}
    end,

    ets:insert(?DINERL_DATA, {?ARGS_KEY, NewArgs}),
    {ok, NewArgs}.


expected([], Acc) ->
    Acc;
expected([{Option, Value}|Rest], Acc) ->
    expected(Rest, [value_and_action({Option, Value})|Acc]).


attr_updates([], Acc) ->
    Acc;
attr_updates([{AttrName, Opts}|Rest], Acc) ->
    attr_updates(Rest, [{AttrName, expected(Opts, [])}|Acc]).



value_and_action({value, V}) ->
    {<<"Value">>, V};
value_and_action({action, put}) ->
    {<<"Action">>, <<"PUT">>};
value_and_action({action, add}) ->
    {<<"Action">>, <<"ADD">>};
value_and_action({action, delete}) ->
    {<<"Action">>, <<"DELETE">>};
value_and_action({exists, V}) ->
    {<<"Exists">>, V}.
