-module(dinerl).
-author('Valentino Volonghi <valentino@adroll.com>').
-define(DINERL_DATA, dinerl_data).
-define(ARGS_KEY, args).

-export([setup/3, update_data/3, api/2, api/3]).
 
%% bench() ->
%%     dru:benchcall(fun() -> dynamodb:call("DFDSAFFDSAFASD", "adfso90w2kjrojewqjkdkqdqkp", "DynamoDBv20110629", "adfsoij09ur21ij0weqjoiwdkmnacsmncioj-0i-023-02310i1489uifhwjkfqjkdskjdslkjdaljkdqkljqioewqowiefiojfqwe", ets:lookup_element(date, date, 2), "adsfadsfadfsfsadsadfadfs") end, 1000000).
 
setup(AccessKeyId, SecretAccessKey, Zone) ->
    ets:new(?DINERL_DATA, [named_table, public]),
    update_data(AccessKeyId, SecretAccessKey, Zone),
    timer:apply_interval(1000, ?MODULE, update_data, [AccessKeyId, SecretAccessKey, Zone]).

update_data(AccessKeyId, SecretAccessKey, Zone) ->
    case catch(ets:lookup_element(?DINERL_DATA, ?ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            CurrentApiAccessKeyId = "123",
            CurrentApiSecretAccessKey = "123",
            Zone = Zone,
            CurrentApiToken = "123",
            CurrentExpirationSeconds = calendar:datetime_to_gregorian_seconds(erlang:universaltime());

        Result ->
            [CurrentApiAccessKeyId,
             CurrentApiSecretAccessKey,
             Zone,
             CurrentApiToken,
             _Date,
             CurrentExpirationSeconds] = Result

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
            
            NewArgs = [ApiAccessKeyId, ApiSecretAccessKey, Zone, ApiToken, NewDate, ExpirationSeconds];

        false ->
            NewArgs = [CurrentApiAccessKeyId, CurrentApiSecretAccessKey, Zone, CurrentApiToken, NewDate, CurrentExpirationSeconds]
    end,
    
    ets:insert(?DINERL_DATA, {?ARGS_KEY, NewArgs}).

api(Name, Body) ->
    api(Name, Body, undefined).

api(Name, Body, Timeout) ->
    case catch(ets:lookup_element(?DINERL_DATA, ?ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            {error, missing_credentials};
        [ApiAccessKeyId, ApiSecretAccessKey, Zone, ApiToken, Date, _] ->
            dinerl_client:api(ApiAccessKeyId, ApiSecretAccessKey, Zone, ApiToken, Date, Name, Body, Timeout)
    end.



%%     NowSeconds = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
%%     Seconds = ExpirationSeconds-NowSeconds, % Assumption is that expiration is in the future.
    


%%     timer:apply_after((Seconds-120)*1000, , update_token, [AccessKeyId, SecretAccessKey, Zone]).




%% update_data() ->


%%     ets:insert(?DINERL_DATA, {date, httpd_util:rfc1123_date()}).

%% update_token(AccessKeyId, SecretAccessKey) ->
%%     NewToken = iam:get_session_token(AccessKeyId, SecretAccessKey),
%%     ets:insert(?DINERL_DATA, {token, NewToken}),
%%     ExpirationString = proplists:get_value(expiration, NewToken),
%%     NowSeconds = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
%%     ExpirationSeconds = calendar:datetime_to_gregorian_seconds(iso8601:parse(ExpirationString)),
%%     Seconds = ExpirationSeconds-NowSeconds, % Assumption is that expiration is in the future.
%%     timer:apply_after((Seconds-120)*1000, dinerl, update_token, [AccessKeyId, SecretAccessKey]).
