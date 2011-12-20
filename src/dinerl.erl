-module(dinerl).
-author('Valentino Volonghi <valentino@adroll.com>').

-define(DINERL_DATA, dinerl_data).

-export([setup/2, update_date/0, update_token/2]).

%% bench() ->
%%     dru:benchcall(fun() -> dynamodb:call("DFDSAFFDSAFASD", "adfso90w2kjrojewqjkdkqdqkp", "DynamoDBv20110629", "adfsoij09ur21ij0weqjoiwdkmnacsmncioj-0i-023-02310i1489uifhwjkfqjkdskjdslkjdaljkdqkljqioewqowiefiojfqwe", ets:lookup_element(date, date, 2), "adsfadsfadfsfsadsadfadfs") end, 1000000).

setup(AccessKeyId, SecretAccessKey) ->
    ets:new(?DINERL_DATA, [named_table, public]),
    timer:apply_interval(1000, dinerl, update_date, []),
    update_token(AccessKeyId, SecretAccessKey).

update_date() ->
    ets:insert(?DINERL_DATA, {date, httpd_util:rfc1123_date()}).

update_token(AccessKeyId, SecretAccessKey) ->
    NewToken = iam:get_session_token(AccessKeyId, SecretAccessKey),
    ets:insert(?DINERL_DATA, {token, NewToken}),
    ExpirationString = proplists:get_value(expiration, NewToken),
    NowSeconds = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    ExpirationSeconds = calendar:datetime_to_gregorian_seconds(iso8601:parse(ExpirationString)),
    Seconds = ExpirationSeconds-NowSeconds, % Assumption is that expiration is in the future.
    timer:apply_after((Seconds-120)*1000, dinerl, update_token, [AccessKeyId, SecretAccessKey]).
    
