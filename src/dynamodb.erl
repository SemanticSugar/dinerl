-module(dynamodb).

-author('Valentino Volonghi <valentino@adroll.com>').

-include("dinerl_types.hrl").

-export([call/5, call/6]).

%% @todo Remove when lhttpc's specs are fixed
-dialyzer({nowarn_function, [{call, 6}, {submit, 4}]}).

-spec endpoint(zone()) -> endpoint().
endpoint("us-east-1" ++ _R) ->
    "dynamodb.us-east-1.amazonaws.com";
endpoint("us-west-1" ++ _R) ->
    "dynamodb.us-west-1.amazonaws.com";
endpoint("us-west-2" ++ _R) ->
    "dynamodb.us-west-2.amazonaws.com";
endpoint("ap-northeast-1" ++ _R) ->
    "dynamodb.ap-northeast-1.amazonaws.com";
endpoint("ap-southeast-1" ++ _R) ->
    "dynamodb.ap-southeast-1.amazonaws.com";
endpoint("eu-west-1" ++ _R) ->
    "dynamodb.eu-west-1.amazonaws.com".

-spec region(zone()) -> string().
region("us-east-1" ++ _R) ->
    "us-east-1";
region("us-west-1" ++ _R) ->
    "us-west-1";
region("us-west-2" ++ _R) ->
    "us-west-2";
region("ap-northeast-1" ++ _R) ->
    "ap-northeast-1";
region("ap-southeast-1" ++ _R) ->
    "ap-southeast-1";
region("eu-west-1" ++ _R) ->
    "eu-west-1".

-spec call(awsv4:credentials(),
           zone(),
           string(),
           aws_datetime(),
           iolist(),
           undefined | pos_integer()) ->
              result().
call(Credentials, Zone, Target, ISODate, Body, undefined) ->
    call(Credentials, Zone, Target, ISODate, Body, 1000);
call(Credentials, Zone, Target, ISODate, Body, Timeout) ->
    Host = endpoint(Zone),
    Headers =
        awsv4:headers(Credentials,
                      #{service => "dynamodb",
                        target_api => Target,
                        method => "POST",
                        aws_date => ISODate,
                        host => Host,
                        region => region(Zone)},
                      Body),
    submit("http://" ++ Host ++ "/",
           [{"content-type", "application/x-amz-json-1.0"} | Headers],
           Body,
           Timeout).

call(Credentials, Zone, Target, RFCDate, Body) ->
    call(Credentials, Zone, Target, RFCDate, Body, 1000).

-spec submit(endpoint(), headers(), iolist(), integer()) -> result().
submit(Endpoint, Headers, Body, Timeout) ->
    %io:format("Request:~nHeaders:~p~nBody:~n~p~n~n", [Headers, iolist_to_binary(Body)]),
    case lhttpc:request(Endpoint, "POST", Headers, Body, Timeout, [{max_connections, 10000}])
        of
        {ok, {{200, _}, _Headers, Response}} ->
            %io:format("Response: ~p~n", [Response]),
            {ok, Response};
        {ok, {{400, Code}, _Headers, ErrorString}} ->
            {error, Code, ErrorString};
        {ok, {{413, Code}, _Headers, ErrorString}} ->
            {error, Code, ErrorString};
        {ok, {{500, Code}, _Headers, ErrorString}} ->
            {error, Code, ErrorString};
        {error, Reason} ->
            {error, unknown, Reason};
        Other ->
            {error, response, Other}
    end.
