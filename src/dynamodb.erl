-module(dynamodb).

-type aws_datetime() :: string().
-type endpoint() :: string().
-type header() :: {string() | atom(), string()}.
-type headers() :: [header()].

-export_type([aws_datetime/0]).

-export([call/5, call/6]).

%% @todo Remove when lhttpc's specs are fixed
-dialyzer({nowarn_function, [call/6, submit/4]}).

-spec endpoint(dinerl:zone()) -> endpoint().
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

-spec region(dinerl:zone()) -> string().
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
           dinerl:zone(),
           string(),
           aws_datetime(),
           iodata(),
           undefined | pos_integer()) ->
              dinerl:result().
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
    submit(Host, [{"content-type", "application/x-amz-json-1.0"} | Headers], Body, Timeout).

call(Credentials, Zone, Target, RFCDate, Body) ->
    call(Credentials, Zone, Target, RFCDate, Body, 1000).

-spec submit(endpoint(), headers(), iodata(), integer()) -> dinerl:result().
submit(Host, Headers, Body, Timeout) when is_list(Host) ->
    MaxConnections = dinerl_util:get_env(max_connections),
    Opts = [{max_connections, MaxConnections}],
    Endpoint = "http://" ++ Host ++ "/",
    dinerl_util:increment([dinerl, dynamodb, call, {endpoint, list_to_atom(Host)}]),
    F = fun() -> lhttpc:request(Endpoint, "POST", Headers, Body, Timeout, Opts) end,
    case dinerl_util:time_call([dinerl, dynamodb, call, time, list_to_atom(Host)], F) of
        {ok, {{200, _}, _Headers, Response}} ->
            dinerl_util:increment([dinerl,
                                   dynamodb,
                                   call,
                                   result,
                                   {endpoint, list_to_atom(Host)},
                                   {result, ok}]),
            {ok, Response};
        {ok, {{400, Code}, _Headers, ErrorString}} ->
            dinerl_util:increment([dinerl,
                                   dynamodb,
                                   call,
                                   result,
                                   {endpoint, list_to_atom(Host)},
                                   {result, '400'}]),
            {error, Code, ErrorString};
        {ok, {{413, Code}, _Headers, ErrorString}} ->
            dinerl_util:increment([dinerl,
                                   dynamodb,
                                   call,
                                   result,
                                   {endpoint, list_to_atom(Host)},
                                   {result, '413'}]),
            {error, Code, ErrorString};
        {ok, {{500, Code}, _Headers, ErrorString}} ->
            dinerl_util:increment([dinerl,
                                   dynamodb,
                                   call,
                                   result,
                                   {endpoint, list_to_atom(Host)},
                                   {result, '500'}]),
            {error, Code, ErrorString};
        {error, Reason} ->
            dinerl_util:increment([dinerl,
                                   dynamodb,
                                   call,
                                   result,
                                   {endpoint, list_to_atom(Host)},
                                   {result, error}]),
            {error, unknown, Reason};
        Other ->
            dinerl_util:increment([dinerl,
                                   dynamodb,
                                   call,
                                   result,
                                   {endpoint, list_to_atom(Host)},
                                   {result, unknown}]),
            {error, response, Other}
    end.
