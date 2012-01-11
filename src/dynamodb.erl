-module(dynamodb).
-author('Valentino Volonghi <valentino@adroll.com>').

-include("dinerl_types.hrl").

-export([endpoint/1, signature_header/7, call/7, call/8]).


-spec endpoint(zone()) -> endpoint().
endpoint("us-east-1" ++ _R) -> "dynamodb.us-east-1.amazonaws.com";
endpoint("us-west-1" ++ _R) -> "dynamodb.us-west-1.amazonaws.com";
endpoint("us-west-2" ++ _R) -> "dynamodb.us-west-2.amazonaws.com";
endpoint("ap-northeast-1" ++ _R) -> "dynamodb.ap-northeast-1.amazonaws.com";
endpoint("ap-southeast-1" ++ _R) -> "dynamodb.ap-southeast-1.amazonaws.com";
endpoint("eu-west-1" ++ _R) -> "dynamodb.eu-west-1.amazonaws.com".



signature_header(AccessKeyId, SecretAccessKey, Target, Token, Date, EndPoint, Body) ->
    SignString = ["POST", $\n,
                  "/", $\n, 
                  $\n, 
                  "host:", EndPoint, $\n,
                  "x-amz-date:", Date, $\n,
                  "x-amz-security-token:", Token, $\n,
                  "x-amz-target:", Target, $\n,
                  $\n,
                  Body],
    StringToSign = crypto:sha(SignString),
    Signature = base64:encode_to_string(crypto:sha_mac(SecretAccessKey, StringToSign)),
    {ok,
     {"x-amzn-authorization", 
      ["AWS3 AWSAccessKeyId=",
       AccessKeyId,
       ",Algorithm=HmacSHA1,SignedHeaders=host;x-amz-date;x-amz-security-token;x-amz-target,Signature=",
       Signature]}}.


-spec call(access_key_id(), secret_access_key(), 
           zone(), string(), token(), rfcdate(),
           any()) -> result().
call(AccessKeyId, SecretAccessKey, Zone, Target, Token, RFCDate, Body) ->
    call(AccessKeyId, SecretAccessKey, Zone, Target, Token, RFCDate, Body, 1000).

-spec call(access_key_id(), secret_access_key(), 
           zone(), string(), token(), rfcdate(),
           any(), integer()) -> result().
call(AccessKeyId, SecretAccessKey, Zone, Target, Token, RFCDate, Body, undefined) ->
    call(AccessKeyId, SecretAccessKey, Zone, Target, Token, RFCDate, Body, 1000);
call(AccessKeyId, SecretAccessKey, Zone, Target, Token, RFCDate, Body, Timeout) ->
    EndPoint = endpoint(Zone),
    {ok, SHeader} = signature_header(AccessKeyId, SecretAccessKey, Target,
                                     Token, RFCDate, EndPoint, Body),
    submit("http://" ++ EndPoint ++ "/", 
           [{"content-type", "application/x-amz-json-1.0"},
            {"x-amz-date", RFCDate},
            {"x-amz-security-token", Token},
            {"x-amz-target", Target}, SHeader], Body, Timeout).


-spec submit(endpoint(), headers(), any(), integer()) -> result().
submit(Endpoint, Headers, Body, Timeout) ->
    case lhttpc:request(Endpoint, "POST", Headers, Body, Timeout, [{max_connections, 5000}]) of
        {ok, {{200, _}, _Headers, Response}} ->
            {ok, Response};
        {ok, {{400, Code}, _Headers, ErrorString}} ->
            {error, Code, ErrorString};
        {ok, {{413, Code}, _Headers, ErrorString}} ->
            {error, Code, ErrorString};
        {ok, {{500, Code}, _Headers, ErrorString}} ->
            {error, Code, ErrorString};
        {error, Reason} ->
            {error, unknown, Reason}
    end.
