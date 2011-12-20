-module(dynamodb).
-author('Valentino Volonghi <valentino@adroll.com>').

-export([endpoint/1, signature_header/7, call/7]).


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
     {"x-amzn-authorization", ["AWS3 AWSAccessKeyId=", AccessKeyId, ",Algorithm=HmacSHA1,SignedHeaders=host;x-amz-date;x-amz-security-token;x-amz-target,Signature=", Signature]}}.



call(AccessKeyId, SecretAccessKey, Zone, Target, Token, RFCDate, Body) ->
    EndPoint = endpoint(Zone),
    {ok, SHeader} = signature_header(AccessKeyId, SecretAccessKey, Target, Token, RFCDate, EndPoint, Body),
    submit("http://" ++ EndPoint ++ "/", [{"content-type", "application/x-amz-json-1.0"}, {"x-amz-date", RFCDate}, {"x-amz-security-token", Token}, {"x-amz-target", Target}, SHeader], Body).



submit(Endpoint, Headers, Body, Timeout) ->
    case lhttpc:request(Endpoint, "POST", Headers, Body, Timeout, [{max_connections, 5000}]) of
        {ok, {{200, _}, _Headers, Response}} ->
            {ok, Response};
        {error, Reason} ->
            {error, Reason}
    end.
