%%% @copyright (C) 2014, AdRoll
%%% @doc
%%%         Helper module for obtaining information from an instance
%%%         metadata server.
%%% @end
%%% Created : 17 Oct 2014 by Mike Watters <mike.watters@adroll.com>

-module(imds).

-export([role_name/0,
         zone/0,
         instance_id/0,
         public_hostname/0,
         get_session_token/0,
         imds_response/3,
         imds_response/4]).


-define(IMDS_HOST, "169.254.169.254").
-define(IMDS_URL, "http://" ++ ?IMDS_HOST ++ "/latest/meta-data/").
-define(INSTANCE_ID_URL, ?IMDS_URL ++ "instance-id").
-define(INSTANCE_HOSTNAME_URL, ?IMDS_URL ++ "public-hostname").
-define(AZ_URL, ?IMDS_URL ++ "placement/availability-zone").
-define(IAM_URL, ?IMDS_URL ++ "iam/").
-define(IAM_ROLES_URL, ?IAM_URL ++ "security-credentials/").
-define(IMDS_HEADERS, [{"Connection", "Close"}]).
-define(IMDS_TIMEOUT, 30000).
-define(IMDS_RETRIES, 3).


%%%% API

%% @doc Obtain the current role name from the instance metadata server.
-spec role_name() -> {error, term()} | {ok, string()}.
role_name() ->
    imds_text_response(?IAM_ROLES_URL).

%% @doc Obtain the name of the current availability zone from the
%% instance metadata server.
-spec zone() -> {error, term()} | {ok, string()}.
zone() ->
    imds_text_response(?AZ_URL).

-spec instance_id() -> {error, term()} | {ok, string()}.
instance_id() ->
    imds_text_response(?INSTANCE_ID_URL).

-spec public_hostname() -> {error, term()} | {ok, string()}.
public_hostname() ->
    imds_text_response(?INSTANCE_HOSTNAME_URL).


%% @doc Obtain a session token from the instance metadata server,
%% returning a proplist containing 'expiration', 'access_key_id',
%% 'secret_access_key', and 'token' entries.
-spec get_session_token() -> {error, term()} | list(proplists:property()).
get_session_token() ->
    case role_name() of
        {ok, RoleName} ->
            %% fixme; urlencode the role name.
            TokenUrl = ?IAM_ROLES_URL ++ RoleName,
            imds_token_response(TokenUrl);
        Error ->
            Error
    end.

%% @doc Make a GET request to the given URL, expecting (accepting) the
%% given mime types, and with the given request timeout in
%% milliseconds.
-spec imds_response(string(), list(string()), pos_integer()) ->
    {ok, term()} | {error, term()}.
imds_response(Url, MimeTypes, Timeout) ->
    AcceptHeader = {"Accept", string:join(MimeTypes, ", ")},
    RequestHeaders = [AcceptHeader | ?IMDS_HEADERS],
    case httpc:request(get, {Url, RequestHeaders}, [{timeout, Timeout}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            case lists:member(mime_type(Headers), MimeTypes) of
                true ->
                    {ok, Body};
                false ->
                    %% the server ignored our accept header:
                    {error, unacceptable_response}
            end;
        {ok, {{_, 406, _}, _, _}} ->
            %% the server respected our accept header and could not
            %% produce a response with any of the requested mime
            %% types:
            {error, unacceptable_response};
        {ok, {{_, Code, Status}, _, _}} ->
            {error, {bad_response, {Code, Status}}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc As in imds_response/3, but wrapping with catch & retry.
-spec imds_response(string(), list(string()), pos_integer(), pos_integer()) ->
    {ok, term()} | {error, term()}.
imds_response(Url, MimeTypes, Timeout, Retries) ->
    call_with_retry(?MODULE, imds_response, [Url, MimeTypes, Timeout],
                    "Could not obtain IMDS response: ~p~n",
                    Retries).


%%%% INTERNAL FUNCTIONS

%% @doc Call the given arity-1 Transform function with the result of a
%% successful call to imds_response/4, or return the error which
%% resulted from that call.
-spec imds_transform_response(string(), list(string()), function()) ->
    {error, term()} | term().
imds_transform_response(Url, MimeTypes, Transform) ->
    case imds_response(Url, MimeTypes, ?IMDS_TIMEOUT, ?IMDS_RETRIES) of
        {ok, Result} ->
            Transform(Result);
        Error ->
            Error
    end.


%%
-spec imds_text_response(string()) ->
    {ok, string()} | {error, term()}.
imds_text_response(Url) ->
    imds_transform_response(Url, ["text/plain"],
                            %% fixme; assumes utf-8 encoding.
                            fun (Result) ->
                                    case unicode:characters_to_list(Result) of
                                        {error, _, _} ->
                                            {error, invalid_unicode};
                                        {incomplete, _, _} ->
                                            {error, invalid_unicode};
                                        String ->
                                            {ok, String}
                                    end
                            end).

%%
-spec imds_token_response(string()) ->
    list(proplists:property()) | {error, term()}.
imds_token_response(Url) ->
    MimeTypes = ["text/plain", "application/json"],
    imds_transform_response(Url, MimeTypes,
                            fun metadata_response_to_token_proplist/1).


%% @doc Obtain relevant values from the JSON response body, returning
%% a proplist with atom keys and the appropriate values.
metadata_response_to_token_proplist(Body) ->
    Targets = [{<<"Expiration">>, expiration},
               {<<"AccessKeyId">>, access_key_id},
               {<<"SecretAccessKey">>, secret_access_key},
               {<<"Token">>, token}],
    case dmochijson2:decode(Body) of
        {struct, Plist} ->
            case proplists:get_value(<<"Code">>, Plist) of
                <<"Success">> ->
                    lists:foldl(fun ({Element, Value}, Acc) ->
                                        case lists:keyfind(Element, 1, Targets) of
                                            {_, AtomName} ->
                                                [{AtomName, binary_to_list(Value)} | Acc];
                                            _ ->
                                                Acc
                                        end
                                end,
                                [],
                                Plist);
                _ ->
                    {error, failed_token_response}
            end;
        _ ->
            {error, invalid_token_json}
    end.


%% @doc Return the base mime type based on the given response headers,
%% assuming text/plain if that header is missing.
-spec mime_type(list()) -> string().
mime_type(Headers) ->
    case find_header("content-type", Headers) of
        {ok, MimeType} ->
            [BaseType|_] = string:tokens(MimeType, ";"),
            string:strip(BaseType);
        _ ->
            "text/plain"
    end.


%% @doc Return the named HTTP response header from the given proplist
%% of headers (case-insensitive).
-spec find_header(string(), list(proplists:property())) -> undefined | {ok, string()}.
find_header(Name, Headers) ->
    case lists:keyfind(string:to_lower(Name), 1, [{string:to_lower(HeaderName), HeaderValue}
                                                  || {HeaderName, HeaderValue} <- Headers]) of
        {_, Value} ->
            {ok, Value};
        _ ->
            undefined
    end.



%% @doc Call the given M:F with Args, emitting an error with the given
%% ErrorFormat (with the error as the single format argument) and
%% retrying otherwise.
-spec call_with_retry(module(), function(), list(), string(), integer()) ->
    {ok, term()} | {error, term()}.
call_with_retry(Module, Fun, Args, ErrorFormat, Retries) ->
    if
        Retries > 0 ->
            case catch(apply(Module, Fun, Args)) of
                {ok, Result} ->
                    {ok, Result};
                Error ->
                    error_logger:error_msg(ErrorFormat, [Error]),
                    call_with_retry(Module, Fun, Args, ErrorFormat, Retries - 1)
            end;
        true ->
            {error, retries_exceeded}
    end.



%%%% UNIT TESTS
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


find_header_test() ->
    Headers = [{"Content-Type","text/plain; charset=utf-8"},
               {"Content-Length","15"},
               {"Date","Fri, 17 Oct 2014 21:41:13 GMT"}],
    ?assertMatch({ok, "15"}, find_header("content-length", Headers)),
    ?assertMatch({ok, "text/plain; charset=utf-8"}, find_header("Content-Type", Headers)),
    ?assertEqual(undefined, find_header("X-YZZY", Headers)),
    ok.


mime_type_test() ->
    Headers = [{"Content-Type","text/plain; charset=utf-8"},
               {"Content-Length","15"},
               {"Date","Fri, 17 Oct 2014 21:41:13 GMT"}],
    ?assertEqual("text/plain", mime_type(Headers)),
    ?assertEqual("text/plain", mime_type([])),
    ?assertEqual("text/html", mime_type([{"content-type", "text/html; foo=bar"}])),
    ok.


metadata_response_to_proplist_test() ->
    Body = <<"{\"Code\":\"Success\",\"LastUpdated\":\"2014-10-17T15:17:07-07:00\",\"Type\":\"AWS-HMAC\",\"AccessKeyId\":\"XYZZY\",\"SecretAccessKey\":\"FLOOBLE\",\"Token\":\"BAZZLE\",\"Expiration\":\"2014-10-18T09:00:30Z\"}">>,
    Result = metadata_response_to_token_proplist(Body),
    Expected = [{expiration, "2014-10-18T09:00:30Z"},
                {access_key_id, "XYZZY"},
                {secret_access_key, "FLOOBLE"},
                {token, "BAZZLE"}],
    [?assertEqual(proplists:get_value(Key, Expected),
                  proplists:get_value(Key, Result))
     || Key <- [expiration, access_key_id, secret_access_key, token]],
    ok.


-endif.
