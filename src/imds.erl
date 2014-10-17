%%% @copyright (C) 2014, AdRoll
%%% @doc
%%%         Helper module for obtaining access tokens from the instance
%%%         metadata server (or hologram).
%%% @end
%%% Created : 17 Oct 2014 by Mike Watters <mike.watters@adroll.com>

-module(imds).

-export([get_role_name/0,
         get_session_token/0]).


-define(IAM_URL, "http://169.254.169.254/latest/meta-data/iam/").
-define(IAM_ROLES_URL, ?IAM_URL ++ "security-credentials/").
-define(MDS_TIMEOUT, 200000).

%%%% API


%% @doc Obtain the current role name from the instance metadata server.
-spec get_role_name() -> {error, term()} | {ok, string()}.
get_role_name() ->
    case catch(lhttpc:request(?IAM_ROLES_URL, 'GET', [], ?MDS_TIMEOUT)) of
        {ok, {{200, "OK"}, Headers, Body}} ->
            case mime_type(Headers) of
                "text/plain" ->
                    %% fixme; assumes utf-8 encoding.
                    {ok, unicode:characters_to_list(Body, utf8)};
                _ ->
                    {error, unexpected_mime_type}
            end;
        _ ->
            {error, bad_role_response}
    end.


%% @doc Obtain a session token from the instance metadata server,
%% returning a proplist containing 'expiration', 'access_key_id',
%% 'secret_access_key', and 'token' entries.
-spec get_session_token() -> {error, term()} | list().
get_session_token() ->
    case get_role_name() of
        {ok, RoleName} ->
            %% fixme; urlencode the role name.
            TokenUrl = ?IAM_ROLES_URL ++ RoleName,
            case catch(lhttpc:request(TokenUrl, 'GET', [], ?MDS_TIMEOUT)) of
                {ok, {{200, "OK"}, _Headers, Body}} ->
                    %% note: response type is (currently text/plain), but the body is JSON.
                    metadata_response_to_token_proplist(Body);
                _ ->
                    {error, bad_token_response}
            end;
        Error ->
            Error
    end.



%%%% INTERNAL FUNCTIONS

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
                                                [{AtomName, Value} | Acc];
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
    Expected = [{expiration, <<"2014-10-18T09:00:30Z">>},
                {access_key_id, <<"XYZZY">>},
                {secret_access_key, <<"FLOOBLE">>},
                {token, <<"BAZZLE">>}],
    [?assertEqual(proplists:get_value(Key, Expected),
                  proplists:get_value(Key, Result))
     || Key <- [expiration, access_key_id, secret_access_key, token]],
    ok.


-endif.
