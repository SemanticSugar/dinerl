-module(awsrequest).
-author('Valentino Volonghi <valentino@adroll.com>').

-export([add_sign/5, get_timestamp/1]).

add_sign(AccessKeyId, SecretAccessKey, HostUrl, Method, ArgList) ->
    Path = "/",
    UrlParams = [{"AWSAccessKeyId", AccessKeyId},
                 {"SignatureMethod", "HmacSHA1"},
                 {"SignatureVersion", "2"},
                 {"Timestamp", get_timestamp(new)}],

    EncodedUrlParams = urlencode(lists:keysort(1, lists:merge(UrlParams, ArgList))),
    SignString = [Method, $\n, HostUrl, $\n, Path, $\n, EncodedUrlParams],
    Signature = base64:encode_to_string(crypto:sha_mac(SecretAccessKey, SignString)),
    EncodedUrlParams ++ "&Signature=" ++ quote_plus(Signature).

get_timestamp(new) ->
    get_timestamp(erlang:universaltime());
get_timestamp({_M, _S, _Micro}=Now) ->
    get_timestamp(calendar:now_to_universal_time(Now));
get_timestamp({{Y, M, D}, {H, Mn, S}}) ->
    lists:flatten(
      io_lib:fwrite("~4..0B-~2..0B-~2..0BT~2..0B:~2.10.0B:~2.10.0B.000Z",
                    [Y, M, D, H, Mn, S])).




%%%% Taken from mochiweb %%%%
-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [S]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [S, Separator | Acc]).

%% @spec quote_plus(atom() | integer() | string() | binary()) -> string()
%% @doc URL safe encoding of the given term.
quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
    quote_plus(binary_to_list(Binary));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$0, $2, ?PERCENT | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

%% @spec urlencode([{Key, Value}]) -> string()
%% @doc URL encode the property list.
urlencode(Props) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
                                   [[quote_plus(K), $=, quote_plus(V)] | Acc]
                           end, [], Props),
    lists:flatten(revjoin(RevPairs, $&, [])).
%%%%
