Here's how you use this thing without using any macro help:

    dinerl:setup("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "us-east-1b").
    dinerl:create_table(<<"TestTable">>, [{<<"HashKeyElement">>, [{<<"AttributeName">>, <<"Key">>}, {<<"AttributeType">>, <<"S">>}]}], 50, 50).
    dinerl:list_tables().
    dinerl:put_item(<<"TestTable">>, [{<<"Key">>, [{<<"S">>, <<"jello">>}]}], []).
    dinerl:get_item(<<"TestTable">>, [{<<"HashKeyElement">>, [{<<"S">>, <<"jello">>}]}], []).

    put(Key, Value, TTL, Now) ->
        dinerl:put_item(<<"Attributions">>, [{<<"UserKey">>, [{<<"S">>, Key}]},
                                             {<<"Updated">>, [{<<"N">>, list_to_binary(integer_to_list(Now))}]},
                                             {<<"TTL">>, [{<<"N">>, list_to_binary(integer_to_list(TTL))}]},
                                             {<<"Value">>, [{<<"S">>, Value}]}], []).

    get(Key, Now, Default) ->
        case dinerl:get_item(<<"Attributions">>, [{<<"HashKeyElement">>, [{<<"S">>, Key}]}], [{attrs, [<<"TTL">>, <<"Updated">>, <<"Value">>, <<"Visited">>]}]) of
            {ok, Element} ->
                ParsedResult = parsejson(Element),
                return_if_not_expired(Key, ParsedResult, Now, Default);

            {error, Short, Long} ->
                io:format("~p", [Long]),
                Default
        end.

    add(Key, Value, TTL, Now) ->
        dinerl:update_item(<<"Attributions">>,
                           [{<<"HashKeyElement">>, [{<<"S">>, Key}]}],
                           [{update, [{<<"Visited">>, [{value, [{<<"SS">>, [Value]}]},
                                                       {action, add}]},
                                      {<<"Updated">>, [{value, [{<<"N">>, list_to_binary(integer_to_list(Now))}]},
                                                       {action, put}]},
                                      {<<"TTL">>, [{value, [{<<"N">>, list_to_binary(integer_to_list(TTL))}]},
                                                   {action, put}]}]}]).



    parsejson([]) ->
        [];
    parsejson({struct, L}) ->
        parsejson(L);
    parsejson([{<<"Item">>, {struct, Fields}}|_Rest]) ->
        parsejsonfields(Fields, []);
    parsejson([_H|T]) ->
        parsejson(T).

    parsejsonfields([], Acc) ->
        Acc;
    parsejsonfields([{Name, {struct, [{<<"N">>, Value}]}}|Rest], Acc) ->
        parsejsonfields(Rest, [{Name, list_to_integer(binary_to_list(Value))}|Acc]);
    parsejsonfields([{Name, {struct, [{<<"S">>, Value}]}}|Rest], Acc) ->
        parsejsonfields(Rest, [{Name, Value}|Acc]);
    parsejsonfields([{Name, {struct, [{<<"NS">>, Value}]}}|Rest], Acc) ->
        parsejsonfields(Rest, [{Name, all_to_int(Value)}|Acc]);
    parsejsonfields([{Name, {struct, [{<<"SS">>, Value}]}}|Rest], Acc) ->
        parsejsonfields(Rest, [{Name, Value}|Acc]).


    return_if_not_expired(_, [], _, Default) ->
        Default;
    return_if_not_expired(Key, ParsedResult, Now, Default) ->
        Updated = proplists:get_value(<<"Updated">>, ParsedResult),
        TTL = proplists:get_value(<<"TTL">>, ParsedResult),
        case (Updated+TTL) > Now of
            true ->
                ParsedResult;
            false ->
                dinerl:delete_item(<<"Attributions">>, [{<<"HashKeyElement">>, [{<<"S">>, Key}]}], []),
                Default
        end.

    all_to_int(L) ->
        all_to_int(L, []).
    all_to_int([], Acc) ->
        lists:reverse(Acc);
    all_to_int([H|T], Acc) ->
        all_to_int(T, [list_to_integer(binary_to_list(H))|Acc]).

    pytime() ->
        pytime(erlang:now()).
    pytime({MegaSecs, Secs, MicroSecs}) ->
        erlang:trunc((1.0e+6 * MegaSecs) + Secs + (1.0e-6 * MicroSecs)).

