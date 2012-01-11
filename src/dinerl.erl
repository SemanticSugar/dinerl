-module(dinerl).
-author('Valentino Volonghi <valentino@adroll.com>').

-export([api/7]).

method_name(batch_get_item) ->
    "DynamoDBv20110924.BatchGetItem";
method_name(create_table) ->
    "DynamoDBv20110924.CreateTable";
method_name(delete_item) ->
    "DynamoDBv20110924.DeleteItem";
method_name(delete_table) ->
    "DynamoDBv20110924.DeleteTable";
method_name(describe_table) ->
    "DynamoDBv20110924.DescribeTable";
method_name(get_item) ->
    "DynamoDBv20110924.GetItem";
method_name(list_tables) ->
    "DynamoDBv20110924.ListTables";
method_name(put_item) ->
    "DynamoDBv20110924.PutItem";
method_name(q) ->
    "DynamoDBv20110924.Query";
method_name(scan) ->
    "DynamoDBv20110924.Scan";
method_name(update_item) ->
    "DynamoDBv20110924.UpdateItem";
method_name(update_table) ->
    "DynamoDBv20110924.UpdateTable";



api(AccessKeyId, SecretAccessKey, Zone, Token, RFCDate, Name, Body) ->
    case dynamodb:call(AccessKeyId, SecretAccessKey, method_name(Name), Token, RFCDate, mochijson2:encode(Body)) of
        {ok, Response} ->
            {ok, mochijson2:decode(Response)};
        {error, Reason} ->
            {error, Reason}
    end.
