-module(dinerl_client).
-author('Valentino Volonghi <valentino@adroll.com>').

-include("dinerl_types.hrl").

-export([api/5, api/6]).

%%
%% Item related operations
%%
-spec method_name(method()) -> string().
method_name(batch_get_item) ->
    "DynamoDB_20111205.BatchGetItem";
method_name(batch_write_item) ->
    "DynamoDB_20120810.BatchWriteItem";
method_name(get_item) ->
    "DynamoDB_20111205.GetItem";
method_name(put_item) ->
    "DynamoDB_20111205.PutItem";
method_name(delete_item) ->
    "DynamoDB_20111205.DeleteItem";
method_name(update_item) ->
    "DynamoDB_20120810.UpdateItem";

%%
%% Table related operations
%%
method_name(create_table) ->
    "DynamoDB_20111205.CreateTable";
method_name(list_tables) ->
    "DynamoDB_20111205.ListTables";
method_name(describe_table) ->
    "DynamoDB_20111205.DescribeTable";
method_name(update_table) ->
    "DynamoDB_20111205.UpdateTable";
method_name(delete_table) ->
    "DynamoDB_20111205.DeleteTable";

%%
%% query interface
%%
method_name(query_item_20120810) ->
    "DynamoDB_20120810.Query";

method_name(query_item_20111205) ->
    "DynamoDB_20111205.Query";
method_name(scan) ->
    "DynamoDB_20111205.Scan".


-spec api(awsv4:credentials(), zone(), aws_datetime(), method(), any(), undefined | integer()) -> result().
api(Credentials, Zone, ISODate, Name, Body, Timeout) ->
    case dynamodb:call(Credentials, Zone, method_name(Name),
                       ISODate, dmochijson2:encode(Body), Timeout) of
        {ok, Response} ->
            {ok, dmochijson2:decode(Response)};
        {error, Code, Reason} ->
            {error, Code, Reason}
    end.

api(Credentials, Zone, ISODate, Name, Body) ->
    api(Credentials, Zone, ISODate, Name, Body, undefined).
