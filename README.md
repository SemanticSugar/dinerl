Here's how you use this thing without using any macro help:

dinerl:setup("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "us-east-1b").
dinerl:create_table(<<"TestTable">>, [{<<"HashKeyElement">>, [{<<"AttributeName">>, <<"Key">>}, {<<"AttributeType">>, <<"S">>}]}], 50, 50).
dinerl:list_tables().
dinerl:put_item(<<"TestTable">>, [{<<"Key">>, [{<<"S">>, <<"jello">>}]}], []).
dinerl:get_item(<<"TestTable">>, [{<<"HashKeyElement">>, [{<<"S">>, <<"jello">>}]}], []).
