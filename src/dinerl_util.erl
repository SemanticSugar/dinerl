-module(dinerl_util).

-export([get_env/1, noop/2, time_call/2, time_call/3, increment/1, increment/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv time_call(Metric, Fun, 1.0)
-spec time_call(term(), fun(() -> Result)) -> Result.
time_call(Metric, Fun) ->
    time_call(Metric, Fun, 1.0).

%% @doc Evaluates Fun() and (with a probability of Ratio) reports its evaluation
%%      time as a histogram metric called Metric.
-spec time_call(term(), fun(() -> Result), number()) -> Result.
time_call(Metric, Fun, 1.0) ->
    Start = erlang:system_time(microsecond),
    Result = Fun(),
    End = erlang:system_time(microsecond),
    Diff = End - Start,
    histogram(Metric, Diff),
    Result;
time_call(Metric, Fun, Ratio) when is_number(Ratio) ->
    Start = erlang:system_time(microsecond),
    Result = Fun(),
    case rand:uniform() =< Ratio of
        true ->
            End = erlang:system_time(microsecond),
            Diff = End - Start,
            histogram(Metric, Diff);
        false ->
            ok
    end,
    Result.

histogram(Metric, Value) ->
    {StatMod, StatFun} = histogram_stat_callback(),
    erlang:apply(StatMod, StatFun, [Metric, Value]).

increment(Metric) ->
    increment(Metric, 1).

increment(Metric, Value) ->
    {StatMod, StatFun} = increment_stat_callback(),
    erlang:apply(StatMod, StatFun, [Metric, Value]).

noop(_Key, _Value) ->
    ok.

get_env(Key) ->
    case application:get_env(dinerl, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            exit({undefined_configuration, Key})
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
histogram_stat_callback() ->
    get_env(histogram_stat_callback).

increment_stat_callback() ->
    get_env(increment_stat_callback).
