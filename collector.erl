-module(collector).

-include("profile.hrl").

-export([gather_results/3, merge_aggregates/2, start/1]).

start(Args) ->
    Start = erlang:monotonic_time(millisecond),
    spawn(?MODULE, gather_results, [Start | Args]).

gather_results(Start, 0, Aggregate) ->
    io:format("Results: ~p~n", [Aggregate]),
    End = erlang:monotonic_time(millisecond),
    DiffSeconds = (End - Start) / 1000,
    io:format("Took: ~p s~n", [DiffSeconds]),
    ?STOP_PROFILING,
    init:stop();
gather_results(_Start, Count, Aggregate) ->
    receive
        {From, output} ->
            From ! Aggregate,
            gather_results(_Start, Count, Aggregate);
        {done, SubAggregate} ->
            UpdatedAggregate = merge_aggregates(Aggregate, SubAggregate),
            gather_results(_Start, Count - 1, UpdatedAggregate)
    end.

merge_aggregates(Agg1, Agg2) ->
    maps:fold(fun(City, {Count, Min, Max, Total}, Acc) ->
                 case maps:is_key(City, Agg1) of
                     true ->
                         {OldCount, OldMin, OldMax, OldTotal} = maps:get(City, Agg1),
                         NewCount = OldCount + Count,
                         NewMin = min(OldMin, Min),
                         NewMax = max(OldMax, Max),
                         NewTotal = OldTotal + Total,
                         maps:put(City, {NewCount, NewMin, NewMax, NewTotal}, Acc);
                     false ->
                         maps:put(City, {Count, Min, Max, Total}, Acc)
                 end
              end,
              Agg1,
              Agg2).
