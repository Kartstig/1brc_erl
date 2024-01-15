-module(collector).

-include("profile.hrl").

-export([calculate_time_difference/2, gather_results/3, merge_aggregates/2, start/1]).

start(Args) ->
    Start = erlang:monotonic_time(millisecond),
    spawn(?MODULE, gather_results, [Start | Args]).

calculate_time_difference({Mega1, Secs1, Micro1}, {Mega2, Secs2, Micro2}) ->
    % Convert the entire timestamp to microseconds for each timestamp
    TotalMicro1 = Mega1 * 1000000 * 1000000 + Secs1 * 1000000 + Micro1,
    TotalMicro2 = Mega2 * 1000000 * 1000000 + Secs2 * 1000000 + Micro2,

    % Find the difference in microseconds
    MicroDiff = abs(TotalMicro1 - TotalMicro2),

    % Convert the difference back to seconds
    MicroDiff / 1000000.

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
