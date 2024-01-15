-module(processor).

-export([process_chunk/2, start/1, update_aggregate/3]).

start(Args) ->
    spawn(?MODULE, process_chunk, Args).

process_chunk(Chunk, CollectorPid) ->
    process_lines(Chunk, CollectorPid, #{}).

process_lines(<<>>, CollectorPid, Aggregate) ->
    CollectorPid ! {done, Aggregate};
process_lines(Chunk, CollectorPid, Aggregate) ->
    {Line, Rest} = extract_line(Chunk, <<>>),
    NewAggregate = process_line(Line, Aggregate),
    process_lines(Rest, CollectorPid, NewAggregate).

extract_line(<<>>, Accum) ->
    {Accum, <<>>};
extract_line(<<"\n", Rest/binary>>, Accum) ->
    {Accum, Rest};
extract_line(<<Char, Rest/binary>>, Accum) ->
    extract_line(Rest, <<Accum/binary, Char>>).

process_line(Line, Aggregate) ->
    [CityBin, TempBin] = binary:split(Line, <<";">>),
    update_aggregate(CityBin, binary_to_float(TempBin), Aggregate).

update_aggregate(City, Temp, Aggregate) ->
    case maps:get(City, Aggregate, undefined) of
        undefined ->
            maps:put(City, {1, Temp, Temp, Temp}, Aggregate);
        {OldCount, OldMin, OldMax, OldTotal} ->
            NewAggregate = {OldCount + 1, min(OldMin, Temp), max(OldMax, Temp), OldTotal + Temp},
            maps:put(City, NewAggregate, Aggregate)
    end.
