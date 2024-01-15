-module(processor).

-export([process_chunk/2, start/1, update_aggregate/3]).

start(Args) ->
    spawn(?MODULE, process_chunk, Args).

process_chunk(Chunk, CollectorPid) ->
    Lines = binary:split(Chunk, <<"\n">>, [global, trim]),
    Aggregate = parse_lines(Lines, #{}),
    CollectorPid ! {done, Aggregate}.

parse_lines([], Aggregate) ->
    Aggregate;
parse_lines([Line | Rem], Aggregate) ->
    % Convert binary to string if necessary
    StringLine =
        case erlang:is_binary(Line) of
            true ->
                binary_to_list(Line);
            false ->
                Line
        end,
    NewAggregate =
        case string:split(StringLine, ";", all) of
            [City, TempStr] when is_list(City) andalso is_list(TempStr) ->
                try list_to_float(TempStr) of
                    Temp ->
                        update_aggregate(City, Temp, Aggregate)
                catch
                    _:_ ->
                        % Handle non-float temperature or other parsing errors
                        io:format("Couldn't read temperature from line: ~p~n", [Line]),
                        Aggregate
                end;
            _ ->
                io:format("Couldn't split line: ~p~n", [Line]),
                Aggregate
        end,
    parse_lines(Rem, NewAggregate).

update_aggregate(City, Temp, Aggregate) ->
    case maps:get(City, Aggregate, undefined) of
        undefined ->
            maps:put(City, {1, Temp, Temp, Temp}, Aggregate);
        {OldCount, OldMin, OldMax, OldTotal} ->
            NewAggregate = {OldCount + 1, min(OldMin, Temp), max(OldMax, Temp), OldTotal + Temp},
            maps:put(City, NewAggregate, Aggregate)
    end.
