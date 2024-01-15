-module(file_loader).

-include("profile.hrl").

-define(NUM_PROCESSES, 4).

-export([run/1]).

run(FileName) ->
    ?START_PROFILING,
    CollectorPid = collector:start([?NUM_PROCESSES, #{}]),
    {ok, FullBinaryFile} = file:read_file(FileName),
    ByteLen = byte_size(FullBinaryFile),
    io:format("Loaded <~p>: ~p MB~n", [FileName, ByteLen / 1024 / 1024]),
    split_and_spawn_file_part(FullBinaryFile, ByteLen, CollectorPid, ?NUM_PROCESSES),
    {ok, CollectorPid}.

split_and_spawn_file_part(Bin, _, CollectorPid, 1) ->
    processor:start([Bin, CollectorPid]),
    ok;
split_and_spawn_file_part(Bin, BinLen, CollectorPid, ChunkNum) ->
    ChunkSize = BinLen div ChunkNum,
    ChunkOffset = BinLen - ChunkSize,
    {ScopedLinePos, _} = binary:match(Bin, <<"\n">>, [{scope, {ChunkOffset, ChunkSize}}]),
    NewSplit = binary:part(Bin, {ScopedLinePos + 1, BinLen - ScopedLinePos - 1}),
    processor:start([NewSplit, CollectorPid]),
    RemSize = BinLen - (BinLen - ScopedLinePos) + 1,
    RemainingBin = binary:part(Bin, {0, RemSize}),
    split_and_spawn_file_part(RemainingBin, RemSize, CollectorPid, ChunkNum - 1).
