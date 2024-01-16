# One Billion Row Challenge

From https://github.com/gunnarmorling/1brc

## Requirements

1. Erlang
2. A really long text file (Best to generate it from the referenced repo)

To run:

```shell
$ ./run.sh test.txt
```

## Performance

This completes the 13GB file in ~80s on my 32 core Threadripper 1950X.

It's not the most efficient and I'm happy to take suggestions.

Here's a profile of a [processor](processor.erl):

```
****** Process <0.81.0>    -- 3.07 % of profiled time ***
FUNCTION                          CALLS        %       TIME  [uS / CALLS]
--------                          -----  -------       ----  [----------]
processor:process_chunk/2             1     0.00          1  [      1.00]
erlang:max/2                   31247080     2.15    5066758  [      0.16]
erlang:min/2                   31247080     2.19    5163737  [      0.17]
maps:get/3                     31247493     3.67    8651138  [      0.28]
erlang:binary_to_float/1       31247493     6.28   14797465  [      0.47]
processor:process_lines/3      31247494     6.30   14863477  [      0.48]
processor:process_line/2       31247493     6.73   15859309  [      0.51]
maps:put/3                     31247493     8.88   20942131  [      0.67]
processor:update_aggregate/3   31247493    11.11   26187540  [      0.84]
binary:split/2                 31247493    15.86   37396414  [      1.20]
processor:extract_line/2      431103333    36.85   86886865  [      0.20]
----------------------------  ---------  -------  ---------  [----------]
Total:                        712329946  100.00%  235814835  [      0.33]
```

[file_loader](file_loader.erl)

```
****** Process <0.9.0>    -- 0.00 % of profiled time ***
FUNCTION                                 CALLS        %  TIME  [uS / CALLS]
--------                                 -----  -------  ----  [----------]
init:b2a/1                                   1     0.00     0  [      0.00]
init:start_em/1                              1     0.00     0  [      0.00]
init:get_flag/3                              1     0.00     0  [      0.00]
io:request/3                                 1     0.00     0  [      0.00]
io:default_output/0                          1     0.00     0  [      0.00]
io:bc_req/3                                  1     0.00     0  [      0.00]
io:io_request/2                              1     0.00     0  [      0.00]
gen:call/4                                   2     0.00     0  [      0.00]
gen:'-call/4-fun-0-'/4                       2     0.00     0  [      0.00]
lists:keyfind/3                              1     0.00     0  [      0.00]
erlang:dt_restore_tag/1                      1     0.00     0  [      0.00]
erlang:dt_spread_tag/1                       1     0.00     0  [      0.00]
erlang:group_leader/0                        1     0.00     0  [      0.00]
erlang:list_to_tuple/1                       1     0.00     0  [      0.00]
erlang:make_ref/0                            1     0.00     0  [      0.00]
erlang:monotonic_time/1                      1     0.00     0  [      0.00]
erlang:'++'/2                                1     0.00     0  [      0.00]
file:native_name_encoding/0                  1     0.00     0  [      0.00]
file:read_file/1                             1     0.00     0  [      0.00]
file:file_name/1                             1     0.00     0  [      0.00]
file:check_and_call/2                        1     0.00     0  [      0.00]
file:check_args/1                            2     0.00     0  [      0.00]
net_kernel:dflag_unicode_io/1                1     0.00     0  [      0.00]
collector:start/1                            1     0.52     1  [      1.00]
io:format/2                                  1     0.52     1  [      1.00]
gen:do_for_proc/2                            2     0.52     1  [      0.50]
code:call/1                                  3     0.52     1  [      0.33]
gen_server:call/3                            2     0.52     1  [      0.50]
erlang:atom_to_list/1                        1     0.52     1  [      1.00]
erlang:function_exported/3                   3     0.52     1  [      0.33]
file:file_name_1/2                           3     0.52     1  [      0.33]
code:ensure_loaded/1                         3     1.04     2  [      0.67]
erlang:module_loaded/1                       3     1.04     2  [      0.67]
file:call/2                                  1     1.04     2  [      2.00]
io:execute_request/3                         1     1.56     3  [      3.00]
error_handler:ensure_loaded/1                3     1.56     3  [      1.00]
error_handler:undefined_function/3           3     2.08     4  [      1.33]
erlang:whereis/1                             5     2.08     4  [      0.80]
processor:start/1                           32     4.17     8  [      0.25]
code_server:call/1                           3     4.17     8  [      2.67]
binary:part/2                               62     5.21    10  [      0.16]
erlang:monitor/2                             6     5.21    10  [      1.67]
gen:do_call/4                                2     7.81    15  [      7.50]
erlang:demonitor/2                           7     8.33    16  [      2.29]
file_loader:split_and_spawn_file_part/4     32    11.46    22  [      0.69]
binary:match/3                              31    17.19    33  [      1.06]
erlang:spawn/3                              33    21.88    42  [      1.27]
---------------------------------------  -----  -------  ----  [----------]
Total:                                     269  100.00%   192  [      0.71]
```

[collector](collector.erl)

```
****** Process <0.80.0>    -- 0.00 % of profiled time ***
FUNCTION                                  CALLS        %   TIME  [uS / CALLS]
--------                                  -----  -------   ----  [----------]
io:request/3                                  2     0.00      0  [      0.00]
io:io_request/2                               2     0.00      0  [      0.00]
gen:call/4                                    1     0.00      0  [      0.00]
gen:'-call/4-fun-0-'/4                        1     0.00      0  [      0.00]
gen_server:call/3                             1     0.00      0  [      0.00]
erlang:make_ref/0                             2     0.00      0  [      0.00]
erlang:monotonic_time/1                       1     0.00      0  [      0.00]
net_kernel:dflag_unicode_io/1                 2     0.00      0  [      0.00]
io:default_output/0                           2     0.00      1  [      0.50]
io:bc_req/3                                   2     0.00      1  [      0.50]
gen:do_for_proc/2                             1     0.00      1  [      1.00]
erlang:group_leader/0                         2     0.00      1  [      0.50]
erlang:whereis/1                              1     0.00      1  [      1.00]
gen:do_call/4                                 1     0.01      2  [      2.00]
erlang:demonitor/2                            2     0.01      2  [      1.00]
io:format/2                                   2     0.01      3  [      1.50]
erlang:monitor/2                              3     0.01      4  [      1.33]
maps:iterator/1                              32     0.03      9  [      0.28]
maps:fold/3                                  32     0.12     35  [      1.09]
collector:merge_aggregates/2                 32     0.14     42  [      1.31]
io:execute_request/3                          2     0.15     45  [     22.50]
collector:gather_results/3                   33     0.30     88  [      2.67]
erts_internal:map_next/3                     64     2.03    601  [      9.39]
erlang:max/2                              12803     6.51   1930  [      0.15]
erlang:min/2                              12803     6.56   1943  [      0.15]
maps:next/1                               13248     7.33   2173  [      0.16]
maps:fold_1/3                             13248    17.81   5276  [      0.40]
maps:put/3                                13216    26.78   7936  [      0.60]
collector:'-merge_aggregates/2-fun-0-'/4  13216    32.18   9535  [      0.72]
----------------------------------------  -----  -------  -----  [----------]
Total:                                    78757  100.00%  29629  [      0.38]
```

## Improvements

PRs welcome :D
