-define(PROFILE_LOG_FILE, "profile.log").

-ifdef(profile).

-define(START_PROFILING,
        eprof:start(),
        eprof:start_profiling([self()]),
        eprof:log(?PROFILE_LOG_FILE)).
-define(STOP_PROFILING, eprof:stop_profiling(), eprof:analyze()).

- else .

-define(START_PROFILING, true).
-define(STOP_PROFILING, true).

-endif.
