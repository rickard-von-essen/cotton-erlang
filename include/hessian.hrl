-define(M, 2).
-define(m, 0).

-define(CHUNK_SIZE, 1024).

-define(MegaSeconds, 1000000000).
-define(Seconds, 1000).
-define(MicroSeconds, 1000).
-define(UnixEpoch, 62167219200).

-record(encoding_state,{type_map, type_defs = []}).
-record(type_def,{native_type,foreign_type,fieldnames}).
