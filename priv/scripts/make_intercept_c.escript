
%% Our escript header is created by Make.

platform() ->
    case os:cmd("uname -s") of
        "Darwin\n" ->
            osx
    end.

main([InConfigName, OutCBase]) ->
    InConfigA = list_to_atom(InConfigName),
    Config = InConfigA:config(),
    OutCPath = OutCBase ++ ".c",
    ok = faulterl:make(OutCPath, Config),
    OutExport = OutCBase ++ ".export",
    Cmd1 = "egrep '^/\\*--export-- ' " ++ OutCPath ++ " | awk '{printf(\"_%s\\n\", $2)}' > " ++ OutExport,
    io:format("~s\n", [Cmd1]),
    Out1 = os:cmd(Cmd1),
    io:format("~s", [Out1]),
    Cmd2 = case platform() of
               osx ->
                   "gcc -g -o " ++ OutCBase ++ ".dylib -O0 -shared -Xlinker -exported_symbols_list -Xlinker " ++ OutExport ++ " " ++ OutCPath
           end,
    io:format("~s\n", [Cmd2]),
    Out2 = os:cmd(Cmd2),
    io:format("~s", [Out2]),
    io:format("~s", [os:cmd("ls -ld " ++ OutCBase ++ "*")]),
    erlang:halt(0);
main(_Args) ->
    io:format("Usage: ${me} input-config-name output-c-source-basename\n"),
    erlang:halt(1).
