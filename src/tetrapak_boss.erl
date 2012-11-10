-module(tetrapak_boss).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([run/2]).
-export([appname/0]).

app() ->
    false.

tasks(tasks) ->
    case filelib:is_dir(tetrapak:path("src/controller")) orelse
         filelib:is_dir(tetrapak:path("src/model")) of
        true ->
            [{Cmd, ?MODULE, Description} || {Cmd, Description} <- tasks()];
        false ->
            []
    end;

tasks(_) ->
    [].

tasks() ->
    [{"start:dev", "start ChicagoBoss for development"},
     {"build:lang", "build language files"},
     {"build:erlang", "compile Erlang modules using ChicagoBoss"}].

run("start:dev", _) ->
    App = appname(),
    tpk_file:mkdir(tetrapak:path("log")),
    Config = [{developing_app, App}, {applications, [App]}, {db_host, "localhost"},
              {db_port, 1978}, {db_adapter, mock}, {log_dir, "log"}, {server, mochiweb},
              {port, 8001}, {session_adapter, mock}, {session_key, "_boss_session"},
              {session_exp_time, 525600}],
    [application:set_env(boss, ConfOption, ConfValue) || {ConfOption, ConfValue} <- Config],
    tetrapak:require("tetrapak:startapp"),
    reloader:start(),
    tetrapak:require("shell");

run("build:erlang", _) ->
    App = appname(),
    OutDir = tetrapak:path("ebin"),
    tpk_file:mkdir(OutDir),

    %% load the boss reload module
    reloader:start(),
    %% put boss into devel mode
    application:set_env(boss, developing_app, App),

    TranslatorPid = boss_translator:start([{application, App}]),
    case catch boss_load:load_all_modules(App, TranslatorPid, OutDir) of
        {ok, AllModules} ->
            [output(replace_atom(Name), Modules) || {Name, Modules} <- AllModules],
            done;
        {'EXIT', Error} ->
            io:format("failed to load: ~p~n", [Error]),
            tetrapak:fail()
    end;

run("build:lang", _) ->
    tetrapak:require("build:erlang"),
    boss_lang:update_po(tetrapak:get("config:appfile:name")),
    done.

replace_atom(Atom) ->
    String = atom_to_list(Atom),
    replace(String, "_", " ").

replace(String, [From], [To]) ->
    lists:reverse(lists:foldl(fun(FromA, Acc) when (From == FromA) -> [To | Acc];
                                 (A, Acc)  -> [A | Acc]
                              end, "", String)).

%output(_Name, [])                           -> ok;
output(_Name, Modules) when is_list(Modules) ->
    [io:format("Compiled ~s~n", [Module]) || Module <- Modules].

appname() ->
    [AppSrc] = filelib:wildcard(filename:join(tetrapak:path("src/"), "*.app.src")),
    Name = filename:basename(AppSrc, ".app.src"),
    list_to_atom(Name).
