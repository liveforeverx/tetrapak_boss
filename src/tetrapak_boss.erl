-module(tetrapak_boss).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([check/1, run/2]).
-export([appname/0]).

-define(LANG_JSON_DIR, tetrapak:path("priv/static/lang")).
-define(LANG_PO_DIR, tetrapak:path("translations")).
% --------------------------------------------------------------------------------------------------
% -- tetrapak callbacks
app() ->
    Check = build_app(),
    {Check, Check}.

build_app() ->
    filelib:is_dir(tetrapak:path("src/controller")) orelse
    filelib:is_dir(tetrapak:path("src/model")) orelse
    filelib:is_dir(tetrapak:path("src/view")).


tasks(tasks) ->
    case build_app() of
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
     {"build:erlang", "compile Erlang modules using ChicagoBoss"},
     {"clean:lang", "Remove gettext JSON files"}].

% --------------------------------------------------------------------------------------------------
% -- Plugin tasks

check("build:lang") ->
    tpk_util:check_files_mtime(?LANG_PO_DIR, "po", ?LANG_JSON_DIR, "json").

run("start:dev", _) ->
    io:format(user, "info: ~p~n", [ok]),
    tpk_file:mkdir(tetrapak:path("log")),
    Config =
        case file:consult(filename:join(tetrapak:dir(), "tetrapak/boss.config")) of
            {ok, RawConfig} ->
                %% Use enit or Chicago Boss configuration style
                lists:flatten(RawConfig);
            {error, enoent} ->
                []
        end,
    [set_configuration(App, Configuration) || {App, Configuration} <- [{boss, default_config()}] ++ Config],
    tetrapak:require("tetrapak:startapp"),
    tetrapak:require("shell");

run("build:erlang", _) ->
    App = appname(),
    OutDir = tetrapak:path("ebin"),

    set_dev_mode(App),

    {ok, BossVersion} = application:get_key(boss, vsn),
    case BossVersion > "0.8.1" of
        true -> {ok, TranslatorPid} = boss_translator_sup:start_link([{application, App}]);
        % Support for old boss versions
        false -> TranslatorPid = boss_translator:start([{application, App}])
    end,

    Result = case catch boss_load:load_all_modules(App, TranslatorPid, OutDir) of
                 {ok, AllModules} ->
                     [output(replace_atom(Name), Modules) || {Name, Modules} <- AllModules],
                     done;
                 {'EXIT', Error} ->
                     io:format("failed to load: ~p~n", [Error]),
                     tetrapak:fail()
             end,
    unset_dev_mode(),
    Result;

run("build:lang", _) ->
    tetrapak:require("build:erlang"),
    tpk_file:mkdir(tetrapak:path("priv/lang")),
    boss_lang:update_po(tetrapak:get("config:appfile:name")),
    done;

run("clean:lang", _) ->
    tpk_file:delete(?LANG_JSON_DIR).

% --------------------------------------------------------------------------------------------------
% -- helpers

set_configuration(boss, Configuration) ->
    application:load(boss),
    App = appname(),
    Config = [{applications, [App]} | Configuration],
    [application:set_env(boss, ConfOption, ConfValue) || {ConfOption, ConfValue} <- Config];
set_configuration(App, Config) ->
    application:load(App),
    [application:set_env(App, ConfOption, ConfValue) || {ConfOption, ConfValue} <- Config].

default_config() ->
    [{db_host, "localhost"},
     {db_port, 1978},
     {db_adapter, mock},
     {log_dir, "log"},
     {server, mochiweb},
     {port, 8001},
     {session_adapter, mock},
     {session_key, "_boss_session"},
     {session_exp_time, 525600}].

replace_atom(Atom) ->
    String = atom_to_list(Atom),
    replace(String, "_", " ").

replace(String, [From], [To]) ->
    lists:reverse(lists:foldl(fun(FromA, Acc) when (From == FromA) -> [To | Acc];
                                 (A, Acc)  -> [A | Acc]
                              end, "", String)).

output(_Name, Modules) when is_list(Modules) ->
    [io:format("Compiled ~s~n", [Module]) || Module <- Modules].

appname() ->
    [AppSrc] = filelib:wildcard(filename:join(tetrapak:path("src/"), "*.app.src")),
    Name = filename:basename(AppSrc, ".app.src"),
    list_to_atom(Name).

set_dev_mode(App) ->
    application:load(boss),
    %% load the boss reload module
    reloader:start(),
    %% put boss into devel mode
    application:set_env(boss, developing_app, App).

unset_dev_mode() ->
    %% load the boss reload module
    reloader:stop(),
    %% put boss into devel mode
    application:unset_env(boss, developing_app).
