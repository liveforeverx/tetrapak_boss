-module(tetrapak_boss).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([check/1, run/2]).
-export([appname/0]).
-import(filename, [join/1]).
-define(LANG_JSON_DIR, tetrapak:path(join(["priv", "static", "lang"]))).
-define(LANG_PO_DIR, tetrapak:path("translations")).
-define(CONT_DIR, join(["src", "controller"])).
-define(MODEL_DIR, join(["src", "model"])).
-define(VIEW_DIR, join(["src", "view"])).

% --------------------------------------------------------------------------------------------------
% -- tetrapak callbacks
app() ->
    Check = build_app(),
    {Check, Check}.

build_app() ->
    filelib:is_dir(tetrapak:path(?CONT_DIR)) orelse
    filelib:is_dir(tetrapak:path(?MODEL_DIR)) orelse
    filelib:is_dir(tetrapak:path(?VIEW_DIR)).

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
     {"build:appfile", "Generate the application resource file"},
     {"clean:lang", "Remove gettext JSON files"}].

% --------------------------------------------------------------------------------------------------
% -- Plugin tasks

check("build:appfile") ->
    tetrapak_task_appsrc:check("build:appfile");

check("build:lang") ->
    tpk_util:check_files_mtime(?LANG_PO_DIR, "po", ?LANG_JSON_DIR, "json").

run("build:appfile", AppSrc) ->
    case erlang:function_exported(tetrapak_task_appsrc, build_app_file, 2) of
        true -> tetrapak_task_appsrc:build_app_file(AppSrc, fun add_configuration/2);
        false -> build_app_file(AppSrc, fun add_configuration/2)
    end;

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
                     % {ok, [{modules, AllModules}]};
                     done;
                 {'EXIT', Error} ->
                     io:format("failed to load: ~p~n", [Error]),
                     tetrapak:fail()
             end,
    %unset_dev_mode(),
    Result;

run("build:lang", _) ->
    tetrapak:require("build:erlang"),
    tpk_file:mkdir(tetrapak:path(join(["priv", "lang"]))),
    boss_lang:update_po(tetrapak:get("config:appfile:name")),
    done;

run("clean:lang", _) ->
    tpk_file:delete(?LANG_JSON_DIR).

% --------------------------------------------------------------------------------------------------
% -- helpers

set_configuration(boss, Configuration) ->
    application:load(boss),
    App = appname(),
    Config = [{applications, [App]}, {developing_app, App} | Configuration],
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

add_configuration(AppName, Keys) ->
    Update = lists:zip([controller_modules, model_models], [modules(Dir) || Dir <- [?CONT_DIR, ?MODEL_DIR]]),
    ToUpdate = [{view_modules, template_modules(AppName)} | Update],
    Enviroments = proplists:get_value(env, Keys, []),
    NewEnviroments = lists:foldl(fun({Key, Value}, Envs) ->
                                         lists:keystore(Key, 1, Envs, {Key, Value})
                                 end, Enviroments, ToUpdate),
    lists:keystore(env, 1, Keys, {env, NewEnviroments}).

modules(Dir) ->
    Files = tpk_file:wildcard(tetrapak:path(Dir), "*.{erl,ex}"),
    [list_to_atom(filename:basename(Path, ".erl")) || Path <- Files].

template_modules(AppName) ->
    code:ensure_loaded(boss_files),
    case erlang:function_exported(boss_files, view_file_list, 0) of
        true ->
            ViewFiles = boss_files:view_file_list(),
            % TODO: better way to do it
            [template_module(AppName, Path) || Path <- ViewFiles];
        false ->
            []
    end.

template_module(AppName, Path) ->
    ViewPath = string:sub_string(Path, 4),
    ViewString = string:join(string:tokens(ViewPath, "/."), "_"),
    list_to_atom(atom_to_list(AppName) ++ "_" ++ ViewString).

% --------------------------------------------------------------------------------------------------
% -- TODO: remove duplication from

build_app_file(AppSrc, FunModifiedKeys) ->
    AppSrcDisplayPath = tpk_file:rebase_filename(AppSrc, tetrapak:dir(), ""),
    case file:consult(AppSrc) of
        {ok, [{application, AppName, Keys}]} ->
            case {atom_to_list(AppName), filename:rootname(filename:basename(AppSrc), ".app.src")} of
                {AppNameString, AppNameString} ->
                    ok;
                {AppNameString, _} ->
                    tetrapak:fail("application name in ~s (~s) does not match filename", [AppSrcDisplayPath, AppNameString])
            end,
            Vsn = get_app_vsn(AppSrcDisplayPath, proplists:get_value(vsn, Keys), tetrapak:config("build.version")),
            NewKeys1 = lists:keystore(vsn, 1, Keys, {vsn, Vsn}),
            NewKeys2 = lists:keystore(modules, 1, NewKeys1, {modules, get_app_modules()}),
            write_appfile(AppName, FunModifiedKeys(AppName, NewKeys2));
        {ok, _} ->
            tetrapak:fail("~s has invalid term structure", [AppSrcDisplayPath]);
        {error, Error} when is_atom(Error) ->
            tetrapak:fail("could not open ~s: ~s", [AppSrcDisplayPath, file:format_error(Error)]);
        {error, Error = {_Line, _Mod, _EInfo}} ->
            tpk_util:show_error_info(AppSrcDisplayPath, Error),
            tetrapak:fail()
    end.

get_app_vsn(_AppSrc, _AppVsn, CfgVsn) when is_list(CfgVsn) ->
    expand_vsn(CfgVsn);
get_app_vsn(_AppSrc, AppVsn, undefined) when is_list(AppVsn) ->
    expand_vsn(AppVsn);
get_app_vsn(_AppSrc, git, undefined) ->
    %% match what git describe outputs, for rebar compatibility
    expand_vsn("~T{~t}~O{-~o}~T{-g}~c~D{-dirty}");
get_app_vsn(_AppSrc, undefined, AppVsn) when is_list(AppVsn) ->
    expand_vsn(AppVsn);
get_app_vsn(AppSrc, undefined, undefined) ->
    io:format("neither ~s nor the config contain version information~n", [AppSrc]),
    tetrapak:fail("could not determine application version").

expand_vsn([]) ->
    [];
expand_vsn("~~" ++ R) ->
    "~" ++ expand_vsn(R);
expand_vsn("~t" ++ R) ->
    tetrapak:get("config:vcs:last_tag") ++ expand_vsn(R);
expand_vsn("~c" ++ R) ->
    tetrapak:get("config:vcs:commit") ++ expand_vsn(R);
expand_vsn("~o" ++ R) ->
    tetrapak:get("config:vcs:offset") ++ expand_vsn(R);
expand_vsn("~b" ++ R) ->
    tetrapak:get("config:vcs:branch") ++ expand_vsn(R);
expand_vsn("~d" ++ R) ->
    build_timestamp() ++ expand_vsn(R);
expand_vsn("~O{" ++ R) ->
    expand_condition(R, "config:vcs:offset", "0");
expand_vsn("~T{" ++ R) ->
    expand_condition(R, "config:vcs:last_tag", "");
expand_vsn("~D{" ++ R) ->
    expand_condition(R, "config:vcs:dirty", false);
expand_vsn([C | R]) ->
    [C | expand_vsn(R)].

expand_condition(Str, CheckKey, EmptyValue) ->
    case re:run(Str, "([^\\}]*)\\}(.*)", [{capture, all_but_first, list}]) of
        {match, [Insert, Rest]} ->
            case tetrapak:get(CheckKey) of
                EmptyValue ->
                    expand_vsn(Rest);
                _ ->
                    expand_vsn(Insert) ++ expand_vsn(Rest)
            end
    end.

build_timestamp() ->
    {{Y,Mo,D},{H,Min,S}} = calendar:universal_time(),
    tpk_util:f("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B", [Y, Mo, D, H, Min, S]).

get_app_modules() ->
    [list_to_atom(filename:rootname(F, ".beam")) || F <- filelib:wildcard("*.beam", tetrapak:path("ebin"))].

write_appfile(AppName, Keys) ->
    OutputFile = filename:join(tetrapak:path("ebin"), atom_to_list(AppName) ++ ".app"),
    file:write_file(OutputFile, io_lib:fwrite("{application, ~s,~n  ~p~n}.", [AppName, Keys])).
