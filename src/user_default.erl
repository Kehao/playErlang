-module(user_default).
-compile(export_all).

z(File) ->  
  Out=compile:file(["./src/", atom_to_list(File),".erl"], 
    [{outdir,"./ebin/"}, {i,"./src/"},report,debug_info ]),            
  lm(),            
  Out.

x(File) ->    
  Out=compile:file(["./test/", atom_to_list(File),".erl"], 
    [ {outdir,"./ebin/"}, {i,"./src/"},report,debug_info ]),    
  lm(),    
  Out.

s(App) ->    
  application:start(App).

r(App) ->    
  application:stop(App),                                                
  %    os:cmd("ebin ./src/*.erl -I ./include/ +debug_info"),    
  make:all(),    
  make:all([load]),    
  lm(),    
  application:start(App).

%% @spec ensure_started(App::atom()) -> ok
%% @doc Start the given App if it has not been started already.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


zz(File) ->    
  ?MODULE:z(File),    
  File:start().

xx(File) ->    
  ?MODULE:x(File),    
  File:start().

rbb() ->    
  rb:start([{max,20}]).

rbb(Num) ->    rb:start([{max,Num}]).

rbl() ->    rb:list().

rbs(Num) ->    
  rb:show(Num).

test() ->    
  reloader:reload_modules(reloader:all_changed()).

ll() ->
  make:all(),    
  lm().
lm() ->    [c:l(M) || M <- mm()].
mm() ->    modified_modules().
modified_modules() ->    
  [M || {M,_} <- code:all_loaded(), module_modified(M) == true].
module_modified(Module) ->
  case code:is_loaded(Module) of
    {file,preloaded} ->
      false;
    {file,Path} ->
      CompileOpts = proplists:get_value(compile,Module:module_info()),
      CompileTime = proplists:get_value (time,CompileOpts),            
      Src = proplists:get_value (source,CompileOpts),            
      module_modified (Path,CompileTime,Src);
    _ -> false    
  end.

module_modified(Path,PrevCompileTime,PrevSrc) ->
  case find_module_file(Path) of
    false ->            false;
    ModPath -> case beam_lib:chunks(ModPath, ["CInf"]) of  
        {ok, {_, [{_, CB}]}} ->
          CompileOpts = binary_to_term(CB),
          CompileTime = proplists:get_value(time,CompileOpts),
          Src = proplists:get_value(source,CompileOpts),
          not (CompileTime == PrevCompileTime) and (Src == PrevSrc);
        _ -> false                   
      end    
  end.

find_module_file(Path) ->
  case file:read_file_info(Path) of        
    {ok,_} ->            Path;
    _ -> % maybe path was changed            
      case code:where_is_file(filename:basename(Path)) of                
        non_existing ->  false;
        NewPath -> NewPath            
      end    
  end.


%% Reload code
reload() ->
    LibExclude = base_lib_path(),
    Modules = [M || {M, P} <- code:all_loaded(), is_list(P) andalso string:str(P, LibExclude) =:= 0],
    [shell_default:l(M) || M <- Modules].

%% Reload code then exec F
reload_then(F) ->
    reload(),
    F().

%% Compiles all files in Emakefile and load into current shell.
sync() ->
    make:all([load]).

%% Run the make command in shell.
make() ->
    run_command(["make", "all"]).

%% Run git command in shell.
git(Command) ->
    CommandList = ["git", Command],
    run_command(CommandList).

dbg()                           -> dbg:tracer().

dbg(c)                          -> dbg:stop_clear();
dbg(M)                          -> dbge({M, '_', '_'}, []).

dbg(M, c)                       -> dbgc({M, '_', '_'});
dbg(M, r)                       -> dbge({M, '_', '_'}, dbg_rt());
dbg(M, l)                       -> dbgl({M, '_', '_'}, []);
dbg(M, lr)                      -> dbgl({M, '_', '_'}, dbg_rt());
dbg(M, F) when is_atom(F)       -> dbge({M,   F, '_'}, []);
dbg(M, O)                       -> dbge({M, '_', '_'}, O).

dbg(M, F, c)                    -> dbgc({M,   F, '_'});
dbg(M, F, l)                    -> dbgl({M,   F, '_'}, dbg_rt());
dbg(M, F, r)                    -> dbge({M,   F, '_'}, dbg_rt());
dbg(M, F, lr)                   -> dbgl({M,   F, '_'}, dbg_rt());
dbg(M, F, A) when is_integer(A) -> dbge({M,   F,   A}, []);
dbg(M, F, O)                    -> dbge({M,   F, '_'}, O).

dbg(M, F, A, c)                 -> dbgc({M,   F,   A});
dbg(M, F, A, r)                 -> dbge({M,   F,   A}, dbg_rt());
dbg(M, F, A, l)                 -> dbgl({M,   F,   A}, dbg_rt());
dbg(M, F, A, lr)                -> dbgl({M,   F,   A}, dbg_rt());
dbg(M, F, A, O)                 -> dbge({M,   F,   A}, O).

tc_avg(M, F, A, N) when N > 1 ->
    L = tl(lists:reverse(tc_loop(M, F, A, N, []))),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
              "Median: ~b mics~n"
              "Average: ~b mics~n",
              [Min, Max, Med, Avg]),
    Med.

tc_loop(_M, _F, _A, 0, List) ->
    List;
tc_loop(M, F, A, N, List) ->
    case timer:tc(M, F, A) of
        {_T, {'EXIT', Reason}} -> exit(Reason);
        {T, _Result} -> tc_loop(M, F, A, N - 1, [T|List])
    end.

%%%% Private Functions

run_command(CommandList) ->
    Result = os:cmd(string:join(CommandList, " ")),
    io:format("~s~n", [Result]).

dbgc(MFA)    -> dbg:ctp(MFA).
dbge(MFA, O) -> dbg:tracer(), dbg:p(all, call), dbg:tp(MFA, O).
dbgl(MFA, O) -> dbg:tracer(), dbg:p(all, call), dbg:tpl(MFA, O).
dbg_rt() -> x.

base_lib_path() ->
    KernAppPath = code:where_is_file("kernel.app"),
    string:substr(KernAppPath, 1, string:str(KernAppPath,"kernel") - 1).


words(Words) ->
    lists:flatten([0|| _X <- lists:seq(1, Words div 2)]).

substrings(S) -> 
    Slen = length(S),
    [string:sub_string(S,B,E) || 
        B <- lists:seq(1, Slen), E <- lists:seq(B, Slen)].

get_file_lines(F) ->
    get_file_lines_acc(F, []).
get_file_lines_acc(F, Acc) ->
    case file:read_line(F) of
        eof ->
            Acc;
        {ok, Data} ->
            get_file_lines_acc(F, [Data | Acc])
    end.

p(S, S2) ->
    io:format("~n~p:~n~p~n", [S, S2]).

int_to_month(Int) ->
    lists:nth(Int, [jan, feb, mar, apr, may, jun, jul, aug,
                    sep, oct, nov, dec]).

fat_processes() ->
    fat_processes(10).
fat_processes(C) ->
    L = lists:sort([{proplists:get_value(heap_size,L),
                     P,
                     proplists:get_value(initial_call, L),
                     proplists:get_value(current_function, L)}
                    ||{L,P} <- [{process_info(P), P} || P <- processes()]]),
    lists:reverse(lists:nthtail(length(L) - C, L)).

t(Mod, Fun, Args) ->
    dbg:stop_clear(),
    dbg:start(),
    dbg:tracer(),
    dbg:p(all,c),
    dbg:tpl(Mod, Fun, Args).

intervals(Start, Stop, _) when Start > Stop ->
    [];
intervals(Start, Stop, N) when Start == Stop; (Start + N) > Stop ->
    [{Start, Stop}];
intervals(Start, Stop, N) ->
    [{Start, Start + N} | intervals(Start + N + 1, Stop, N)].

