-module(vendorize).

%% API exports
-export([main/1]).

-compile(export_all).

-record(dep, {
          name,
          engine,
          rev,
          url,
          %% has the revision of this dep changed i.e. is dirty, needs
          %% push
          changed=false :: boolean()
         }).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
vendorize(RootDir, Config) ->
    DAG = digraph:new([acyclic]),
    Proj = filename:basename(RootDir),
    Root = digraph:add_vertex(DAG, Proj, "root"),
    {ok, RebarConf} = file:consult(filename:join([RootDir, "rebar.config"])),
    Deps = proplists:get_value(deps, RebarConf, []),
    DepDir = filename:join([RootDir, "deps"]),
    build_dag(Deps, DAG, Root, DepDir, Config),
    dag_to_dot(DAG, Proj).

build_dag([], DAG, _Parent, _Dir, _Config) ->
    DAG;
build_dag([Dep | Deps], DAG, Parent, Dir, Config) ->
    {Name, _Version, {_Engine, _Loc, Rev}=_Src} = Dep,
    %% the _expected_ Rev, 'cos the actual rev at `Name' may be
    %% different if there are multiple conflicting revs for this dep
    %% we should only care about the real `Rev', I wonder what this
    %% means for rebar lock files?
    %% TODO handle actual rev on disk labelling when visiting dep
    %% TODO: call git to get _real_ rev here for label
    MyVertex = maybe_add_vertex(DAG, Name, Rev),
    %% Label the edge with the expected `Rev', though the vertex may
    %% update it's labelled `Rev', the edge will keep it's label
    maybe_add_edge(DAG, Parent, MyVertex, Rev),
    MyDir = filename:join([Dir, Name]),
    NewRev = case file:consult(filename:join([MyDir, "rebar.config"])) of
                 {ok, RebarConf} ->
                     MyDeps = proplists:get_value(deps, RebarConf, []),
                     %% Depth first
                     build_dag(MyDeps, DAG, MyVertex, Dir, Config),
                     %% vendorize here: all of MyDeps have been vendorized, so I can
                     %% update my rebar.config with the new information, then possibly
                     %% commit, and thus add a new rev info to myself
                     {ok, IsConfChanged} = rewrite_rebar_conf(MyDeps, RebarConf, MyDir, Config),
                     IsConfChanged;
                 {error,enoent} ->
                     false
             end,

    ok = vendorize_deps(Dep, NewRev, Config),

    %% then breadth
    build_dag(Deps, DAG, Parent, Dir, Config).

rewrite_rebar_conf([], _RebarConf, _MyDir, _Config) ->
    %% no deps to change, so no need to re-write config
    {ok, false};
rewrite_rebar_conf(MyDeps, RebarConf, MyDir, Config) ->
    %% @TODO what about revisions? New revisions?
    NewDeps = redrum:remap_deps(MyDeps, Config),
    case NewDeps /= MyDeps of
        true ->
            ok = redrum:rewrite_config(NewDeps, MyDir, RebarConf, []),
            {ok, true};
        false ->
            {ok, false}
    end.

%% change _my_ origin, maybe make a new branch for the sake of tagging
vendorize(Dep, IsConfChanged, Config) ->
    %% Called after the config file is re-written, so a branch will be
    %% checked out. Maybe change the origin, commit, get the new rev,
    %% don't push.
    {Name, _VSN, {_Engine, _Loc, Rev}=_Src} = Dep,
    {Name, _VSN, {NewEngine, NewLoc, _Rev}=_NewSrc} = redrum:remap_dep(Dep, Config),
    NewRev = maybe_new_rev(Name, Rev, Config, IsConfChanged),
    maybe_add_new_remote(Name, NewEngine, NewLoc),
    put(Name, #dep{name=Name, engine=NewEngine, rev=NewRev, url=NewLoc, changed=IsConfChanged}).

maybe_new_rev(Name, Rev, Config, _RebarConfChange=true) ->
    %% 

dag_to_dot(DAG, Name) ->
    File = Name ++ ".dot",
    {ok, IODevice} = file:open(File, [write]),
    io:format(IODevice, "~s ~s {~n", [digraph, Name ]),
    Edges = digraph:edges(DAG),
    lists:foreach(
      fun(E) ->
              {E, Src, Trgt, Label} = digraph:edge(DAG, E),
              io:format(IODevice, "  ~s -> ~s [ label=\"~p\"];~n",[Src, Trgt, label_to_string(Label)])
      end,
      Edges
     ),

    io:format(IODevice, "}~n", []),
    file:close(IODevice),
    DAG.


maybe_add_vertex(DAG, Dep, _Label) ->
    case digraph:vertex(DAG, Dep) of
        false ->
            digraph:add_vertex(DAG, Dep);
        {V, _L} ->
            %% TODO handle labelling
            V
    end.

maybe_add_edge(DAG, V1, V2, Rev) ->
    Edges = digraph:edges(DAG, V1),
    Action =
        try
            lists:foldl(fun(E, _A) ->
                                case digraph:edge(DAG, E) of
                                    {Edge, V1, V2, _} ->
                                        throw({exists, Edge});
                                     _ ->
                                         add
                                 end
                         end,
                         add,
                         Edges)
        catch throw:{exists, Edge1} -> {exists, Edge1}
        end,

    case Action of {exists, Edge} ->
            Edge;
        add -> digraph:add_edge(DAG, V1, V2, Rev)
    end.

label_to_string({tag, Label}) when is_list(Label) ->
    {tag, list_to_atom(Label)};
label_to_string(Label) when is_list(Label) ->
    list_to_atom(Label).
