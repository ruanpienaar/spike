-module(spike).
-export([
    connect_and_do/4,
    do_inject/3,
    inject/2,
    do_purge/3,
    purge/2,
    hard_purge/2
]).

-spec connect_and_do(atom(), atom(), inject | purge, list(atom())) -> ok.
connect_and_do(Node, Cookie, Action, Modules) ->
    % For Escript
    {ok, _} = application:ensure_all_started(hawk),
    HangingPid = self(),
    ActionFun = fun() ->
        case Action of 
                 inject ->
                     io:format("Injecting ...~n", []),
                     do_inject(HangingPid, Node, Modules);
                 purge ->
                     io:format("Purging ...~n", []),
                     do_purge(HangingPid, Node, Modules)
        end
    end,
    case hawk:add_node(
         Node, 
         Cookie, 
         [{connect, fun() ->
             io:format("Node Connected!~n"),
             ActionFun
         end}], 
         [{disconnect, fun() ->
                io:format("Node Disconnected while attempting ~p\n", [Action]) 
         end}]
    ) of
      {ok, P} -> 
        true = erlang:link(P)
    end,
    receive
        done ->
            io:format("~p Done ... \n", [Action]),
            ok = hawk:remove_node(Node),
            timer:sleep(50)
        after 2500 ->
            io:format("~p Timeout ... Closing ...\n", [Action]),
            ok = hawk:remove_node(Node),
            timer:sleep(50)
    end.

do_inject(P, Node, Mods) when is_list(Mods) ->
    [ok = inject(Node, M) || M <- Mods],
    io:format("Done injecting.............", []),
    P ! done.

do_purge(P, Node, Mods) ->
    [ok = purge(Node, M) || M <- Mods],
    io:format("Done injecting.............", []),
    P ! done.

inject(Node, Module) ->
    case code:get_object_code(Module) of
        {Module, Bin, File} ->
            case rpc:call(Node, code, load_binary,
                          [Module, File, Bin]) of
                {module, Module} ->
                    io:format("Loaded ~p on ~p~n", [Module, Node]);
                {Error, Reason} when Error =:= error;
                                     Error =:= badrpc ->
                    {error, {load_binary_failed, Reason}}
            end;
        error ->
            {error, {get_object_code_failed, Module}}
    end.
    
purge(Node, Module) ->
    Res = try rpc:call(Node, code, soft_purge, [Module]) of
              true ->
                  ok;
              false ->
                  hard_purge(Node, Module);
              {badrpc, _} = RPCError ->
                  {error, RPCError}
          catch
              C:E ->
                  {error, {C,E}}
          end,
    case Res of
        ok ->
            case rpc:call(Node, code, delete, [Module]) of
                true ->
                    io:format("Purged ~p on ~p~n", [Module, Node]);
                false ->
                    io:format("Could not purged ~p from ~p~n", [Module, Node])
            end;
        {error, Error} ->
            io:format("Error while purging  ~p from ~p: ~p~n", [Module, Node, Error])
    end.

hard_purge(Node, Module) ->
    try rpc:call(Node, code, purge, [Module]) of
        true ->
            ok;
        false ->
           io:format("Could not code:purge ~p~n", [Module]);
        {badrpc, _} = RPCError ->
            {error, RPCError}
    catch
        C:E ->
            {error, {C,E}}
    end.
    