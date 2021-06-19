-module(queries).
-author("Daniel Jodłoś").

-export([
    check_permissions/2
    ]).

-spec check_permissions(From :: binary(), To :: binary()) -> binary().
check_permissions(From, To) ->
    case graph:get_edge(From, To) of
        {ok, #{<<"Permissions">> := Permissions}} -> Permissions;
        {error, no_connection} -> 
            error(no_connection);
        {error, _} ->
            {ok, Children} = graph:list_children(From),
            Results = lists:map(fun (Child) -> check_permissions(Child, To) end, Children),
            Permissions_To_Children = lists:map(fun ({Child, FurtherPerms}) -> 
                {ok, #{<<"Permissions">> := Perms}} = graph:get_edge(From, Child),
                gmm_utils:permissions_and(Perms, FurtherPerms)
            end, lists:zip(Children, Results)),
            lists:foldl(fun gmm_utils:permissions_or/2, <<"00000">>, Permissions_To_Children)
    end.