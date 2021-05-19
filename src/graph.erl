-module(graph).
-author("Daniel Jodłoś").

-export([
    get_user/1, 
    get_users/0, 
    add_user/1, 
    delete_user/1, 
    update_user/2,
    add_user_to_group/2,
    is_member_of_group/2,
    list_group_users/1,
    remove_user_from_group/2
]).

get_user(Id) ->
    persistence:get(Id).

get_users()->
    persistence:keys("*").

add_user(Name) ->
    Id = << <<Y>> ||<<X:4>> <= crypto:hash(md5, term_to_binary(make_ref())), Y <- integer_to_list(X,16)>>,
    Json = json_utils:encode(#{<<"id">> => Id, <<"Name">> => Name}),
    Response = persistence:set(Id, Json),
    case Response of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> {ok, Id}
    end.

update_user(Id, NewName) ->
    Json = json_utils:encode(#{<<"id">> => Id, <<"Name">> => NewName}),
    Response = persistence:set(Id, Json),
    case Response of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.

delete_user(Id) ->
    Response = persistence:del(Id),
    case Response of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.

children_key(Key) when is_binary(Key) == true ->
    <<Key/binary, "/children">>;
children_key(Key) -> children_key(binary:list_to_bin(Key)).


parents_key(Key) when is_binary(Key) == true ->
    <<Key/binary, "/parents">>;
parents_key(Key) -> parents_key(binary:list_to_bin(Key)).


add_user_to_group(User, Group) ->
    persistence:set_add(children_key(Group), User),
    persistence:set_add(parents_key(User), Group).

remove_user_from_group(User, Group) ->
    persistence:set_remove(children_key(Group), User),
    persistence:set_remove(parents_key(User), Group).

is_member_of_group(User, Group) ->
    {ok, Result} = persistence:set_is_member(children_key(Group), User),
    Result.

list_group_users(Group) ->
    {ok, Result} = persistence:set_list_members(children_key(Group)),
    Result.
