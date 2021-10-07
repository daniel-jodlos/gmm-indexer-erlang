%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2021 15:22
%%%-------------------------------------------------------------------
-module(rest_utils).
-author("piotr").

%% API
-export([replace/3]).

-spec replace(T, T, [T]) -> [T].
replace(Element, Replacement, [Element | T]) ->
  [Replacement | T];
replace(Element, Replacement, [H | T]) ->
  [H | replace(Element, Replacement, T)];
replace(_Element, _Replacement, []) ->
  [].