%% Copyright 2014 Kolab Systems AG (http://www.kolabsys.com)
%%
%% Aaron Seigo (Kolab Systems) <seigo a kolabsys.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(eimap_uidset_tests).
-include_lib("eunit/include/eunit.hrl").

% c("test/eimap_uidset_tests.erl"). eunit:test(eimap_uidset).

new_test_() ->
    Data =
    [
        % input, output
        { [4], <<"4">> },
        { [1, 2, 3, 4], <<"1,2,3,4">> },
        { [-1, 1, 2, 3, 4], <<"1,2,3,4">> },
        { [1, {100, 10}, 2, 3, 4], <<"1,10:100,2,3,4">> },
        { [1, {10, 100}, 2, 3, 4], <<"1,10:100,2,3,4">> },
        { [1, {-10, 100}, 2, 3, 4], <<"1,2,3,4">> },
        { [1, {-100, 10}, 2, 3, 4], <<"1,2,3,4">> },
        { [1, {-10, 100}, 2, 3, 4], <<"1,2,3,4">> },
        { [1, {"alpha", 100}, <<"random_binary">>, some_atom, 2, 3, 4], <<"1,2,3,4">> },
        { <<"1,2,3,4">>, <<>> },
        { other, <<>> },
        { [], <<>> }
    ],
    lists:foldl(fun({ List, Binary }, Acc) -> [?_assertEqual(Binary, eimap_uidset:uid_list_to_binary(List))|Acc] end, [], Data).

single_value_test_() ->
    [
        ?_assertEqual([1], iterate_uidset(eimap_uidset:parse(<<"1,">>))),
        ?_assertEqual([1], iterate_uidset(eimap_uidset:parse(<<"1">>))),
        ?_assertEqual([20], iterate_uidset(eimap_uidset:parse(<<"20">>))),
        ?_assertEqual([147], iterate_uidset(eimap_uidset:parse(<<"147">>)))
    ].

multiple_single_value_test_() ->
    [
        ?_assertEqual([1, 2], iterate_uidset(eimap_uidset:parse(<<"1,2">>)))
    ].

range_test_() ->
    [
        ?_assertEqual([1, 2, 3], iterate_uidset(eimap_uidset:parse(<<"1:3">>))),
        ?_assertEqual([3, 2, 1], iterate_uidset(eimap_uidset:parse(<<"3:1">>))),
        ?_assertEqual([1], iterate_uidset(eimap_uidset:parse(<<"1:1">>)))
    ].

multiple_range_test_() ->
    [ ?_assertEqual([1, 2, 3, 10, 11, 12, 13, 14, 15], iterate_uidset(eimap_uidset:parse(<<"1:3,10:15">>))) ].

mix_single_and_range_test_() ->
    [
        ?_assertEqual([1, 3, 4, 5], iterate_uidset(eimap_uidset:parse(<<"1,3:5">>))),
        ?_assertEqual([1, 3, 4, 5, 10], iterate_uidset(eimap_uidset:parse(<<"1,3:5,10">>))),
        ?_assertEqual([1, 3, 4, 5, 10, 20, 21, 22, 23, 30], iterate_uidset(eimap_uidset:parse(<<"1,3:5,10,20:23,30">>)))
    ].

mix_single_and_range_with_whitespace_test_() ->
    [
        ?_assertEqual(badarg, eimap_uidset:parse(<<"1, 3:5">>)),
        ?_assertEqual(badarg, eimap_uidset:parse(<<"1,3:5 ,10">>)),
        ?_assertEqual(badarg, eimap_uidset:parse(<<"1,3 :5,10,20:  23,30">>))
    ].

bad_uidsets_test_() ->
    [
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<>>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"-1,3:5">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"alpha">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"1,a,3:5">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"1,3:5,a">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"1;2">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"1, 2">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"1:3:5">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"-11,3:5">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"11,-3:5">>))),
        ?_assertEqual(badarg, iterate_uidset(eimap_uidset:parse(<<"11,3:-5">>)))
    ].


iterate_uidset(badarg) ->
    badarg;
iterate_uidset(UidSet) ->
    %%io:fwrite("we are going to iterate over ~p~n", [UidSet]),
    lists:reverse(iterate_uidset(eimap_uidset:next_uid(UidSet), [])).
iterate_uidset({ none, _UidSet }, Acc) -> Acc;
iterate_uidset({ Uid, UidSet }, Acc) -> iterate_uidset(eimap_uidset:next_uid(UidSet), [Uid|Acc]).

