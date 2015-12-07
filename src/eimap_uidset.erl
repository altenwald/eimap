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

-module(eimap_uidset).
-export([parse/1, next_uid/1]).

%% tokens have the form: integer | { integer, integer }
-record(uidset, { tokens = [], current = none }).

%% currently do not support parsing regular (list) strings, but could be added
parse(UidSet) when is_binary(UidSet) ->
    Components = binary:split(UidSet, <<",">>, [global]),
    try lists:foldl(fun(Component, Acc) -> add_component(binary:split(Component, <<":">>), Acc) end, [], Components) of
        Parsed -> #uidset{ tokens = lists:reverse(Parsed) }
    catch
        _:_ -> badarg
    end.

add_component(Uid, Acc) when is_integer(Uid), Uid >= 0 -> [Uid|Acc];
add_component([First, Second], Acc) -> add_component(binary_to_integer(First), binary_to_integer(Second), Acc);
add_component([<<"">>], Acc) -> Acc;
add_component([Single], Acc) when is_binary(Single) -> add_component(binary_to_integer(Single), Acc);
add_component(_, _Acc) -> throw(badarg).

add_component(First, Second, Acc) when is_integer(First), First >= 0, is_integer(Second), Second >= 0 -> add_range(First, Second, Acc);
add_component(_, _, _Acc) -> throw(badarg).

add_range(First, Second, Acc) when First == Second -> [First|Acc];
add_range(First, Second, Acc) -> [{ First, Second }|Acc].

next_uid(#uidset{ tokens = Tokens, current = Current }) -> next_uid(Tokens, Current).
next_uid([], _Current) -> { none, #uidset {} };
next_uid([{ First, Second }|_] = FullList, Current) -> next_in_range(FullList, First, Second, Current);
next_uid([Uid|UidSet], _Current) -> { Uid, #uidset{ tokens = UidSet } }.

next_in_range(Tokens, First, _Second, none) -> { First, #uidset{ tokens = Tokens, current = First } };
next_in_range(Tokens, First, Second, Current) when First < Second, Current < Second -> Next = Current + 1, { Next, #uidset{ tokens = Tokens, current = Next } };
next_in_range(Tokens, First, Second, Current) when First > Second, Current > Second -> Next = Current - 1, { Next, #uidset{ tokens = Tokens, current = Next } };
next_in_range([_|Tokens], _First, _Second, _Current) -> next_uid(#uidset{ tokens = Tokens }).
