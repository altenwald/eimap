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

-module(erl_imap_utils_tests).
-include_lib("eunit/include/eunit.hrl").

extract_path_from_uri_test_() ->
    Data =
    [
        { <<"user/john.doe/Calendar@example.org">>,
          none, "/",
          <<"imap://john.doe@example.org@kolab.example.org/Calendar;UIDVALIDITY=1424683684/;UID=1">> },
        { <<"user/john.doe/Personal Calendar@example.org">>,
          none, "/",
          <<"imap://john.doe@example.org@kolab.example.org/Personal%20Calendar;UIDVALIDITY=1424683684/;UID=1">> }
    ],
    lists:foldl(fun({ Val, SharePrefix, Sep, Input }, Acc) -> [?_assert(Val == erl_imap_utils:extract_path_from_uri(SharePrefix, Sep, Input))|Acc] end,
                [], Data).

extract_uid_from_uri_test_() ->
    Data =
    [
        { <<"1">>, <<"imap://john.doe@example.org@kolab.example.org/Calendar;UIDVALIDITY=1424683684/;UID=1">> },
        { <<"12">>, <<"imap://john.doe@example.org@kolab.example.org/Calendar;UIDVALIDITY=1424683684/;UID=12">> },
        { <<"123">>, <<"imap://john.doe@example.org@kolab.example.org/Calendar;UIDVALIDITY=1424683684/;UID=123">> },
        { <<"1">>, <<"imap://john.doe@example.org@kolab.example.org/Calendar;UIDVALIDITY=1424683684/;UID=1;foo">> },
        { <<"12">>, <<"imap://john.doe@example.org@kolab.example.org/Calendar;UIDVALIDITY=1424683684/;UID=12;foo=bar">> },
        { <<"123">>, <<"imap://john.doe@example.org@kolab.example.org/Calendar;UIDVALIDITY=1424683684/;UID=123;foo=bar">> }
    ],
    lists:foldl(fun({ Val, Input }, Acc) -> [?_assert(Val == erl_imap_utils:extract_uidset_from_uri(Input))|Acc] end,
                [], Data).

