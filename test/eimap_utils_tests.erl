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

-module(eimap_utils_tests).
-include_lib("eunit/include/eunit.hrl").

% c("test/eimap_utils_tests.erl"). eunit:test(eimap_utils).

extract_path_from_uri_test_() ->
    Data =
    [
        {
          <<"user/john.doe/Calendar@example.org">>,
          none, "/",
          <<"imap://john.doe@example.org@kolab.example.org/Calendar;UIDVALIDITY=1424683684/;UID=1">>
        },
        {
          <<"user/john.doe/Personal Calendar@example.org">>,
          none, "/",
          <<"imap://john.doe@example.org@kolab.example.org/Personal%20Calendar;UIDVALIDITY=1424683684/;UID=1">>
        },
        {
          <<"Personal Calendar">>,
          none, "/",
          <<"imap://kolab.example.org/Personal%20Calendar;UIDVALIDITY=1424683684/;UID=1">>
        },
        {
          <<"Personal Calendar">>,
          "Shared/", "/",
          <<"imap://kolab.example.org/Shared/Personal%20Calendar;UIDVALIDITY=1424683684/;UID=1">>
        },
        {
          <<"Personal Calendar">>,
          "Shared/", "/",
          <<"imap://kolab.example.org/Personal%20Calendar;UIDVALIDITY=1424683684/;UID=1">>
        },
        {
          <<"user/john.doe@example.org">>,
          "Shared/", "/",
          <<"imap://john.doe@example.org@kolab.example.org/INBOX;UIDVALIDITY=1424683684/;UID=1">>
        },
        {
          bad_uri,
          none, "/",
          <<"merf">>
        }
    ],
    lists:foldl(fun({ Val, SharePrefix, Sep, Input }, Acc) -> [?_assertEqual(Val, eimap_utils:extract_path_from_uri(SharePrefix, Sep, Input))|Acc] end,
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
    lists:foldl(fun({ Val, Input }, Acc) -> [?_assertEqual(Val, eimap_utils:extract_uidset_from_uri(Input))|Acc] end,
                [], Data).

split_command_into_components_test_() ->
    Data =
    [
        { { <<>>, <<>>, <<>> }, <<>> },
        { { <<".">>, <<"LIST">>, <<"\"\" \"*\"">> }, <<". LIST \"\" \"*\"">> },
        { { <<"1">>, <<"STARTTLS">>, <<>> }, <<"1 STARTTLS">> },
        { { <<"1">>, <<"STARTTLS">>, <<>> }, <<"1 STARTTLS\r\n">> },
        { { <<"3">>, <<"ID">>, <<"(\"name\" \"Thunderbird\" \"version\" \"38.3.0\")">> }, <<"3 ID (\"name\" \"Thunderbird\" \"version\" \"38.3.0\")">> }
    ],
    lists:foldl(fun({ Val, Input }, Acc) -> [?_assertEqual(Val, eimap_utils:split_command_into_components(Input)) | Acc] end,
                 [], Data).

check_response_for_failure_test_() ->
    Tag = <<"abcdef">>,
    Data =
    [
       { Tag, <<Tag/binary, " NO reasons\r\n">>, { no, <<"reasons">> } },
       { Tag, <<Tag/binary, " NO reasons">>, { no, <<"reasons">> } },
       { Tag, <<Tag/binary, " BAD reasons\r\n">>, { bad, <<"reasons">> } },
       { Tag, <<Tag/binary, " BAD reasons">>, { bad, <<"reasons">> } },
       { Tag, <<Tag/binary, " OK reasons">>, ok },
       { Tag, <<"short">>, ok },
       { undefined, <<"* OK reasons">>, ok }
    ],
    lists:foldl(fun({ Tag2, Input, Output}, Acc) -> [?_assertEqual(Output, eimap_utils:check_response_for_failure(Input, Tag2)) | Acc] end, [], Data).

is_tagged_response_test_() ->
    Tag = <<"abcd">>,
    Data =
    [
       { <<Tag/binary, " Indeed\r\n">>, tagged },
       { <<Tag/binary, " Indeed">>, tagged },
       { <<"one">>, untagged },
       { <<"* Yeah baby">>, untagged }
    ],
    lists:foldl(fun({ Input, Output}, Acc) -> [?_assertEqual(Output, eimap_utils:is_tagged_response(Input, Tag)) | Acc] end, [], Data).

remove_tag_from_response_test_() ->
    Tag = <<"abcd">>,
    Data =
    [
       { Tag, <<Tag/binary, " Indeed\r\n">>, check, <<"Indeed\r\n">> },
       { Tag, <<Tag/binary, " Indeed\r\n">>, trust, <<"Indeed\r\n">> },
       { Tag, <<Tag/binary, " Indeed">>, check, <<"Indeed">>},
       { Tag, <<Tag/binary, " Indeed">>, trust, <<"Indeed">>},
       { undefined, <<"abcd4 Indeed">>, check, <<"abcd4 Indeed">>},
       { undefined, <<"abcd4 Indeed">>, trust, <<"abcd4 Indeed">>},
       { <<>>, <<"abcd4 Indeed">>, check, <<"abcd4 Indeed">>},
       { <<>>, <<"abcd4 Indeed">>, trust, <<"abcd4 Indeed">>},
       { Tag, <<"abcd4 Indeed">>, check, <<"abcd4 Indeed">>},
       { Tag, <<"abcd4 Indeed">>, trust, <<" Indeed">>},
       { Tag, <<"* Yeah baby">>, check, <<"* Yeah baby">> },
       { Tag, <<"">>, check, <<"">> },
       { Tag, <<"">>, trust, <<"">> },
       { Tag, <<>>, check, <<>> },
       { Tag, <<>>, trust, <<>> }
    ],
    lists:foldl(fun({ Tag2, Input, Check, Output}, Acc) -> [?_assertEqual(Output, eimap_utils:remove_tag_from_response(Input, Tag2, Check)) | Acc] end, [], Data).

header_name_test_() ->
    Data =
    [
        { mailbox_uid ,  <<"/vendor/cmu/cyrus-imapd/uniqueid">> },
        { groupware_type ,  <<"X-Kolab-Type">> },
        { groupware_uid ,  <<"Subject">> },
        { dunno,  unknown },
        { "dunno",  unknown },
        { <<"dunno">>,  unknown },
        { 134,  unknown }
    ],
    lists:foldl(fun({ Input, Output}, Acc) -> [?_assertEqual(Output, eimap_utils:header_name(Input)) | Acc] end, [], Data).

ensure_binary_test_() ->
    Data = 
    [
        { "yep", <<"yep">> },
        { <<"yep">>, <<"yep">> },
        { [1, 2, 3], <<1, 2, 3>> },
        { yep, <<"yep">> },
        { 123, <<>> }
    ],
    lists:foldl(fun({ Input, Output}, Acc) -> [?_assertEqual(Output, eimap_utils:ensure_binary(Input)) | Acc] end, [], Data).

only_full_lines_test_() ->
    Data = 
    [
        { <<"yep">>, { <<>>, <<"yep">> } },
        { <<"yep\r\nhohoho">>, { <<"yep\r\n">>, <<"hohoho">> } },
        { <<"nope\r\nyep\r\nhohoho">>, { <<"nope\r\nyep\r\n">>, <<"hohoho">> } },
        { <<"nope\r\nyep\r\nhohoho\r\n">>, { <<"nope\r\nyep\r\nhohoho\r\n">>, <<>> } }
    ],
    lists:foldl(fun({ Input, Output}, Acc) -> [?_assertEqual(Output, eimap_utils:only_full_lines(Input)) | Acc] end, [], Data).

parse_flags_test_() ->
    Data =
    [
        { <<"()">>, [] },
        { <<>>, [] },
        { <<"\\\\Answered \\\\Flagged \\\\Draft \\\\Deleted \\\\Seen">>, [<<"\\\\Answered">>, <<"\\\\Flagged">>, <<"\\\\Draft">>, <<"\\\\Deleted">>, <<"\\\\Seen">> ] },
        { <<"(\\\\Answered \\\\Flagged \\\\Draft \\\\Deleted \\\\Seen)">>, [<<"\\\\Answered">>, <<"\\\\Flagged">>, <<"\\\\Draft">>, <<"\\\\Deleted">>, <<"\\\\Seen">> ] },
        { "(\\\\Answered \\\\Flagged \\\\Draft \\\\Deleted \\\\Seen)", [<<"\\\\Answered">>, <<"\\\\Flagged">>, <<"\\\\Draft">>, <<"\\\\Deleted">>, <<"\\\\Seen">> ] }
    ],
    lists:foldl(fun({ Input, Output}, Acc) -> [?_assertEqual(Output, eimap_utils:parse_flags(Input)) | Acc] end, [], Data).

num_literal_continuation_bytes_test_() ->
    Data =
    [
        { <<"abcd">>, { <<"abcd">>, 0 } },
        { <<"abcd{5}">>, { <<"abcd">>, 5 } },
        { <<"abcd{100}">>, { <<"abcd">>, 100 } },
        { <<"123abcd{100}">>, { <<"123abcd">>, 100  } },
        { <<"ab{123abcd{100}">>, { <<"ab{123abcd">>, 100  } },
        { <<"ab{123abcd{1{00}">>, { <<"ab{123abcd{1">>, 0 } },
        { <<"abcd{aa0}">>, { <<"abcd{aa0}">>, 0 } },
        { <<"abcd{10aa0}">>, { <<"abcd{10aa0}">>, 0 } },
        { <<"abcd100}">>, { <<"abcd100}">>, 0 } },
        { <<"abcd100}">>, { <<"abcd100}">>, 0 } }
    ],
    lists:foldl(fun({ Input, Output}, Acc) -> [?_assertEqual(Output, eimap_utils:num_literal_continuation_bytes(Input)) | Acc] end, [], Data).


