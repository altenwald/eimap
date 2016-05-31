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

-module(eimap_command_annotation_tests).
-include_lib("eunit/include/eunit.hrl").

% c("test/eimap_command_annotation_tests.erl"). eunit:test(eimap_command_annotation).

parse_test_() ->
    Data =
    [
        %% { tag, server_response, expected_parsed_results }
        {
            <<"EG0002">>,
            <<"* ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/kolab/color\" (\"value.shared\" \"32CD32\")\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/kolab/folder-type\" (\"value.shared\" \"event\")\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/x-toltec/test\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/horde/share-params\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/kolab/h-share-attr-desc\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/kolab/uniqueid\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/kolab/pxfb-readable-for\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/kolab/incidences-for\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/kolab/folder-test\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/kolab/displayname\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/kolab/activesync\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/uniqueid\" (\"value.shared\" \"b1a5bb95-2bd3-4628-a0d2-49e9bd10735e\")\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/squat\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/size\" (\"value.shared\" \"5726\")\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/sieve\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/sharedseen\" (\"value.shared\" \"false\")\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/pop3showafter\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/pop3newuidl\" (\"value.shared\" \"true\")\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/partition\" (\"value.shared\" \"default\")\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/news2mail\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/lastupdate\" (\"value.shared\" \"20-Mar-2015 05:17:51 +0100\")\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/lastpop\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/expire\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/vendor/cmu/cyrus-imapd/duplicatedeliver\" (\"value.shared\" \"false\")\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/thread\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/specialuse\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/sort\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/comment\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/checkperiod\" (\"value.shared\" NIL)\r\n    * ANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"/check\" (\"value.shared\" NIL)\r\nEG0002 OK Completed\r\n">>,
            { fini, [
                {<<"/vendor/cmu/cyrus-imapd/duplicatedeliver">>, false},
                {<<"/vendor/cmu/cyrus-imapd/lastupdate">>, <<"20-Mar-2015 05:17:51 +0100">>},
                {<<"/vendor/cmu/cyrus-imapd/partition">>,<<"default">>},
                {<<"/vendor/cmu/cyrus-imapd/pop3newuidl">>,true},
                {<<"/vendor/cmu/cyrus-imapd/sharedseen">>,false},
                {<<"/vendor/cmu/cyrus-imapd/size">>,5726},
                {<<"/vendor/cmu/cyrus-imapd/uniqueid">>, <<"b1a5bb95-2bd3-4628-a0d2-49e9bd10735e">>},
                {<<"/vendor/kolab/folder-type">>,<<"event">>},
                {<<"/vendor/kolab/color">>,<<"32CD32">>}
               ]
            }
        },
        {
            <<"EG0002">>,
            <<"* ANNOTATION user/john.doe/Sent@example.org \"/vendor/x-toltec/test\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/horde/share-params\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/kolab/h-share-attr-desc\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/kolab/uniqueid\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/kolab/pxfb-readable-for\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/kolab/incidences-for\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/kolab/folder-type\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/kolab/folder-test\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/kolab/displayname\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/kolab/color\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/kolab/activesync\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/uniqueid\" (\"value.shared\" \"237357ec-7610-422e-9e55-0bae83caf58a\")\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/squat\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/size\" (\"value.shared\" \"401\")\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/sieve\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/sharedseen\" (\"value.shared\" \"false\")\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/pop3showafter\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/pop3newuidl\" (\"value.shared\" \"true\")\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/partition\" (\"value.shared\" \"default\")\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/news2mail\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/lastupdate\" (\"value.shared\" \"23-Mar-2015 15:19:29 +0100\")\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/lastpop\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/expire\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/vendor/cmu/cyrus-imapd/duplicatedeliver\" (\"value.shared\" \"false\")\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/thread\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/specialuse\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/sort\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/comment\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/checkperiod\" (\"value.shared\" NIL)\r\n         * ANNOTATION user/john.doe/Sent@example.org \"/check\" (\"value.shared\" NIL)\r\nEG0002 OK Completed\r\n">>,
            { fini, [
                {<<"/vendor/cmu/cyrus-imapd/duplicatedeliver">>,false},
                {<<"/vendor/cmu/cyrus-imapd/lastupdate">>,<<"23-Mar-2015 15:19:29 +0100">>},
                {<<"/vendor/cmu/cyrus-imapd/partition">>,<<"default">>},
                {<<"/vendor/cmu/cyrus-imapd/pop3newuidl">>,true},
                {<<"/vendor/cmu/cyrus-imapd/sharedseen">>,false},
                {<<"/vendor/cmu/cyrus-imapd/size">>,401},
                {<<"/vendor/cmu/cyrus-imapd/uniqueid">>,<<"237357ec-7610-422e-9e55-0bae83caf58a">>}
                    ]
            }
        }
    ],
    { _Command, ResponseType } = eimap_command_annotation:new_command(<<"/my/folder">>),
    lists:foldl(fun({ Tag, ServerData, Expected }, Acc) -> [?_assertEqual(Expected, eimap_command:parse_response(ResponseType, ServerData, Tag, eimap_command_annotation))|Acc] end, [], Data).

new_test_() ->
    Data =
    [
        %% mailbox, command
        {
          <<"user/john.doe/Calendar/Personal Calendar@example.org">>,
          { <<"GETANNOTATION \"user/john.doe/Calendar/Personal Calendar@example.org\" \"*\" \"value.shared\"">>, multiline_response }
        }
    ],
    lists:foldl(fun({ Mailbox, Command }, Acc) -> [?_assertEqual(Command, eimap_command_annotation:new_command(Mailbox))|Acc] end, [], Data).

