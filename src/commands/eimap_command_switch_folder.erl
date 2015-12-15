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

-module(eimap_command_switch_folder).
-behavior(eimap_command).
-export([new_command/1, process_line/2, process_tagged_line/2, formulate_response/2]).

%% https://tools.ietf.org/html/rfc3501#section-6.3.2

%% Public API
new_command({ MBox, Mechanism }) when is_list(MBox) -> new_command({ list_to_binary(MBox), Mechanism });
new_command({ MBox, select }) -> new_command(MBox);
new_command({ MBox, examine }) -> { <<"EXAMINE \"", MBox/binary, "\"">>, all_multiline_response };
new_command(MBox) when is_list(MBox) -> new_command(list_to_binary(MBox));
new_command(MBox) when is_binary(MBox) -> { <<"SELECT \"", MBox/binary, "\"">>, all_multiline_response }.

%TODO: parse:
% REQUIRED untagged responses: FLAGS, EXISTS, RECENT
% REQUIRED OK untagged responses:  UNSEEN,  PERMANENTFLAGS, UIDNEXT, UIDVALIDITY
process_line(<<"* OK [", Info/binary>>, Acc) ->
    case binary:match(Info, <<"]">>) of
        nomatch -> Acc;
        { ClosingBracket, _ } -> process_ok_response(binary:part(Info, 0, ClosingBracket), Acc)
    end;
process_line(<<"* FLAGS ", FlagString/binary>>, Acc) ->
    Flags = eimap_utils:parse_flags(FlagString),
    [{ flags, Flags }|Acc];
process_line(<<"* ", Info/binary>>, Acc) ->
    case binary:split(Info, <<" ">>) of
        [ Value, Key ] -> [{ eimap_utils:binary_to_atom(Key), binary_to_integer(Value) }|Acc];
        _ ->Acc
    end;
process_line(_Data, Acc) -> Acc.

process_tagged_line(Data, Acc) ->
    case binary:match(Data, <<"[READ-WRITE]">>) of
        nomatch -> [{ writeable, false }|Acc];
        _ -> [{ writeable, true}|Acc]
    end.

formulate_response(Response, Acc) -> eimap_command:formulate_response(Response, Acc).

%PRIVATE
process_ok_response(<<"PERMANENTFLAGS ", FlagString/binary>>, Acc) -> [{ permanent_flags, eimap_utils:parse_flags(FlagString) }|Acc];
process_ok_response(<<"UIDVALIDITY ", String/binary>>, Acc) -> [{ uid_validity, binary_to_integer(String) }|Acc];
process_ok_response(<<"UIDNEXT ", String/binary>>, Acc) -> [{ uid_next, binary_to_integer(String) }|Acc];
process_ok_response(<<"HIGHESTMODSEQ ", String/binary>>, Acc) -> [{ highest_mod_seq, binary_to_integer(String) }|Acc];
process_ok_response(<<"URLMECH ", String/binary>>, Acc) -> [{ url_mech, eimap_utils:binary_to_atom(String) }|Acc]; %TODO: this is very ugly
process_ok_response(<<"ANNOTATIONS ", String/binary>>, Acc) -> [{ annotations, binary_to_integer(String) }|Acc];
process_ok_response(String, Acc) -> lager:warning("eimap_command_select_folder: Unknown untagged OK response ~s~n", [String]), Acc.

