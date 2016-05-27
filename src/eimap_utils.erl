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

-module(eimap_utils).
-export([
         extract_path_from_uri/3, extract_uidset_from_uri/1,
         split_command_into_components/1, is_tagged_response/2, remove_tag_from_response/3,
         header_name/1, parse_flags/1,
         check_response_for_failure/2,
         ensure_binary/1,
         new_imap_compressors/0,
         only_full_lines/1,
         binary_to_atom/1,
         num_literal_continuation_bytes/1
        ]).

%% Translate the folder name in to a fully qualified folder path such as it
%% would be used by a cyrus administrator.
-spec extract_path_from_uri(SharedPrefix :: binary(), HierarchyDelim :: binary, URI :: binary()) -> Path :: binary() | bad_uri.
extract_path_from_uri(SharedPrefix, HierarchyDelim, URI) when is_binary(URI) ->
    extract_path_from_uri(SharedPrefix, HierarchyDelim, binary_to_list(URI));
extract_path_from_uri(SharedPrefix, HierarchyDelim, URI) when is_list(URI) ->
    %%lager:info("Parsing ~p", [URI]),
    SchemeDefaults = [{ imap, 143 }, { imaps, 993 }],
    ParseOpts = [ { scheme_defaults, SchemeDefaults } ],
    case imap_folder_path(SharedPrefix, HierarchyDelim, http_uri:parse(URI, ParseOpts)) of
        Path when is_list(Path) -> list_to_binary(Path);
        Error -> Error
    end.

-spec extract_uidset_from_uri(URI :: binary()) -> UIDSet:: binary().
extract_uidset_from_uri(URI) when is_binary(URI) ->
    { TagStart, TagEnd } = binary:match(URI, <<";UID=">>),
    UIDStart = TagStart + TagEnd + 1,
    UriLength = byte_size(URI),
    case binary:match(URI, <<";">>, [{ scope, { UIDStart, UriLength - UIDStart } }]) of
        nomatch -> binary:part(URI, UIDStart - 1, UriLength - UIDStart + 1);
        { Semicolon, _ } -> binary:part(URI, UIDStart - 1, Semicolon - UIDStart + 1)
    end.

-spec header_name(mailbox_uid | groupware_uid | groupware_uid) -> binary(); (any()) -> unknown.
header_name(mailbox_uid) -> <<"/vendor/cmu/cyrus-imapd/uniqueid">>;
header_name(groupware_type) -> <<"X-Kolab-Type">>;
header_name(groupware_uid) -> <<"Subject">>;
header_name(_) -> unknown.

-spec parse_flags(FlagString :: binary() | list()) -> Flags :: [binary()].
parse_flags(String) when is_list(String) -> parse_flags(list_to_binary(String));
parse_flags(<<"(", Parened/binary>>) ->
    case binary:match(Parened, <<")">>) of
        nomatch -> [];
        { ClosingParens, _ } -> parse_flags(binary_part(Parened, 0, ClosingParens))
    end;
parse_flags(<<>>) -> [];
parse_flags(FlagString) when is_binary(FlagString) ->
    binary:split(FlagString, <<" ">>, [global]).

-spec check_response_for_failure(Data :: binary(), Tag :: undefined | binary()) -> ok | { error, Reason :: binary() }.
check_response_for_failure(Data, undefined) when is_binary(Data) ->
    check_response_for_failure(Data, <<>>);
check_response_for_failure(Data, Tag) when is_binary(Data), is_binary(Tag) ->
    NoToken = <<Tag/binary, " NO ">>,
    NoTokenLength = byte_size(NoToken),
    case NoTokenLength > byte_size(Data) of
        true -> ok;
        false -> is_no_token_found(Data, Tag, binary:match(Data, NoToken, [ { scope, { 0, NoTokenLength } } ]))
    end.

-spec split_command_into_components(Buffer :: binary()) -> { Tag :: binary(), Command :: binary(), Data :: binary() }.
split_command_into_components(Buffer) when is_binary(Buffer) ->
    split_command(Buffer).

-spec is_tagged_response(Buffer :: binary(), Tag :: binary()) -> tagged | untagged.
is_tagged_response(Buffer, Tag) ->
    TagSize = size(Tag) + 1, % The extra char is a space
    BufferSize = size(Buffer),
    case
        case (TagSize =< BufferSize) of
            true -> <<Tag/binary, " ">> =:= binary:part(Buffer, 0, TagSize);
            _ -> false
        end of

        true -> tagged;
        _ -> untagged
    end.

-spec num_literal_continuation_bytes(Buffer :: binary()) -> { BufferSansContinuation :: binary(), NumberBytes :: integer() }.
num_literal_continuation_bytes(Buffer) when size(Buffer) < 4 ->
    { Buffer, 0 };
num_literal_continuation_bytes(Buffer) ->
    case binary:last(Buffer) =:= $} of
        true -> number_of_bytes_in_continuation(Buffer);
        false -> { Buffer, 0 }
    end.

number_of_bytes_in_continuation(Buffer) ->
    BufferSize = size(Buffer),
    OpenBracePos = find_continuation_open_brace(Buffer, BufferSize - 3),
    confirm_continuation(Buffer, OpenBracePos).

find_continuation_open_brace(_Buffer, 0) -> -1;
find_continuation_open_brace(Buffer, Pos) ->
    case binary:at(Buffer, Pos) of
        ${ -> Pos;
        _ -> find_continuation_open_brace(Buffer, Pos - 1)
    end.

confirm_continuation(Buffer, -1) ->
    { Buffer, 0 };
confirm_continuation(Buffer, OpenBracePos) ->
    BufferSize = size(Buffer),
    try binary_to_integer(binary:part(Buffer, OpenBracePos + 1, BufferSize - OpenBracePos - 2)) of
        Result -> { binary:part(Buffer, 0, OpenBracePos), Result }
    catch
        _:_ -> { Buffer, 0 }
    end.

-spec remove_tag_from_response(Buffer :: binary(), Tag :: undefine | binary(), Check :: check | trust) -> Command :: binary().
remove_tag_from_response(Buffer, undefined, _) ->
    Buffer;
remove_tag_from_response(Buffer, <<>>, _) ->
    Buffer;
remove_tag_from_response(Buffer, Tag, check) ->
    TagSize = size(Tag) + 1, % The extra char is a space
    BufferSize = size(Buffer),
    case TagSize =< BufferSize of
        true ->
            case <<Tag/binary, " ">> =:= binary:part(Buffer, 0, TagSize) of
                true -> binary:part(Buffer, TagSize, BufferSize - TagSize);
                false -> Buffer
            end;
        false -> Buffer
    end;
remove_tag_from_response(Buffer, Tag, trust) ->
    TagSize = size(Tag) + 1, % The extra char is a space
    BufferSize = size(Buffer),
    case TagSize =< BufferSize of
        true -> binary:part(Buffer, TagSize, BufferSize - TagSize);
        false -> Buffer
    end.

%% Private
split_command(<<>>) -> { <<>>, <<>>, <<>> };
split_command(Buffer) ->
    End = eol_found(Buffer, binary:match(Buffer, <<"\r\n">>)),
    { Tag, CommandStart } = searched_in_buffer(Buffer, 0, End, binary:match(Buffer, <<" ">>, [ { scope, { 0, End } } ])),
    { Command, DataStart } = searched_in_buffer(Buffer, CommandStart, End, binary:match(Buffer, <<" ">>, [ { scope, { CommandStart, End - CommandStart  } } ])),
    Data = binary:part(Buffer, DataStart, End - (DataStart)),
    { Tag, Command, Data }.

eol_found(Buffer, nomatch) -> size(Buffer);
eol_found(_Buffer, { MatchStart, _MatchLength }) -> MatchStart.

searched_in_buffer(Buffer, Start, End, nomatch) -> { binary:part(Buffer, Start, End - Start), End };
searched_in_buffer(Buffer, Start, _End, { MatchStart, MatchLength } ) -> { binary:part(Buffer, Start, MatchStart - Start), MatchStart + MatchLength }.

is_no_token_found(Data, Tag, nomatch) ->
    BadToken = <<Tag/binary, " BAD ">>,
    BadTokenLength = byte_size(BadToken),
    Match = binary:match(Data, BadToken, [ { scope, { 0, BadTokenLength } } ]),
    is_bad_token_found(Data, Tag, Match);
is_no_token_found(Data, _Tag, { Start, Length }) ->
    ReasonStart = Start + Length,
    Reason = binary:part(Data, ReasonStart, byte_size(Data) - ReasonStart),
    { no, chop_newlines(Reason) }.

is_bad_token_found(_Data, _Tag, nomatch) ->
    ok;
is_bad_token_found(Data, _Tag, { Start, Length }) ->
    ReasonStart = Start + Length,
    %% -2 is due to the traling \r\n
    Reason = binary:part(Data, ReasonStart, byte_size(Data) - ReasonStart),
    { bad, chop_newlines(Reason) }.

chop_newlines(Data) ->
    Size = size(Data),
    chop_newline(Data, binary:at(Data, Size - 1), Size - 1).

chop_newline(Data, $\r, Size) -> chop_newline(Data, binary:at(Data, Size - 1), Size - 1);
chop_newline(Data, $\n, Size) -> chop_newline(Data, binary:at(Data, Size - 1), Size - 1);
chop_newline(Data, _, Size) -> binary_part(Data, 0, Size + 1).

imap_folder_path_from_parts(none, _HierarchyDelim, [], _Domain, Path) ->
    Path;
imap_folder_path_from_parts(SharedPrefix, _HierarchyDelim, [], _Domain, Path) ->
    case string:str(Path, SharedPrefix) of
        1 -> string:substr(Path, length(SharedPrefix) + 1);
        _ -> Path
    end;
imap_folder_path_from_parts(_SharedPrefix, HierarchyDelim, User, Domain, "INBOX") ->
    string:join(["user", string:join([User, Domain], "@")], HierarchyDelim);
imap_folder_path_from_parts(_SharedPrefix, HierarchyDelim, User , Domain, Path) ->
    string:join(["user", User, string:join([Path, Domain], "@")], HierarchyDelim).

imap_folder_path(_SharedPrefix, _HierarchyDelim, { error, Reason }) ->
    lager:info("ERROR! ~p", [Reason]),
    bad_uri;
imap_folder_path(SharedPrefix, HierarchyDelim, { ok, {_Scheme, User, Domain, _Port, FullPath, _Query} }) ->
    { VDomain, _ImapHost } = split_imap_uri_domain(string:tokens(Domain, "@")),
    [ [_|Path] | _ ] = string:tokens(FullPath, ";"),
    %%lager:info("PARSED IMAP URI: ~p ~p ~p", [User, VDomain, Path]),
    CanonicalPath = imap_folder_path_from_parts(SharedPrefix, HierarchyDelim, User, VDomain, http_uri:decode(Path)),
    %%lager:info("PUT TOGETHER AS: ~p", [CanonicalPath]),
    CanonicalPath.

split_imap_uri_domain([ ImapHost ]) -> { ImapHost, ImapHost };
split_imap_uri_domain([ VDomain, ImapHost ]) -> { VDomain, ImapHost }.

ensure_binary(Arg) when is_list(Arg) -> list_to_binary(Arg);
ensure_binary(Arg) when is_binary(Arg) -> Arg;
ensure_binary(Arg) when is_atom(Arg) -> atom_to_binary(Arg, latin1);
ensure_binary(_Arg) -> <<>>.

new_imap_compressors() ->
    Inflator = zlib:open(),
    ok = zlib:inflateInit(Inflator, -15),
    Deflator = zlib:open(),
    ok = zlib:deflateInit(Deflator, 1, deflated, -15, 8, default),
    { Inflator, Deflator }.

-spec only_full_lines(Buffer :: binary()) -> { BufferOfFullLines :: binary(), TrailingFragmentaryLine :: binary() }.
only_full_lines(Buffer) ->
    BufferLength = size(Buffer),
    only_full_lines(Buffer, BufferLength, binary:at(Buffer, BufferLength - 1), BufferLength).

only_full_lines(Buffer, BufferLength, $\n, Pos) when Pos =:= BufferLength -> { Buffer, <<>> };
only_full_lines(Buffer, BufferLength, $\n, Pos) -> { binary:part(Buffer, 0, Pos + 1), binary:part(Buffer, Pos + 1, BufferLength - Pos - 1) };
only_full_lines(Buffer, _BufferLength, _, 0) -> { <<>>, Buffer };
only_full_lines(Buffer, BufferLength, _, Pos) -> only_full_lines(Buffer, BufferLength, binary:at(Buffer, Pos - 1), Pos - 1).

-spec binary_to_atom(Value :: binary()) -> ValueAsAtom :: atom().
binary_to_atom(Value) -> list_to_atom(string:to_lower(binary_to_list(Value))).
