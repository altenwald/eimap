-module(eimap_command).

-export([
         parse_response/4,
         formulate_response/2,
         process_status_line/2
        ]).

-type more_tuple() :: { more, ParseContinuation :: parse_continuation(), State :: term }.
-type finished_tuple() :: { fini, Results :: term }.
-type error_tuple() :: { error, Reason :: binary() }.
-type parse_continuation() :: fun((Data :: binary(), Tag :: binary(), State :: term()) ->
                                   more_tuple() | finished_tuple() | error_tuple()).

-callback new_command(Args :: any()) -> { binary(), single_line_response | multiline_reponse | blob_response }.
% TODO:bring back when we can depend on OTP 18 which introduced optional_callback
% also define parse/2 (for blob_response), formulate_response/2
%-callback process_line(Data :: binary(), Acc :: any()) ->
%    more_tuple() | finished_tuple() | error_tuple() | starttls.

parse_response(multiline_response, Data, Tag, ParseState) -> multiline_parse(false, Data, Tag, ParseState);
parse_response(all_multiline_response, Data, Tag, ParseState) -> multiline_parse(parse_tagged, Data, Tag, ParseState);
parse_response(single_line_response, Data, Tag, Module) -> Module:formulate_response(Data, Tag);
parse_response(blob_response, Data, Tag, { Continuation, ParseState }) -> Continuation(Data, Tag, ParseState);
parse_response(blob_response, Data, Tag, Module) ->
    case Module:parse(Data, Tag) of
        { more, Continuation, ParseState } -> { more, { Continuation, ParseState } };
        Response -> Response
    end.

multiline_parse(ParseTaggedLine, Data, Tag, { LastPartialLine, Acc, Module }) ->
    FullBuffer = <<LastPartialLine/binary, Data/binary>>,
    { FullLinesBuffer, NewLastPartialLine } = eimap_utils:only_full_lines(FullBuffer),
    Lines = binary:split(FullLinesBuffer, <<"\r\n">>, [global]),
    process_lines(ParseTaggedLine, Tag, NewLastPartialLine, Lines, Acc, Module);
multiline_parse(ParseTaggedLine, Data, Tag, Module) ->
    multiline_parse(ParseTaggedLine, Data, Tag, { <<>>, [], Module }).

process_lines(_ParseTaggedLine, _Tag, LastPartialLine, [], Acc, Module) -> { more, { LastPartialLine, Acc, Module } };
process_lines(ParseTaggedLine, Tag, LastPartialLine, [Line|MoreLines], Acc, Module) ->
    { FirstLine, ContinuationBytes } = eimap_utils:num_literal_continuation_bytes(Line),
    process_line(ContinuationBytes, ParseTaggedLine, eimap_utils:is_tagged_response(FirstLine, Tag), Tag, LastPartialLine, FirstLine, MoreLines, Acc, Module).


process_line(ContinuationBytes, ParseTaggedLine, IsTagged, Tag, LastPartialLine, Line, [<<>>|MoreLines], Acc, Module) ->
    %% skip empty lines
    process_line(ContinuationBytes, ParseTaggedLine, IsTagged, Tag, LastPartialLine, Line, MoreLines, Acc, Module);
process_line(0, ParseTaggedLine, tagged, Tag, _LastPartialLine, Line, _MoreLines, Acc, Module) ->
    Checked = eimap_utils:check_response_for_failure(Line, Tag),
    Module:formulate_response(Checked, parse_tagged(Checked, ParseTaggedLine, Line, Acc, Module));
process_line(0, ParseTaggedLine, untagged, Tag, LastPartialLine, Line, MoreLines, Acc, Module) ->
    %io:format("Calling it here with ~p~n~n...", [Line]),
    process_lines(ParseTaggedLine, Tag, LastPartialLine, MoreLines, Module:process_line(Line, Acc), Module);
process_line(ContinuationBytes, ParseTaggedLine, _IsTagged, Tag, LastPartialLine, Line, [], Acc, Module) ->
    %% the line was continued, but there is no more lines ... so this line is now our last partial line. more must be on its way
    io:format("Missing lines!~p~n~n", [Acc]),
    BytesAsBinary = integer_to_binary(ContinuationBytes),
    process_lines(ParseTaggedLine, Tag, <<Line/binary, ${, BytesAsBinary/binary, $}, LastPartialLine/binary>>, [], Acc, Module);
process_line(_ContinuationBytes, ParseTaggedLine, IsTagged, Tag, LastPartialLine, Line, [NextLine|MoreLines], Acc, Module) ->
    { StrippedNextLine, NextContinuationBytes } = eimap_utils:num_literal_continuation_bytes(NextLine),
    io:format("Connected up the next line: ~p ~i~n", [StrippedNextLine, NextContinuationBytes]),
    FullLine = <<Line/binary, StrippedNextLine/binary>>,
    process_line(NextContinuationBytes, ParseTaggedLine, IsTagged, Tag, LastPartialLine, FullLine, MoreLines, Acc, Module).

formulate_response(ok, Data) -> { fini, Data };
formulate_response({ _, Reason }, _Data) -> { error, Reason }.

parse_tagged(_, false, _Line, Acc, _Module) -> Acc; % we are not passing the tagged line forward (no content, e.g)
parse_tagged(ok, _, Line, Acc, Module) -> Module:process_tagged_line(Line, Acc); % success, so pass it forward
parse_tagged(_Checked, _ParsedTaggedLine, _Line, Acc, _Module) -> Acc. % error, don't bother passing it forward

-spec process_status_line(Line :: binary(), Acc :: list()) -> NewAcc :: list().
process_status_line(<<"* ", Rest/binary>>, Acc) -> process_matched_status_line(binary:split(Rest, <<" ">>, [global]), Acc);
process_status_line(_, Acc) -> Acc.

process_matched_status_line([Number, Key|_], Acc) -> [{ eimap_utils:binary_to_atom(Key), binary_to_integer(Number) }|Acc];
process_matched_status_line(_, Acc) -> Acc.

