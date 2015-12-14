-module(eimap_command).

-export([do_parse/4, formulate_response/2]).

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

do_parse(multiline_response, Data, Tag, ParseState) -> multiline_parse(Data, Tag, ParseState);
do_parse(single_line_response, Data, Tag, Module) -> Module:formulate_response(Data, Tag);
do_parse(blob_response, Data, Tag, { Continuation, ParseState }) -> Continuation(Data, Tag, ParseState);
do_parse(blob_response, Data, Tag, Module) ->
    case Module:parse(Data, Tag) of
        { more, Continuation, ParseState } -> { more, { Continuation, ParseState } };
        Response -> Response
    end.

multiline_parse(Data, Tag, { LastPartialLine, Acc, Module }) ->
    FullBuffer = <<LastPartialLine/binary, Data/binary>>,
    { FullLinesBuffer, NewLastPartialLine } = eimap_utils:only_full_lines(FullBuffer),
    Lines = binary:split(FullLinesBuffer, <<"\r\n">>, [global]),
    process_lines(Tag, NewLastPartialLine, Lines, Acc, Module);
multiline_parse(Data, Tag, Module) ->
    multiline_parse(Data, Tag, { <<>>, [], Module }).

process_lines(_Tag, LastPartialLine, [], Acc, Module) -> { more, { LastPartialLine, Acc, Module } };
process_lines(Tag, LastPartialLine, [Line|MoreLines], Acc, Module) ->
    case eimap_utils:is_tagged_response(Line, Tag) of
        true ->
            Module:formulate_response(eimap_utils:check_response_for_failure(Line, Tag), Acc);
        false ->
            process_lines(Tag, LastPartialLine, MoreLines, Module:process_line(Line, Acc), Module)
    end.

formulate_response(ok, Data) -> { fini, Data };
formulate_response({ _, Reason }, _Data) -> { error, Reason }.

