-module(eimap_command).

-type more_tuple() :: { more, ParseContinuation :: parse_continuation(), State :: term }.
-type finished_tuple() :: { fini, Results :: term }.
-type error_tuple() :: { error, Reason :: binary() }.
-type parse_continuation() :: fun((Data :: binary(), Tag :: binary(), State :: term()) ->
                                   more_tuple() | finished_tuple() | error_tuple()).

-callback new(Args :: any()) -> binary().
-callback parse(Data :: binary(), Tag :: binary()) ->
    more_tuple() | finished_tuple() | error_tuple().


