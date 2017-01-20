-module(solution).
-export([main/0]).
-export([new/0,in/2,drop/1,head/1]).

-define(logging, true).
%%-define(debug, true).
-define(info, true).
%%-define(custom_q, true).

-define(empty_log(F), true).
-define(empty_log(F, A), true).

-ifdef(logging).
-define(log(Format, Args), 
        io:format(standard_error, "[~w,~w] : ", [?MODULE, ?LINE]), 
        io:format(standard_error, Format, Args), 
        io:format(standard_error, "~n", [])).
-else.
-define(log(F,A), ?empty_log(F,A)).
-endif.

-define(log(Format), ?log(Format, [])).

-ifdef(debug).
-define(log_debug(F,A), ?log(F,A)).
-define(log_debug(F), ?log(F)).
-else.
-define(log_debug(F,A), ?empty_log(F,A)).
-define(log_debug(F), ?empty_log(F)).
-endif.

-ifdef(info).
-define(log_info(F,A), ?log(F,A)).
-define(log_info(F), ?log(F)).
-else.
-define(log_info(F,A), ?empty_log(F,A)).
-define(log_info(F), ?empty_log(F)).
-endif.

-ifdef(custom_q).
-define(q, ?MODULE).
-else.
-define(q, queue).
-endif.

-define(query_enqueue, 1).
-define(query_dequeue, 2).
-define(query_peek, 3).

%% Build a queue out of two stacks
%% Since a list is essentially a stack
%% Lets just use that

%% Q => {Deq, Enq, Head}

%% enqueue is straightforward
in(E, {[], [], _H})->
    {[], [E], E};
in(E, {Deq, Enq, H})->
    ?log_debug("Enqueueing ~w: enq stack = ~w", [E, Enq]),
    {Deq, [E|Enq], H}.

%% Empty queue has no head
reshuffle({[], [], _H})-> {[], [], 0};
%% Deq stack is empty, but there is something in enq stack
reshuffle({[], Enq, H})->
    reshuffle({lists:reverse(Enq), [], H});
%% Deq stack is still kicking, update head
reshuffle({Deq = [H|_], Enq, _})->
    {Deq, Enq, H}.

%% nothing to dequeue
drop(Q = {[], [], _H})-> Q;

%% drop and reshuffle if required
drop({[_E|Deq], Enq, H})-> 
    ?log_debug("Dequeued element: deq stack = ~w", [Deq]),
    reshuffle({Deq, Enq, H});

%% deq stack is empty, reshuffle and and then drop
%% and then reshuffle again if required
drop(Q = {[], _Enq, _H})->
    drop(reshuffle(Q)).

%% nothing to peek at
head({[], [], _H})-> error;
%% Optimizing peek by storing head
head({_Deq, _Enq, H})->H.

-ifdef(peek).
-ifndef(custom_q).
peek(Q)->
    H = ?q:head(Q),
    io:format("~w~n", [H]),
    Q.
-else.
peek(Q)->
    H= ?q:head(Q),
    io:format("~w~n", [H]),
    Q.
-endif.
-endif.


new()->
    ?log_info("Using custom q '~w' instead of erlang built-in", [?q]),
    {[],[], 0}.

read_and_process_query(Q, Size, In, Out)->
    {ok, [Query]} = io:fread("","~d"),
    case Query of
        ?query_enqueue ->
            {ok, [Element]} = io:fread("", "~d"),
            {?q:in(Element, Q), Size+1, In+1, Out};
        ?query_dequeue ->
            {?q:drop(Q), Size-1, In, Out+1};
        ?query_peek ->
            H = ?q:head(Q),
            io:format("~w~n", [H]),
            {Q, Size, In, Out}
    end.

process_query(0, {_, Size, In, Out})-> 
    ?log_info("Size = ~w In = ~w Out = ~w", [Size, In, Out]);
process_query(N, {Q, Size, In, Out})->
    process_query(N-1, read_and_process_query(Q, Size, In, Out)).

process_query(N)->
    Q = ?q:new(),
    process_query(N, {Q, 0, 0, 0}).

main()->
    {ok, [NumOfQueries]} = io:fread("", "~d"),
    ?log_info("Number of Queries = ~w", [NumOfQueries]),
    process_query(NumOfQueries).
