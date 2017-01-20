% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

-module(solution).
-export([main/0]).

is_fun(_, {false, _F}) -> io:format("NO~n");
is_fun([], {true, _F}) -> io:format("YES~n");
is_fun([H|Tail], {true, F}) ->
    {X,Y} = H,
    Function = case F(X) of
                   undef -> {true, fun (X1) when X1 == X -> Y;
                                       (X1) -> F(X1) end
                            };
                   Y -> {true, F};
                   _ -> {false, F}
               end,
    is_fun(Tail, Function).

is_fun(List)->
    F = fun(_X)-> undef end,
    is_fun(List, {true, F}).

read_N_Tuples(0, List)->List;
read_N_Tuples(N, List)->
    {ok, [X, Y]} = io:fread("","~d ~d"),
    read_N_Tuples(N-1, [{X,Y}|List]).

process_test_case(0)->true;
process_test_case(T)->
    {ok, [N]} = io:fread("","~d"),
    List = read_N_Tuples(N, []),
    %[io:format("~w ", [L])||L<-List],
    is_fun(List),
    process_test_case(T-1).

main() ->
    {ok, [T]} = io:fread("","~d"),
    process_test_case(T).

