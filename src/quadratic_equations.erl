%%%-------------------------------------------------------------------
%%% @author ggrybova
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2018 19:15
%%%-------------------------------------------------------------------
-module(quadratic_equations).
-author("ggrybova").

-export([
    main/1
]).
%%====================================================================
%% API
%%====================================================================


main(Arg0) ->
    case arg_to_binary(Arg0) of
        error -> io:format("Error. Not valid input.~n");
        Arg ->
            case get_equation(Arg) of
                {error, _} -> io:format("Error. Not valid input.~n");
                Map ->
                    Degree = maps:get(degree, Map),
                    if
                        Degree > 2 ->
                            io:format("The polynomial degree is stricly greater than 2, I can't solve.~n");
                        true ->
                            io:format("Begin to solve:~n"),
                            io:format("STEP 1. Coefficients:    A = ~p, B = ~p, C = ~p~n",
                                [maps:get(a, Map), maps:get(b, Map), maps:get(c, Map)]),
                            solve_equation(Map)
                    end
            end
    end.



%%====================================================================
%% Internal functions
%%====================================================================

arg_to_binary(Arg) ->
    try list_to_binary(Arg)
    catch _:_ -> error
    end.

get_equation(Arg) ->
    Arg1 = binary:replace(Arg, [<<" ">>], <<"">>, [global]),
    case binary:split(Arg1, [<<"=">>], []) of
        [Left, Right] ->
            MonomialsL = split(Left, [<<"*">>], []),
            MonomialsR = split(Right, [<<"*">>], []),
            fill_map(MonomialsL, MonomialsR, []);
        E -> {error, E}
    end.

solve_equation(#{degree := -1}) ->
    io:format("STEP 2. Solutions:       all the real numbers~n");
solve_equation(#{degree := 0, c := 0}) ->
    io:format("STEP 2. Solutions:       all the real numbers~n");
solve_equation(#{degree := 0}) ->
    io:format("STEP 2. The solution:    no solutions~n");
solve_equation(#{degree := 1} = Map) ->
    io:format("STEP 2. The solution:    - C / B = ~p~n", [maps:get(c, Map) * -1 / maps:get(b, Map)]);
solve_equation(#{degree := 2, b := 0, c := 0}) -> io:format("STEP 2. The solution:    0~n");
solve_equation(#{degree := 2, a := A, b := B, c := C} = Map) ->
    D = B*B-4*A*C,
    Map2 = Map#{d => D},
    io:format("STEP 2. Discriminant:    D = ~p~nSTEP 3. ", [maps:get(d, Map2)]),
    case D of
        D when D < 0 -> complex_solve(A, B, D);
        D when D == 0 -> io:format("The solution:   - B / 2A = ~p~n", [-1*B/2/A]);
        D when D > 0 -> io:format("Solutions:       (-B - sq(D)) / 2A = ~p,
                         (-B + sq(D)) / 2A = ~p~n",
            [(-1*B-my_sq(D))/A/2,(-1*B+my_sq(D))/A/2]);
        D ->  io:format("What a fuck?")
    end;
solve_equation(_) ->
    io:format("Please, enter valide polinomial.~n").

complex_solve(A, B, D) ->
    SqD = my_sq(D * -1),
    Im = SqD/2/A,
    case -1*B/2/A of
        Re when Re == 0 orelse Re == 0.0 ->
            io:format("STEP 3. Solutions:   -~pi,   ~pi~n", [Im, Im]);
        Re ->
            io:format("STEP 3. Solutions:   ~p - ~pi,    ~p + ~pi~n", [Re, Im, Re, Im])
    end.

fill_map([], [], Acc) ->
    Acc1 = lists:reverse(lists:dropwhile(
        fun(0) -> true;
            (0.0) -> true;
            (_) -> false
        end, Acc)),
    Acc2 = lists:dropwhile(
        fun(0) -> true;
            (0.0) -> true;
            (_) -> false
        end, Acc1),
    case length(Acc2) of
        0 ->
            print_reduce(Acc2),
            io:format("Degree:          0~n"),
            #{a => 0, b => 0, c => 0, degree => -1};
        Len ->
            print_reduce(lists:reverse(Acc)),
            io:format("Degree:          ~p~n", [Len - 1]),
            Map = case  Len of
                      1 ->
                          [C] = Acc2,
                          #{a => 0, b => 0, c => C};
                      2 ->
                          [C, B] = Acc2,
                          #{a => 0, b => B, c => C};
                      3 ->
                          [C, B, A] = Acc2,
                          #{a => A, b => B, c => C};
                      _ ->
                          #{}
                  end,
            maps:put(degree, Len - 1, Map)
    end;
fill_map([], [H2 | T2], Acc) ->
    fill_map([], T2, [ -1*H2 | Acc]);
fill_map([H1 | T1], [], Acc) ->
    fill_map(T1, [], [H1 | Acc]);
fill_map([H1 | T1], [H2 | T2], Acc) ->
    fill_map(T1, T2, [H1 - H2 | Acc]).

print_reduce([]) ->
    io:format("Reduced form:    0 = 0~n");
print_reduce(List) ->
    io:format("Reduced form:    "),
    lists:foldl(
        fun(Coef, Acc) ->
            case Acc of
                _ when Coef == 0 -> io:format("");
                0 when Coef > 0 -> io:format("~p", [Coef]);
                0 when Coef < 0 -> io:format("- ~p ", [Coef * -1]);
                1  when Coef > 0 -> io:format(" + ~p * X", [Coef]);
                1  -> io:format(" - ~p * X", [Coef * -1]);
                _ when Coef > 0 -> io:format(" + ~p * X^~p", [Coef, Acc]);
                _  -> io:format(" - ~p * X^~p", [Coef * -1, Acc])
            end,
            Acc + 1
        end, 0, List),
    io:format(" = 0~n").

%%====================================================================
%% My library
%%====================================================================

split(Arg, Sep, Acc) ->
    case binary:split(Arg, Sep) of
        [Coef , Rest] ->
            case Coef of
                <<_, "^", _, Y/binary>> ->
                    split(Rest, Sep, [convert_to_float(Y) | Acc]);
                Bin ->
                    split(Rest, Sep, [convert_to_float(Bin) | Acc])
            end;
        _ -> lists:reverse(Acc)
    end.
