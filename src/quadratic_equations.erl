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
    main/1,
    reduse_form/1,
    reduse_form_of_polinom/2
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

reduse_form({op,1,Operation, Part1, Part2}) ->
    io:format("("),
	Reduse1 =
	try
		{value, Value1, _} = erl_eval:exprs([Part1], erl_eval:new_bindings()),
        io:format("~p ", [Value1]),
		{integer,1,Value1}
	catch
	    _:_  ->
			case reduse_form(Part1) of
				{ok, Reduse11} -> Reduse11;
				_ ->
                    io:format("~p ", [Part1]),
                    Part1
			end
	end,
    io:format(" ~s ", [Operation]),
	Reduse2 =
		try
			{value, Value2, _} = erl_eval:exprs([Part2], erl_eval:new_bindings()),
            io:format(" ~p", [Value2]),
            {integer,1,Value2}
		catch
			_:_  ->
				case reduse_form(Part2) of
					{ok, Reduse22} -> Reduse22;
					_ ->
                        io:format(" ~p", [Part2]),
                        Part2
				end
		end,
	io:format(")"),
    {ok, {op,1,Operation, Reduse1, Reduse2}};

reduse_form(_) -> error.

reduse_form_of_polinom([{Op, 1}|T], Name) when Op == '+' orelse Op == '-' ->
    reduse_form_of_polinom([{Op, 1}], T, [], Name);

reduse_form_of_polinom(Tokens, Name) -> reduse_form_of_polinom([{'+', 1}|Tokens], Name).

reduse_form_of_polinom(_, [], Acc, _) -> lists:reverse(lists:keysort(1, Acc));

reduse_form_of_polinom(H0, [{dot,1}], Acc, Name) -> reduse_form_of_polinom(H0, [{'+', 1}], Acc, Name);

reduse_form_of_polinom(H0, [{Op, 1}|T], Acc, Var) when (Op == '+' orelse Op == '-') ->
    case get_monomial(lists:reverse(H0)) of
        {Coef, Name, Degree} when Name == Var orelse Name == undefined ->
            NewAcc =
                case lists:keyfind(Degree, 1, Acc) of
                    false ->
                        lists:keystore(Degree, 1, Acc, {Degree, Coef});
                    {D, C} ->
                        lists:keyreplace(D, 1, Acc, {D, C + Coef})
                end,
%%            io:format("Op: ~p~n", [Op]),
            reduse_form_of_polinom([{Op, 1}], T, NewAcc, Var);
        _ ->
            {error, "Not valid polinomial!"}
    end;

reduse_form_of_polinom(H0, [H|T], Acc, Name) -> reduse_form_of_polinom([H|H0], T, Acc, Name).

get_monomial(List) ->
    {Part01, Part02} = lists:splitwith(
        fun({atom,1,_}) -> false;
            (_) -> true
        end, List),
    {Degree, Var, Part2} =
        case Part02 of
            [{atom,1,V}, {'^',1}, {integer,1,X}|T] -> {X, V, T};
            [{atom,1,V}|T] -> {1, V, T};
            T2 -> {0, undefined, T2}
        end,
    io:format("Part01: ~p~n", [Part01]),
    Part1 =
        case Part2 of
            [] when Var =/= undefined ->
                case lists:last(Part01) of
                    {Op,1} when Op == '/' -> [not_polinom];
                    _ -> lists:droplast(Part01)
                end;
            _ -> Part01
        end,
    io:format("Part1: ~p~n", [Part1]),
    Val = Part1 ++ Part2 ++ [{dot,1}],
    try
        {ok, AbsForm} = erl_parse:parse_exprs(Val),
        {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
        {Value, Var, Degree}
    catch _:_ -> {error, "Not valid polinomial!"}
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
            [(-1*B-my_lib:my_sqrt(D))/A/2,(-1*B+my_lib:my_sqrt(D))/A/2]);
        D ->  io:format("What a fuck?")
    end;
solve_equation(_) ->
    io:format("Please, enter valide polinomial.~n").

complex_solve(A, B, D) ->
    SqD = my_lib:my_sqrt(D * -1),
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
                    split(Rest, Sep, [my_lib:binary_to_number(Y) | Acc]);
                Bin ->
                    split(Rest, Sep, [my_lib:binary_to_number(Bin) | Acc])
            end;
        _ -> lists:reverse(Acc)
    end.

