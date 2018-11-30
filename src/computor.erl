%%%-------------------------------------------------------------------
%%% @author ggrybova
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2018 2:45 PM
%%%-------------------------------------------------------------------
-module(computor).
-author("ggrybova").

%%====================================================================
%% API
%%====================================================================
-export([
    main/0
]).

main() ->
	go(#{}).

go(Map0) ->
	Arg0 = io:get_line("> "),
	case Arg0 of
		"q\n" -> ok;
        "clean\n" -> io:format("ok~n"), go(#{});
		_ ->
            {ok, Map1} = get_map(Arg0, Map0),
			io:format("Map = ~p~n", [Map1]),
			go(Map1)
	end.

get_map(Arg0, Map) ->
%%    io:format("Arg = ~p~n", [Arg0]),
    Arg =
        case lists:member($%, Arg0) of
            true ->
                ArgBin0 = list_to_binary(Arg0),
                ArgBin = binary:replace(ArgBin0, <<"%">>, <<" rem ">>, [global]),
                binary_to_list(ArgBin);
            _ ->
                Arg0
        end,
%%    io:format("Arg = ~p~n", [Arg]),
    case erl_scan:string(Arg) of
        {ok, Tokens0, _} ->
            Tokens = case lists:member({'^', 1}, Arg0) of
                         true -> power(Tokens0);
                         _ -> Tokens0
                     end,
            io:format("Tokens = ~p~n", [Tokens]),
            case execute_token(get_type(Tokens, Map), Map) of
                {Name, Proplist} ->
                    print_variable(Proplist),
                    {ok, maps:put(Name, Proplist, Map)};
                _ -> {ok, Map}
            end;
        {error, R} ->
            io:format("Error format: ~p~n", [R]),
            {ok, Map}
    end.


%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%	1.	{ok,Tokens,_EndLine} = erl_scan:string("{1,2,[hello]}.").
%%	2.	{ok,AbsForm} = erl_parse:parse_exprs(Tokens).
%%	3.	{value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()).
%%  Value.

%%  S = "[[2-1, 4/2, 2+1],[2*2, 15/3, 6]].".
%%  {ok,T,_} = erl_scan:string(S).
%%  {ok,Abs} = erl_parse:parse_exprs(T).
%%  {value,V,_} = erl_eval:exprs(Abs, erl_eval:new_bindings()).


%%====================================================================
%% Internal functions
%%====================================================================

%% Map = {var1, proplist1}
%%
%% Proplist = [
%%				{type, variable | complex | matrice | function},
%%				{name, Name},
%%				{value, map for each types},
%%				{reduce, binary or string}
%%			]

power(Tokens0) -> power(Tokens0, []).




reassign_tokens(Tokens, Map) ->
    NewTokens = lists:filtermap(
        fun({atom,1,Var}) ->
            case maps:get(Var, Map, undefined) of
                undefined -> {true, {atom,1,Var}};
                Obj ->
                    Type = proplists:get_value(type, Obj),
                    Value = proplists:get_value(value, Obj),
                    case Type of
                        variable ->
                            if is_float(Value) -> {true, {float,1,Value}};
                                true -> {true, {integer,1,Value}}
                            end;
                        matrice -> {true, {matrice,1,Value}};

                        _ -> io:format("reassign-~n"),{true, {atom,1,Var}}
                    end
            end;
            (Elem) -> {true, Elem}
        end, Tokens),
    NewTokens.

lost_multiplication([]) -> [];

lost_multiplication([Token]) -> [Token];

lost_multiplication(Tokens) ->
    lost_multiplication([], Tokens, []).

lost_multiplication(_, [], Acc) -> lists:reverse(Acc);

lost_multiplication([], [{atom,1,Atom}|T], Acc) ->
    lost_multiplication({atom,1,Atom}, T, [{atom,1,Atom} | [{'*',1}| [{integer,1,1} | Acc]]]);

lost_multiplication([], [P|T], Acc) -> lost_multiplication(P, T, [P|Acc]);

lost_multiplication({Type,1,_Int}, [{atom,1,Atom}|T], Acc) when Type == integer orelse Type == float ->
    lost_multiplication({atom,1,Atom}, T, [{atom,1,Atom} | [{'*',1}| Acc]]);

lost_multiplication(_, [{atom,1,Atom}|T], Acc) ->
    lost_multiplication({atom,1,Atom}, T, [{atom,1,Atom} | [{'*',1}| [{integer,1,1} | Acc]]]);

lost_multiplication(_, [], Acc) -> lost_multiplication([], [], Acc);

lost_multiplication(_, [P|T], Acc) -> lost_multiplication(P, T, [P|Acc]).

execute_token({Type,Name, Expr}, _) when
        Type == variable orelse
        Type == matrice orelse
        Type == function ->
    Proplist = [
        {type, Type},
        {value, Expr}
    ],
    {Name, Proplist};

execute_token({complex,Name,[{atom,1,i}]}, _) -> {Name, [{type, complex},{value, {0, 1}}]};
execute_token({complex,Name,[{op,1,'*',{Type,1,Val},{atom,1,i}}]}, _) when Type == integer orelse Type == float ->
    {Name, [{type, complex},{value, {0, Val}}]};
execute_token({complex,Name,[{op,1,'*',{op,1,'+',{Type,1,Val}},{atom,1,i}}]}, _) when Type == integer orelse Type == float ->
    {Name, [{type, complex},{value, {0, Val}}]};
execute_token({complex,Name,[{op,1,'*',{op,1,'-',{Type,1,Val}},{atom,1,i}}]}, _) when Type == integer orelse Type == float ->
    {Name, [{type, complex},{value, {0, -1*Val}}]};

execute_token({complex,Name, [{op,1,Op, Var1, Var2}]}, _) ->
    [Re, Im] = lists:foldl(
        fun({Type,1,Val}, [AccRe, AccIm]) when Type == integer orelse Type == float -> [AccRe + Val, AccIm];
            ({op,1,'-',{Type,1,Val}}, [AccRe, AccIm]) when Type == integer orelse Type == float -> [AccRe - Val, AccIm];
            ({op,1,'+',{Type,1,Val}}, [AccRe, AccIm]) when Type == integer orelse Type == float -> [AccRe + Val, AccIm];
            ({op,1,_, Var3, Var4}, [AccRe, AccIm]) ->
                ImVar = case Var3 of
                            {atom, 1, i} -> Var4;
                            _ -> Var3
                        end,
                {value,Value, _} = erl_eval:exprs([ImVar], erl_eval:new_bindings()),
                [AccRe, AccIm + Value];
            ({op,1,'+',{op,1,'*',Var5,Var6}}, [AccRe, AccIm]) ->
                ImVar2 = case Var5 of
                             {atom, 1, i} -> Var6;
                             _ -> Var5
                         end,
                {value,Value2, _} = erl_eval:exprs([ImVar2], erl_eval:new_bindings()),
                [AccRe, AccIm + Value2];
            ({op,1,'-',{op,1,'*',Var5,Var6}}, [AccRe, AccIm]) ->
                ImVar2 = case Var5 of
                             {atom, 1, i} -> Var6;
                             _ -> Var5
                         end,
                {value,Value2, _} = erl_eval:exprs([ImVar2], erl_eval:new_bindings()),
                [AccRe, AccIm - Value2];
            (_, [AccRe, AccIm]) -> [AccRe, AccIm]
        end, [0,0], [Var1, {op,1,Op,Var2}]),
    Proplist = [{type, complex}, {value, {Re, Im}}],
    {Name, Proplist};

execute_token({_, [{type, _}, {value, _}]} = Res, _) -> Res;

execute_token(_, _) -> io:format("ERROR: Wrong input data!~n").

get_type([{atom, 1, Name1}, {'=', 1}, {atom, 1, i}], Map) ->
    get_type([{atom, 1, Name1}, {'=', 1}, {integer,1,1}, {'*',1}, {atom,1,i}], Map);

get_type([{atom, 1, Name1}, {'=', 1}, {atom, 1, Name2}], Map) ->
    case maps:find(Name2, Map) of
        {ok, [{type, Type}, {value, Value}]} -> {Name1, [{type, Type}, {value, Value}]};
        _ -> error
    end;

get_type([{atom,1,Name} | [{'(',1} | [{atom,1,Var} | [{')',1} | [{'=',1} | Expr]]]]], Map) ->
    Tokens0 = lost_multiplication(Expr) ++ [{dot, 1}],
    Tokens1 = reassign_tokens(Tokens0, Map),
	{ok,[Abs]} = erl_parse:parse_exprs(Tokens1),
	case quadratic_equations:reduse_form(Abs) of
		{ok, Value} -> {function, {Name, Var}, Value};
		E -> E
	end;

get_type([{atom,1,Name} | [{'=',1} | [{'[',1} | Expr]]], Map) ->
	Tokens0 = lost_multiplication(Expr) ++ [{dot, 1}],
    Tokens1 = reassign_tokens(Tokens0, Map),
    Tokens = [{'[', 1} | lists:reverse(lists:foldl(
        fun({';', 1}, Acc) -> [{',', 1} | Acc];
            (Term, Acc) -> [Term | Acc]
        end, [], Tokens1))],
    Is_matrice0 = lists:foldl(
        fun({'[', 1}, Acc) -> ['[' | Acc];
            ({']', 1}, Acc) -> [']' | Acc];
            (_, Acc) -> Acc
        end, [], Tokens),
%%    io:format(":Is_matrice0:   ~p~n", [Is_matrice0]),
    case string:str(Is_matrice0, ['[', '[', '[']) of
        0 ->
            try
                {ok, Abs} = erl_parse:parse_exprs(Tokens),
                try
                    {value, Value, _} = erl_eval:exprs(Abs, erl_eval:new_bindings()),
%%            io:format("::   ~p~n", [Value]),
                    Is_matrice = lists:foldl(
                        fun(_, false) -> false;
                            (El, Acc) ->
%%                        io:format("El:   ~p~nAcc:   ~p~n", [El, Acc]),
                                case length(El) of
                                    Acc -> Acc;
                                    _ -> false
                                end
                        end, length(lists:nth(1, Value)), Value),
%%            io:format("Is_matrice:   ~p~n", [Is_matrice]),
                    case Is_matrice of
                        false -> error;
                        _ -> {matrice, Name, Value}
                    end
                catch
                    _:_ -> error
                end
            catch
                _:_ -> error
            end;
        _ -> error
    end;


get_type([{atom,1,Name} | [{'=',1} | Expr]], Map) ->
    Tokens0 = lost_multiplication(Expr) ++ [{dot, 1}],
    Tokens = reassign_tokens(Tokens0, Map),
                io:format("Tokens:   ~p~n", [Tokens]),
    try
        {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
        try
            {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
            case is_atom(Value) of
                true when Value == i -> {complex, Name, AbsForm};
                true -> {reassign1, Name, AbsForm};
                _ -> {variable, Name, Value}
            end
        catch
            _:_ ->
                case lists:keyfind(atom, 1, Tokens) of
                    {atom, 1, i} -> {complex, Name, AbsForm};
                    {atom, 1, _} -> {reassign1, Name, AbsForm};
                    _ -> {expression1, Name, AbsForm}
                end
        end
    catch
        _:_ ->
            io:format("get_type!!!!!!~n"),
            case lists:keyfind(atom, 1, Tokens) of
                {atom, 1, _} -> {reassign2, Name, Tokens};
                _ -> {expression2, Name, Tokens}
            end
    end;

get_type(X, _) -> X.

print_variable([{type, variable},{value, Value}]) -> io:format("~p~n", [Value]);

print_variable([{type, matrice},{value, Value}]) -> [io:format("~p~n", [List]) || List <- Value];

print_variable([{type, function},{value, _Value}]) -> io:format("~n");

print_variable([{type, complex},{value, {0, 0}}]) -> io:format("0*i~n");
print_variable([{type, complex},{value, {0, Im}}]) when Im == 1 -> io:format("i~n");
print_variable([{type, complex},{value, {0, Im}}]) when Im == -1 -> io:format("-i~n");
print_variable([{type, complex},{value, {0, Im}}]) -> io:format("~pi~n", [Im]);
print_variable([{type, complex},{value, {Re, 0}}]) -> io:format("~p + 0*i~n", [Re]);
print_variable([{type, complex},{value, {Re, Im}}]) when Im == 1 -> io:format("~p + i~n", [Re]);
print_variable([{type, complex},{value, {Re, Im}}]) when Im > 0 -> io:format("~p + ~pi~n", [Re, Im]);
print_variable([{type, complex},{value, {Re, Im}}]) when Im == -1 -> io:format("~p - i~n", [Re]);
print_variable([{type, complex},{value, {Re, Im}}]) -> io:format("~p - ~pi~n", [Re, -1*Im]);

print_variable(_) -> ok.

%%====================================================================
%% Tests
%%====================================================================
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

expression_test_() ->
    Map = #{},
    Args1 = [
        "varA = 2\n",
        "varB = 4.242\n",
        "varC = -4.3\n",
        "varD =2 * varC\n",
        "varD=varD\n",
        "VarE = 1 -VarL\n",
        "varE         =\t\t2 *(2 + 4 *varC  -4 /3)\n"
    ],
    Test1 = lists:foldl(
        fun(Arg, Acc) ->
            {ok, Res1} = get_map(Arg, Acc),
            Res1
        end, Map, Args1),
    Args2 = ["a = 2\n", "a = 2a\n", "b = 2/3 - 1\n", "c= 2 * (4 + \ta+ 3)\n", "d = c\n", "e = 2d + (82 - b)\n", "ff = 0\n",
        "g = 1/ff + 4\n", "h = 5%0\n", "i =2 * (4 + varA + 3)\n", "j = 2+6  *4 - 10/5 + 3%   7 +  7% 3\n",
        "k = 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999\n"],
    Test2 = lists:foldl(
        fun(Arg, Acc) ->
            {ok, Res2} = get_map(Arg, Acc),
            Res2
        end, Map, Args2),
    [
        ?_assertEqual(Test1, #{
            varA => [{type, variable}, {value, 2}],
            varB => [{type, variable}, {value, 4.242}],
            varC => [{type, variable}, {value, -4.3}],
            varD => [{type, variable}, {value, -8.6}],
            varE => [{type, variable}, {value, -33.06666666666666}]}),
        ?_assertEqual(Test2, #{
            a => [{type, variable}, {value, 4}],
            b => [{type, variable}, {value, -0.33333333333333337}],
            c => [{type, variable}, {value, 22}],
            d => [{type, variable}, {value, 22}],
            e => [{type, variable}, {value, 126.33333333333333}],
            ff => [{type, variable}, {value, 0}],
            g => [{type, variable}, {value, 4.0}],
            j => [{type, variable}, {value, 28.0}],
            k => [{type, variable}, {value, 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999}]})
    ].

matrice_test_() ->
    Map = #{},
    Args = [
        "matA = [[2,3];[4,3]]\n",
        "matB = [[3,4]]\n",
        "matC = [[1,2];[3,2];[3,4]]\n",
        "matD=[[1,2,3.2,4]]\n",
        "matE = [[2-1, 4/2, 2+1];[2*2, 15/3, 6]]\n",
        "matF = [[ff, 4/2, 2+1];[2*d, d/b, a - 0.66666666666666663]]\n",
        "a = 0.66666666666666663\n",
        "b = 654\n",
        "d = 1\n",
        "ff = 0\n",
        "matF = [[ff, 4/2, 2+1];[2*d, d/b, a - 0.66666666666666663]]\n",
        "matG = [[[2,3]];[4,3]]\n",
        "matH = matI\n",
        "matI = matA\n",
        "matJ = [[[2,3]];[[4],3]]\n",
        "matK = [[2,3];[[4,3]]\n",
        "matL = [matA]\n",
        "matM = [[2,3];[4,3];5]\n",
        "matN = [[[3,4]]]\n",
        "matO = [[[3],[4]]]\n",
        "matP = [[2,[3],[4]]]\n",
        "matQ = [[1,2];[[3,2]];[3,4]]\n",
        "matQ = [[1,2,[[3,2]],3,4]]\n"
    ],
    Test = lists:foldl(
        fun(Arg, Acc) ->
            {ok, Res} = get_map(Arg, Acc),
            Res
        end, Map, Args),
    ?_assertEqual(Test, #{
        a => [{type,variable},{value,0.6666666666666666}],
        b => [{type,variable},{value,654}],
        d => [{type,variable},{value,1}],
        ff => [{type,variable},{value,0}],
        matA => [{type,matrice},{value,[[2,3],[4,3]]}],
        matB => [{type,matrice},{value,[[3,4]]}],
        matC => [{type,matrice},{value,[[1,2],[3,2],[3,4]]}],
        matD => [{type,matrice},{value,[[1,2,3.2,4]]}],
        matE => [{type,matrice},{value,[[1,2.0,3],[4,5.0,6]]}],
        matF => [{type,matrice},{value,[[0,2.0,3],[2,654.0,0.0]]}],
        matI => [{type,matrice},{value,[[2,3],[4,3]]}]
    }).

complex_test_() ->
    Map = #{},
    Args1 = [
        "a1 = 2+i\n",
        "a2 = 2-i\n",
        "a3 = -2+i\n",
        "a4 =-2-i\n",
        "a5=+2+i\n",
        "a6 = 2.1+3i\n",
        "a7 = 2.1-3i\n",
        "a8 = -2.1+3i\n",
        "a9 =-2.1-3i\n",
        "a10=+2.1+3i\n"
    ],
        Args2 = [
        "a11 = 2+3*i\n",
        "a12 = 2-3*i\n",
        "a13 = -2+3*i\n",
        "a14 =-2-3*i\n",
        "a15=+2+3*i\n",
        "a16 = 0+3.8i\n",
        "a17 = 2-0i\n",
        "a18 = -2+0*i\n",
        "a19 =-3i\n",
        "a20=+3i\n"
    ],
        Args3 = [
        "a21 = 3.5i\n",
        "a22 = -3*i\n",
        "a23 = +3*i\n",
        "a24 =3*i\n",
        "a25=-i\n",
        "a26 = +i\n",
        "a27 = i\n",
        "a28 = 0i\n"
    ],
    Test1 = lists:foldl(fun(Arg, Acc) -> {ok, Res1} = get_map(Arg, Acc), Res1 end, Map, Args1),
    Test2 = lists:foldl(fun(Arg, Acc) -> {ok, Res1} = get_map(Arg, Acc), Res1 end, Map, Args2),
    Test3 = lists:foldl(fun(Arg, Acc) -> {ok, Res1} = get_map(Arg, Acc), Res1 end, Map, Args3),
    [?_assertEqual(Test1, #{
        a1 => [{type,complex},{value,{2,1}}],
        a2 => [{type,complex},{value,{2,-1}}],
        a3 => [{type,complex},{value,{-2,1}}],
        a4 => [{type,complex},{value,{-2,-1}}],
        a5 => [{type,complex},{value,{2,1}}],
        a6 => [{type,complex},{value,{2.1,3}}],
        a7 => [{type,complex},{value,{2.1,-3}}],
        a8 => [{type,complex},{value,{-2.1,3}}],
        a9 => [{type,complex},{value,{-2.1,-3}}],
        a10 => [{type,complex},{value,{2.1,3}}]}),
    ?_assertEqual(Test2, #{
        a11 => [{type,complex},{value,{2,3}}],
        a12 => [{type,complex},{value,{2,-3}}],
        a13 => [{type,complex},{value,{-2,3}}],
        a14 => [{type,complex},{value,{-2,-3}}],
        a15 => [{type,complex},{value,{2,3}}],
        a16 => [{type,complex},{value,{0,3.8}}],
        a17 => [{type,complex},{value,{2,0}}],
        a18 => [{type,complex},{value,{-2,0}}],
        a19 => [{type,complex},{value,{0,-3}}],
        a20 => [{type,complex},{value,{0,3}}]}),
    ?_assertEqual(Test3, #{
        a21 => [{type,complex},{value,{0, 3.5}}],
        a22 => [{type,complex},{value,{0,-3}}],
        a23 => [{type,complex},{value,{0,3}}],
        a24 => [{type,complex},{value,{0,3}}],
        a25 => [{type,complex},{value,{0,-1}}],
        a26 => [{type,complex},{value,{0,1}}],
        a27 => [{type,complex},{value,{0,1}}],
        a28 => [{type,complex},{value,{0,0}}]})].

%%function_test_() ->
%%    Map = #{},
%%    Args1 = [
%%
%%    ],
%%    Test1 = lists:foldl(fun(Arg, Acc) -> {ok, Res1} = get_map(Arg, Acc), Res1 end, Map, Args1),
%%    ?_assertEqual(Test1, #{
%%
%%}).

-endif.
%%  rp(eunit:test(computor)).