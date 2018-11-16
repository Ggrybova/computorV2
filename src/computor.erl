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

go(Map) ->
	Arg0 = io:get_line("> "),
	case Arg0 of
		"q\n" -> ok;
        "clean\n" -> io:format("ok~n"), go(#{});
		_ ->
			Arg =
				case lists:member($%, Arg0) of
					true ->
						ArgBin0 = list_to_binary(Arg0),
						ArgBin = binary:replace(ArgBin0, <<"%">>, <<" rem ">>, [global]),
						binary_to_list(ArgBin);
					_ ->
						Arg0
				end,
			Map0 =
				case erl_scan:string(Arg) of
					{ok, Tokens, _} ->
						case execute_token(get_type(Tokens, Map), Map) of
							{Name, Proplist} ->
								print_variable(Proplist),
								maps:put(Name, Proplist, Map);
							_ -> Map
						end;
					{error, R} ->
						io:format("Error format: ~p~n", [R]),
						Map
				end,
			io:format("Map = ~p~n", [Map0]),
			go(Map0)
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

reassign_tokens(Tokens, Map) ->
    NewTokens = lists:filtermap(
        fun({atom,1,Var}) ->
            case maps:get(Var, Map, undefined) of
                undefined -> {true, {atom,1,Var}};
                Obj ->
                    Type = proplists:get_value(type, Obj),
                    Value = proplists:get_value(value, Obj),
%%                    io:format("     Type = ~p, Value = ~p~n", [Type, Value]),
                    case Type of
                        variable ->
%%                            io:format("reassign+~n"),
                            if is_float(Value) -> {true, {float,1,Value}};
                                true -> {true, {integer,1,Value}}
                            end;
                        _ -> io:format("reassign-~n"),{true, {atom,1,Var}}
                    end
            end;
            (Elem) -> {true, Elem}
        end, Tokens),
%%    io:format("     NewExpr = ~p~n", [NewExpr]),
    NewTokens.

lost_multiplication([]) -> [];

lost_multiplication([Token]) -> [Token];

lost_multiplication(Tokens) ->
    lost_multiplication([], Tokens, []).

lost_multiplication(_, [], Acc) -> lists:reverse(Acc);

lost_multiplication([], [P|T], Acc) -> lost_multiplication(P, T, [P|Acc]);

lost_multiplication({integer,1,_Int}, [{atom,1,Atom}|T], Acc) ->
    lost_multiplication({atom,1,Atom}, T, [{atom,1,Atom} | [{'*',1}| Acc]]);

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

execute_token({complex,Name, [{op,1,Op, Var1, Var2}]}, _) ->
    [Re, Im] = lists:foldl(
        fun({integer,1,Val}, [AccRe, AccIm]) -> [AccRe + Val, AccIm];
            ({op,1,'-',{integer,1,Val}}, [AccRe, AccIm]) -> [AccRe - Val, AccIm];
%%            ({float,1,Val}, [AccRe, AccIm]) -> [AccRe + Val, AccIm];
%%            ({op,1,'-',{float,1,Val}}, [AccRe, AccIm]) -> [AccRe - Val, AccIm];
            ({op,1,Op2, Var3, Var4}, [AccRe, AccIm]) ->
                ImVar =
                    case Var3 of
                        {atom, 1, i} -> Var4;
                        _ -> Var3
                    end,
                {value,Value, _} = erl_eval:exprs([ImVar], erl_eval:new_bindings()),
                [AccRe, AccIm + Value]
        end, [0,0], [Var1, {op,1,Op,Var2}]),
    io:format("~p   ~pi~n", [Re, Im]),
    Proplist = [
        {type, complex},
        {value, {Re, Im}}
    ],
    {Name, Proplist}.

get_type([{atom,1,Name} | [{'(',1} | [{atom,1,Var} | [{')',1} | [{'=',1} | Expr]]]]], Map) ->
    Tokens0 = lost_multiplication(Expr) ++ [{dot, 1}],
    Tokens1 = reassign_tokens(Tokens0, Map),
%%    io:format("Tokens1: ~p~n", [Tokens1]),
	{ok,[Abs]} = erl_parse:parse_exprs(Tokens1),
%%	io:format("Abs: ~p~n", [Abs]),
	case quadratic_equations:reduse_form(Abs) of
		{ok, Value} ->
%%			io:format("Value: ~p~n", [Value]),
			{function, {Name, Var}, Value};
		E -> E
	end;
%%	{value,Value,_} = erl_eval:exprs(Abs, erl_eval:new_bindings()),


get_type([{atom,1,Name} | [{'=',1} | [{'[',1} | Expr]]], Map) ->
	Tokens0 = lost_multiplication(Expr) ++ [{dot, 1}],
    Tokens1 = reassign_tokens(Tokens0, Map),
	Tokens = [{'[',1} | lists:reverse(lists:foldl(
		fun({';',1}, Acc) -> [{',',1} | Acc];
			(Term, Acc) -> [Term | Acc]
		end, [], Tokens1))],
    {ok,Abs} = erl_parse:parse_exprs(Tokens),
    {value,Value,_} = erl_eval:exprs(Abs, erl_eval:new_bindings()),
	{matrice, Name, Value};

get_type([{atom,1,Name} | [{'=',1} | Expr]], Map) ->
    Tokens0 = lost_multiplication(Expr) ++ [{dot, 1}],
    Tokens = reassign_tokens(Tokens0, Map),
        try
		{ok, AbsForm} = erl_parse:parse_exprs(Tokens),
%%		io:format("AbsForm: ~p~n", [AbsForm]),
			try
				{value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
					if is_atom(Value) -> {reassign1, Name, AbsForm};
						true -> {variable, Name, Value}
					end
			catch
			    _:_  ->
					case lists:keyfind(atom, 1, Tokens) of
						{atom,1,i} -> {complex, Name, AbsForm};
						{atom,1,_} -> {reassign1, Name, AbsForm};
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

print_variable(_) -> ok.



%%arg_to_binary(Arg) ->
%%	try list_to_binary(Arg)
%%	catch _:_ -> error
%%	end.
%%
%%parse_expression(Input) ->
%%	Arg1 = binary:replace(Input, [<<" ">>], <<"">>, [global]),
%%	case binary:split(Arg1, [<<"=">>], []) of
%%		[Name, Expr] ->
%%			io:format("Name: ~p~nExpression: ~p~n", [Name, Expr]);
%%		E -> {error, E}
%%	end.
