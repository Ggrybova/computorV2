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
                    case Type of
                        variable ->
                            if is_float(Value) -> {true, {float,1,Value}};
                                true -> {true, {integer,1,Value}}
                            end;
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
execute_token({complex,Name,[{op,1,'*',{integer,1,Val},{atom,1,i}}]}, _) -> {Name, [{type, complex},{value, {0, Val}}]};
execute_token({complex,Name,[{op,1,'*',{op,1,'+',{integer,1,Val}},{atom,1,i}}]}, _) -> {Name, [{type, complex},{value, {0, Val}}]};
execute_token({complex,Name,[{op,1,'*',{op,1,'-',{integer,1,Val}},{atom,1,i}}]}, _) -> {Name, [{type, complex},{value, {0, -1*Val}}]};

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

execute_token(_, _) -> io:format("ERROR: Wrong input data!~n").

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
			try
				{value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
					case is_atom(Value) of
                        true when Value == i -> {complex, Name, AbsForm};
                        true -> {reassign1, Name, AbsForm};
						_ -> {variable, Name, Value}
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
