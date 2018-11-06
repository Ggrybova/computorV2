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
        "clean\n" -> go(#{});
		_ ->
			Arg =
				case lists:member($%, Arg0) of
					true ->
						ArgBin0 = list_to_binary(Arg0),
						ArgBin = binary:replace(ArgBin0, <<"%">>, <<" rem ">>),
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
%%	1.	{ok,Tokens,_EndLine} = erl_scan:string("{1,2,[hello]}."),
%%	2.	{ok,AbsForm} = erl_parse:parse_exprs(Tokens),
%%	3.	{value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
%%Value.

%%====================================================================
%% Internal functions
%%====================================================================

reassign_tokens(Tokens, Map) ->
    NewTokens = lists:filtermap(
        fun({atom,1,Var}) ->
            case maps:get(Var, Map, undefined) of
                undefined -> {true, {atom,1,Var}};
                Obj ->
                    Type = proplists:get_value(type, Obj),
                    Value = proplists:get_value(value, Obj),
                    io:format("     Type = ~p, Value = ~p~n", [Type, Value]),
                    case Type of
                        variable ->
                            io:format("111~n"),
                            if is_float(Value) -> {true, {float,1,Value}};
                                true -> {true, {integer,1,Value}}
                            end;
                        _ -> io:format("222~n"),{true, {atom,1,Var}}
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

lost_multiplication({integer,1,Int}, [{atom,1,Atom}|T], Acc) ->
    lost_multiplication({atom,1,Atom}, T, [{atom,1,Atom} | [{'*',1}| Acc]]);

lost_multiplication(_, [P|T], Acc) -> lost_multiplication(P, T, [P|Acc]).

execute_token({variable,Name, Expr}, _) ->
	Proplist = [
		{type, variable},
		{value, Expr}
	],
	{Name, Proplist};

execute_token({reassign1,Name, Expr}, _) ->
    Proplist = [
        {type, reassign1},
        {value, Expr}
    ],
    {Name, Proplist};

execute_token({matrice,	Name, Expr} = Res, _) ->
	io:format("!!!	Res = ~p~n", [Res]);

execute_token(Token, _) ->
    case Token of
%%        {variable,	Name, Expr} = Token -> io:format("!!!	Res = ~p~n", [Token]);
        {complex1,	Name, Expr} = Token -> io:format("!!!	Res = ~p~n", [Token]);
		{complex2,	Name, Expr} = Token -> io:format("!!!	Res = ~p~n", [Token]);
		{reassign1,	Name, Expr} = Token -> io:format("!!!	Res = ~p~n", [Token]);
		{reassign2,	Name, Expr} = Token -> io:format("!!!	Res = ~p~n", [Token]);
%%        {matrice,	Name, Expr} = Token -> io:format("!!!	Res = ~p~n", [Token]);
        {function,	Name, Expr} = Token -> io:format("!!!	Res = ~p~n", [Token]);
        Res -> io:format("ERROR Type: ~p~n", [Res])
    end.

get_type([{atom,1,Name} | [{'(',1} | [{atom,1,Var} | [{')',1} | [{'=',1} | Expr]]]]], _) -> {function, {Name, Var}, Expr};
get_type([{atom,1,Name} | [{'=',1} | [{'[',1} | Expr]]], _) -> {matrice, Name, Expr};
get_type([{atom,1,Name} | [{'=',1} | Expr]], Map) ->
    Tokens0 = lost_multiplication(Expr) ++ [{dot, 1}],
	io:format("Tokens0: ~p~n", [Tokens0]),
    Tokens = reassign_tokens(Tokens0, Map),
    io:format("Tokens: ~p~n", [Tokens]),
        try

		{ok, AbsForm} = erl_parse:parse_exprs(Tokens),
		io:format("AbsForm: ~p~n", [AbsForm]),
			try
				{value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
					if is_atom(Value) -> {reassign1, Name, AbsForm};
						true -> {variable, Name, Value}
					end
			catch
			    _:_  ->
					case lists:keyfind(atom, 1, Tokens) of
						{atom,1,i} -> {complex1, Name, AbsForm};
						{atom,1,_} -> {reassign1, Name, AbsForm};
						_ -> {expression1, Name, AbsForm}
					end
			end
	catch
		_:_ ->
			case lists:keyfind(atom, 1, Tokens) of
				{atom, 1, i} -> {complex2, Name, Tokens};
				{atom, 1, _} -> {reassign2, Name, Tokens};
				_ -> {expression2, Name, Tokens}
			end
	end;

get_type(X, _) -> X.

print_variable([{type, variable},{value, Value}]) ->
	io:format("~p~n", [Value]);

print_variable(_) -> ok.


arg_to_binary(Arg) ->
	try list_to_binary(Arg)
	catch _:_ -> error
	end.

parse_expression(Input) ->
	Arg1 = binary:replace(Input, [<<" ">>], <<"">>, [global]),
	case binary:split(Arg1, [<<"=">>], []) of
		[Name, Expr] ->
			io:format("Name: ~p~nExpression: ~p~n", [Name, Expr]);
		E -> {error, E}
	end.


%% Map = {var1, prplist1}
%%
%% Proplist = [
%%				{type, variable | complex | matrice | function},
%%				{name, Name},
%%				{value, map for each types},
%%				{reduce, binary or string}
%%			]
