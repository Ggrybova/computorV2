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
	go().
%%	case arg_to_binary(String) of
%%		error -> io:format("Error. Not valid input.~n");
%%		Bin ->
%%			io:format("Input: ~p~n", [Bin]),
%%			PropList = parse_expression(Bin) ,
%%			io:format("PropList: ~p~n", [PropList])
%%	end.

go() ->
	Arg0 = io:get_line("> "),
	case Arg0 of
		"q\n" -> ok;
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
            io:format("Arg: ~p~n", [Arg]),
			case erl_scan:string(Arg) of
				{ok, Tokens, N} ->
%%					io:format("Token: ~p~n", [Token]),
                    execute_token(Tokens);
				{error, R} ->
					io:format("Error format: ~p~n", [R])
			end,
			go()
	end.
%%	parse_expression(Arg),
%%	exec(Arg).

%%io:get_line("> ").
%%io:parse_erl_exprs('>')
%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%	1.	{ok,Tokens,_EndLine} = erl_scan:string("{1,2,[hello]}."),
%%	2.	{ok,AbsForm} = erl_parse:parse_exprs(Tokens),
%%	3.	{value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
%%Value.

%%====================================================================
%% Internal functions
%%====================================================================

execute_token(Token) ->
    X = get_type(Token),
    case X of
        {variable,	Name, Expr} = Res -> io:format("!!!	Res = ~p~n", [Res]);
        {complex,	Name, Expr} = Res -> io:format("!!!	Res = ~p~n", [Res]);
		{complex2,	Name, Expr} = Res -> io:format("!!!	Res = ~p~n", [Res]);
		{reassign1,	Name, Expr} = Res -> io:format("!!!	Res = ~p~n", [Res]);
		{reassign2,	Name, Expr} = Res -> io:format("!!!	Res = ~p~n", [Res]);
        {matrice,	Name, Expr} = Res -> io:format("!!!	Res = ~p~n", [Res]);
        {function,	Name, Expr} = Res -> io:format("!!!	Res = ~p~n", [Res]);
        Res -> io:format("ERROR Type: ~p~n", [Res])
    end.

get_type([{atom,1,Name} | [{'(',1} | [{atom,1,Var} | [{')',1} | [{'=',1} | Expr]]]]]) -> {function, {Name, Var}, Expr};
get_type([{atom,1,Name} | [{'=',1} | [{'[',1} | Expr]]]) -> {matrice, Name, Expr};
get_type([{atom,1,Name} | [{'=',1} | Expr]]) ->
	Tokens = Expr ++ [{dot, 1}],
	io:format("Token: ~p~n", [Tokens]),
	try

		{ok, AbsForm} = erl_parse:parse_exprs(Tokens),
		io:format("AbsForm: ~p~n", [AbsForm]),
			try
				{value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
				{variable, Name, Value}
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

get_type(X) -> X.


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

%%validate_expression(Name, Expr) ->
%%	case get_type(Expr) of
%%		var ->
%%			io:format("Type = ~p~n", [var]);
%%		compl ->
%%			io:format("Type = ~p~n", [compl]);
%%		matr ->
%%			io:format("Type = ~p~n", [matr]);
%%		func ->
%%			io:format("Type = ~p~n", [func]);
%%		E ->
%%			io:format("Type = ~p~n", [E])
%%	end.
%%
%%get_type(Expr) ->
%%	try
%%		X =


%% Map = {var1, prplist1
%%
%% Proplist = [
%%				{type, variable | complex | matrice | function},
%%				{name, Name},
%%				{value, map for each types},
%%				{reduce, binary or string}
%%			]
