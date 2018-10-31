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
				{ok, Token, N} ->
					io:format("~p~n", [Token]),
                    execute_token(Token);
				{error, R} ->
					io:format("Error format: ~p~n", [R])
			end,
			go()
	end.
%%	parse_expression(Arg),
%%	exec(Arg).

%%io:get_line("> ").
%%io:parse_erl_exprs('>')

%%====================================================================
%% Internal functions
%%====================================================================

execute_token(Token) ->
    X = get_type(Token),
    case X of
        {variable, Name, Expr}  -> io:format("~p~n", [X]);
        {complex, Name, Expr}   -> io:format("~p~n", [X]);
        {matrice, Name, Expr}   -> io:format("~p~n", [X]);
        {function, Name, Expr}  -> io:format("~p~n", [X]);
        _ -> io:format("ERROR Type~n")
    end.

get_type([{atom,1,Name} | [{'(',1} | [{atom,1,Var} | [{')',1} | [{'=',1} | Expr]]]]]) -> {function, {Name, Var}, Expr};
get_type([{atom,1,Name} | [{'=',1} | [{'[',1} | Expr]]]) -> {matrice, Name, Expr};
get_type([{atom,1,Name} | [{'=',1} | Expr]]) -> {variable, Name, Expr};
get_type(_) -> ok.


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
