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
%%			PropList = parse_expression(Bin),
%%			io:format("PropList: ~p~n", [PropList])
%%	end.

go() ->
	Arg = io:parse_erl_exprs('>'),
	case Arg of
		{ok,[{atom,1,q}],3} -> ok;
		_ ->
			io:format("Arg: ~p~n", [Arg]),
			go()
	end.
%%	parse_expression(Arg),
%%	exec(Arg).


%%====================================================================
%% Internal functions
%%====================================================================

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


%% Proplist = [
%%				{type, variable | complex | matrice | function},
%%				{name, Name},
%%				{value, map for each types},
%%				{reduce, binary or string}
%%			]
