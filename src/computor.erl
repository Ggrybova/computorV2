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
    main/1
]).

main(String) ->
	case arg_to_binary(String) of
		error -> io:format("Error. Not valid input.~n");
		Bin ->
			io:format("Input: ~p~n", [Bin]),
			PropList = parse_expression(Bin),
			io:format("PropList: ~p~n", [PropList])
	end.

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


%% Proplist = [
%%				{type, variable | complex | matrice | function},
%%				{name, Name},
%%				{value, map for each types},
%%				{reduce, binary or string}
%%			]
