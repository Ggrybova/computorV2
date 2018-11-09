%%%-------------------------------------------------------------------
%%% @author ggrybova
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2018 19:16
%%%-------------------------------------------------------------------
-module(my_lib).
-author("ggrybova").

-export([
    binary_to_number/1,
    my_sqrt/1,
    my_pow/2
]).

%%====================================================================
%% API
%%====================================================================

%% F = fun(X, Y) -> my_lib:my_pow(X, Y) end.
%% F(3,4).

binary_to_number(Bin) ->
    try
        binary_to_float(Bin)
    catch
        _:_ ->
            try
                binary_to_integer(Bin)
            catch
                _:_ ->
                    io:format("Format error: ~p", [Bin]),
                    halt()
            end
    end.

my_sqrt(0) -> 0;
my_sqrt(1) -> 1;
my_sqrt(D) ->
    my_sqrt(D, D/2).
my_sqrt(D, A) ->
    A2 = 0.5*(A + D/A),
    Sub = A2 - A,
    Sub2 =
        if Sub < 0 -> Sub * -1;
            true -> Sub
        end,
    if Sub2 < 0.000001 -> A2;
        true -> my_sqrt(D, A2)
    end.

my_pow(0, _) -> 0;
my_pow(_, 0) -> 1;
my_pow(Var, Pow) ->
    my_pow(Var, Pow, Var).

my_pow(_, 1, Acc) -> Acc;
my_pow(Var, Pow, Acc) -> my_pow(Var, Pow - 1, Acc * Var).

