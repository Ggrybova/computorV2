%%%-------------------------------------------------------------------
%%% @author ggrybova
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Nov 2018 4:31 PM
%%%-------------------------------------------------------------------
-module(complex_numbers).
-author("ggrybova").

%% API
-export([abs/1, add/2, multy_add/1, conjugate/1, divide/2, equal/2, exp/1, imaginary/1, mul/2, new/2,
	real/1, sub/2]).

abs({A, B}) ->
	math:sqrt(A * A + B * B).

add({A, B}, {C, D}) ->
	{A + C, B + D}.

multy_add([_|_] = List) ->
	lists:foldl(
		fun({Re, Im}, [[AccRe], [AccIm]]) -> [[AccRe + Re], [AccRe + Im]]
		end, [[], []], List);

multy_add(_) -> [].

conjugate({A, B}) ->
	{A, -B}.

divide({A, B}, {C, D}) ->
	{(A * C + B * D) / (C * C + D * D),
		(B * C - A * D) / (C * C + D * D)}.

equal({A, B}, {C, D}) ->
	(A =:= C) andalso (B =:= D).

exp({A, B}) ->
	{math:exp(A) * math:cos(B),
		math:exp(A) * math:sin(B)}.

imaginary({_A, B}) -> B.

mul({A, B}, {C, D}) ->
	{A * C - B * D, B * C + A * D}.

new(R, I) -> {R, I}.

real({A, _B}) -> A.

sub({A, B}, {C, D}) ->
	{A - C, B - D}.