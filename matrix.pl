:- encoding(utf8).
:- use_module(library(clpfd)).
:- use_module(library(yall)).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, chars).

% TODO: use `phrase` (DCG) 

matrix_cols([ColN|[]], [ColN|[]]) :- !.
matrix_cols(M, [Col0|ColsB]) :-
    M = [Col0|ColsA], matrix_cols([ColsA], [ColsB]).

all_empty([[]]).
all_empty([[]|Es]) :- all_empty(Es).

matrix_rows([[]|RowEnds], []) :- all_empty(RowEnds), !.
matrix_rows(Cols, [TrRow0|Rs1_N]) :-
    maplist({}/[Col, ColE0, ColEs]>>(Col=[ColE0|ColEs]), Cols, TrRow0, ColsEs),
	matrix_rows(ColsEs, Rs1_N).

matrix_fmt(M) :-
    matrix_rows(M, Rows),
    maplist({}/[R]>>writeln(R), Rows).

transpose(M, Mt) :-
	%clpfd:transpose(M, Mt).
	matrix_rows(Mt, M).


identity(1, [[1]]).
identity(2, [[1,0],[0,1]]).
identity(3, [[1,0,0],[0,1,0],[0,0,1]]).
identity(4, [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]).
  
zeros(Nrect, M0) :-
    identity(Nrect, M1),
    pointwise(M1, #*, 0, M0).
 
expr(#*, X, Y, Res) :- Res #= X*Y.
expr(#+, X, Y, Res) :- Res #= X+Y.
expr(#-, X, Y, Res) :- Res #= X-Y.
expr(#/, X, Y, Res) :- Res #= X div Y.
expr(#//, X, Y, Res) :- Res #= X mod Y.

expr(*, X, Y, Res) :- Res is X*Y.
expr(+, X, Y, Res) :- Res is X+Y.
expr(-, X, Y, Res) :- Res is X-Y.
expr(/, X, Y, Res) :- Res is X rdiv Y.


pointwise([], _, _, []).
pointwise([Col0|Cols], Op, Scalar, [ResCol0|ResCols]) :-
    maplist({Op, Scalar}/[E, Eres]>>expr(Op, E, Scalar, Eres), Col0, ResCol0),
    pointwise(Cols, Op, Scalar, ResCols).
    
mul(_M1, _M2) :-
	false.