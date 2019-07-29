:- encoding(utf8).

:- use_module(library(clpfd)).
:- use_module(library(yall)).

%:- use_module(library(dcg/basics)).
%:- set_prolog_flag(double_quotes, string).
% TODO: use `phrase` (DCG) 

matrix_cols([ColN], [ColN]).
matrix_cols(M, [Col0|ColsB]) :-
    M = [Col0|ColsA],
    matrix_cols([ColsA], [ColsB]).
	
matrix_rows(Cols, [TrRowN]) :-
	maplist({}/[Col, ColEN]>>(Col=[ColEN]), Cols, TrRowN).
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

zeros1(N, [ZerosCol]) :-
	length(ZerosCol, N),
	maplist({}/[E]>>(E #= 0), ZerosCol).
zeros(Nrect, Cols) :-
	length(Cols, Nrect),
	maplist({Nrect}/[Col]>>zeros1(Nrect, [Col]), Cols).
 
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

sum1(ACol, BCol, RCol) :-
	maplist({}/[A, B, R]>>expr(#+, A, B, R), ACol, BCol, RCol).

sum([], [], []).
sum(ACols, BCols, RCols) :-
    maplist(sum1, ACols, BCols, RCols).

dot([An], [Bn], Res) :-
	Res #= An*Bn.
dot([A0|As], [B0|Bs], Res) :-
	dot(As, Bs, Res1n),
	dot([A0], [B0], Res0),
	Res #= Res0 + Res1n.

mul1([Col], M2, [NCol]) :-
    matrix_rows(M2, Rows),
    maplist({Col}/[RowJ, ColEJ]>>dot(Col, RowJ, ColEJ), Rows, NCol).
    
mul([], _, []).
mul(Cols, M2, NCols) :-
	maplist({M2}/[Col, NCol]>>mul1([Col], M2, [NCol]), Cols, NCols).
	