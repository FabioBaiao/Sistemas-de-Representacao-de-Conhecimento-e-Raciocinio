%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao genealogica.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais
:- op( 900, xfy, '&' ). % operador para conjuncao de questoes
:- op( 900, xfy, '::' ).

% :- dynamic utente/4.
% :- dynamic cuidado_prestado/4.
% :- dynamic ato_medico/4.

% :- dynamic profissional/4.
% :- dynamic atribuido/2.
% :- dynamic ato_medico/5.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes: F, Q, S -> {V,F}
solucoes(F,Q,S) :- demo( Q, verdadeiro ), assert( tmp(F) ), fail.
solucoes(F,Q,S) :- construir( S, [] ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado construir: S1,S2 -> {V,F}
construir(S1, S2) :-
	retract(tmp(X)), !, construir(S1, [X|S2]).
construir(S,S).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado utente :: IdUt, Nome, Idade, Morada -> {V,F,D}

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado cuidado_prestado :: IdServ, Descricao, Instituicao, Cidade -> {V,F,D}

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado ato_medico :: Data, IdUt, IdServ, Custo -> {V,F,D}

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao:: Questão -> {V,F}
nao( Q ) :- Q, !, fail.
nao( Q ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo :: Conjunção, Resposta -> {V,F}
demo( P & X, verdadeiro ) :- demo( P, verdadeiro ), demo( X, verdadeiro ).
demo( P & X, falso ) :- demo( P, falso ).
demo( P & X, falso ) :- demo( X, falso ).
demo( P & X, desconhecido ) :- demo( P, desconhecido ), demo( X, desconhecido ).
demo( P & X, desconhecido ) :- demo( P, desconhecido ), demo( X, verdadeiro ).
demo( P & X, desconhecido ) :- demo( P, verdadeiro ), demo( X, desconhecido ).

demo( P , verdadeiro ) :- P.
demo( P, falso ) :- -P.
demo( P, desconhecido ) :- nao( P ), nao( -P ).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado evolucao :: ?? 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado involucao :: ?? 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes :: Predicado  
