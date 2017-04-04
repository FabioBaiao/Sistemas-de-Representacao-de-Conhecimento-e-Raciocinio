%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao genealogica.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais
:- op( 900, xfy, '&' ). % operador para conjuncao de questoes
:- op( 900, xfy, '::' ).

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
% Extensao do meta-predicado demoC :: Conjunção, Resposta -> {V,F}
demoC( P & X, verdadeiro ) :- demo( P, verdadeiro ), demoC( X, verdadeiro ).
demoC( P & X, falso ) :- demo( P, falso ).
demoC( P & X, falso ) :- demoC( X, falso ).
demoC( P & X, desconhecido ) :- demo( P, desconhecido ), demoC( X, desconhecido ).
demoC( P & X, desconhecido ) :- demo( P, desconhecido ), demoC( X, verdadeiro ).
demoC( P & X, desconhecido ) :- demo( P, verdadeiro ), demoC( X, desconhecido ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo :: Conjunção, Resposta -> {V,F}
demo( P , verdadeiro ) :- P.
demo( P, falso ) :- -P.
demo( P, desconhecido ) :- nao( P ), nao( -P ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado evolucao :: ?? 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado involucao :: ?? 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes :: ?? 
