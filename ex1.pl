%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - EXERCICIO 1

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}

utente(0,jose,20,bla).
utente(1,joao,21,bla).
utente(2,manuel,22,bla).
utente(3,carlos,23,bla).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado Cuidado Prestado: IdServ, Descricao, Inst, Cidade -> {V,F}

cuidado_prestado(0, radiografia, atal, braga).
cuidado_prestado(1, eletrocardiograma, atal, braga).
cuidado_prestado(2, cirurgia, outra, guima).
cuidado_prestado(3, oncologia, clone, porto).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado ato medico: Data, IdUt, IdServ, Custo -> {V,F}

ato_medico(03/03/2017, 3, 3, 30).
ato_medico(03/03/2017, 1, 2, 30).
ato_medico(03/03/2017, 0, 1, 30).
ato_medico(03/03/2017, 2, 2, 30).
ato_medico(03/03/2017, 1, 3, 30).
ato_medico(03/03/2017, 2, 0, 30).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado utente_select: ??? -> {V,F}
%
% indentificar utentes por critérios de selecao --- COMO ASSIM?
%

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado instituicoes: R -> {V,F}

instituicoes(R) :- solucoes((Inst),cuidado_prestado(Id,Desc,Inst,Cid),R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado cuidados_cidade: C, R -> {V,F}

%
% "identificar cuidados prestados por instituicao/cidade" 
%  não sei bem o que stor quer, se é como está aqui ou se quer a listagem por instituicao/cidade
%

cuidados(instituicao,I,R) :- solucoes((Descr), cuidado_prestado(Id,Descr,I,Cid), R).
cuidados(cidade,C,R) :- solucoes((Inst, Descr), cuidado_prestado(Id,Descr,Inst,C), R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado utentes_inst: I, R -> {V,F}
%
%utentes_inst(I,R) :- solucoes((IdServ), cuidado_prestado(IdServ,Descr,I,Cidade), N),
%					 idsUt(N, K),
%					 uts(K,R).
%
%idsUt([],[]).
%idsUt([Id|T], L) :- solucoes((IdUt), ato_medico(Data,IdUt,IdServ,Custo), K),
%					concat(K,L,N),
%					idsUt(T,N).
%
%uts([],[]).
%uts([Id|T], L) :- utente(Id,Nome,_,_), uts(T, K), L is [Nome|K].
%

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado concatenar: L1,L2,R -> {V,F}

concat([], L2, L2).
concat([H|T], L2, [H|L]) :- concat(T,L2,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado comprimento: L,N -> {V,F}

comprimento([], 0).
comprimento([H|T], N) :- comprimento(T,K), N is K+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado nao: Q -> {V,F}

nao(Q) :- Q, !, fail.
nao(Q).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado solucoes: F, Q, S -> {V,F}

solucoes(F,Q,S) :- Q, assert(tmp(F)), fail.
solucoes(F,Q,S) :- construir(S, []).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extencao do predicado construir: S1,S2 -> {V,F}

construir(S1, S2) :- retract(tmp(X)), 
					 !, 
					 construir(S1, [X|S2]).
construir(S,S).
