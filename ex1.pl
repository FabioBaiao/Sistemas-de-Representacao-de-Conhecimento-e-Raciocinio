%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - EXERCICIO 1

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}

utente(0,jose,20,bla).
utente(1,joao,21,bla).
utente(2,manuel,22,bla).
utente(3,carlos,23,bla).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado Cuidado Prestado: IdServ, Descricao, Inst, Cidade -> {V,F}

cuidado_prestado(0, radiografia, atal, braga).
cuidado_prestado(1, eletrocardiograma, atal, braga).
cuidado_prestado(2, cirurgia, outra, guima).
cuidado_prestado(3, oncologia, clone, porto).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ato medico: Data, IdUt, IdServ, Custo -> {V,F}

ato_medico(03/03/2017, 3, 3, 30).
ato_medico(03/03/2017, 1, 2, 30).
ato_medico(03/03/2017, 0, 1, 30).
ato_medico(03/03/2017, 2, 2, 30).
ato_medico(03/03/2017, 1, 3, 30).
ato_medico(03/03/2017, 2, 0, 30).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente_select: ??? -> {V,F}
%
% indentificar utentes por critérios de selecao --- COMO ASSIM?
%

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado instituicoes: R -> {V,F}

instituicoes(R) :- solucoes((Inst),cuidado_prestado(Id,Desc,Inst,Cid),R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidados_cidade: C, R -> {V,F}

%
% "identificar cuidados prestados por instituicao/cidade" 
%  não sei bem o que stor quer, se é como está aqui ou se quer a listagem por instituicao/cidade
%

cuidados(instituicao,I,R) :- solucoes((Descr), cuidado_prestado(Id,Descr,I,Cid), R).
cuidados(cidade,C,R) :- solucoes((Inst, Descr), cuidado_prestado(Id,Descr,Inst,C), R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utentes_inst: I, R -> {V,F}
/*
utentes_inst(I,R) :- solucoes((IdServ), cuidado_prestado(IdServ,Descr,I,Cidade), N),
					 idsUt(N, K),
					 uts(K,R).

idsUt([],[]).
idsUt([Id|T], L) :- solucoes((IdUt), ato_medico(Data,IdUt,Id,Custo), K),
					concat(K,L,N),
					idsUt(T,N).
*/

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado uts: Ids,Nomes -> {V,F}
%
% Se permitirmos remover utentes sem precisar de remover atos medicos
% relacionados com ele, então isto nao deve funcionar
% Caso seja necessarios apagar os atos medicos de utente para o remover
% entao isto nao tem problema porque se existem atos medicos os ids de
% utentes sao validos e existem.
%

nome_ut(Id, Nome) :- utente(Id, Nome,_,_).

uts([],[]).
uts([I|T], L) :- utente(I,Nome,_,_), uts(T, K), L is [R|K].

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concatenar: L1,L2,R -> {V,F}

concat([], L2, L2).
concat([H|T], L2, [H|L]) :- concat(T,L2,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: L,N -> {V,F}

comprimento([], 0).
comprimento([H|T], N) :- comprimento(T,K), N is K+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado nao: Q -> {V,F}

nao(Q) :- Q, !, fail.
nao(Q).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes: F, Q, S -> {V,F}

solucoes(F,Q,S) :- Q, assert(tmp(F)), fail.
solucoes(F,Q,S) :- construir(S, []).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado construir: S1,S2 -> {V,F}

construir(S1, S2) :- retact(tmp(X)), 
					 !, 
					 construir(S1, [X|S2]).
construir(S,S).
