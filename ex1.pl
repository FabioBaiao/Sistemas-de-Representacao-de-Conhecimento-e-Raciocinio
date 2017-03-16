%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - EXERCICIO 1


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic cuidado_prestado/4.
:- dynamic ato_medico/4.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}

utente(0,jose,20,bla).
utente(1,joao,21,bla).
utente(2,manuel,22,bla).
utente(3,carlos,23,bla).

% Invariante:  nao permitir a insercao de conhecimento
%                         repetido

+utente(Id,Nome,Idade,Morada) :: (solucoes((Id),utente(Id,X,Y,Z),S),
								  comprimento(S,N),
								  N == 1
								 ).

% Invariante para impedir que se remova um utente enquanto existirem atos medicos associados a si
-utente(Id, Nome, Idade, Morada) :: ( solucoes((Id), ato_medico(D,Id, IdServ, C), S),
									  comprimento(S,N),
									  N == 0
									).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado_prestado: IdServ, Descricao, Inst, Cidade -> {V,F}
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
%
% Instituicoes que prestam cuidados médicos
%

instituicoes(R) :- solucoes((Inst),cuidado_prestado(Id,Desc,Inst,Cid),R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidados: I,C,R -> {V,F}
%
% "identificar cuidados prestados por instituicao/cidade" 
% segundo o Paulo Novais é este tipo de resolucao que é pedido com o "instituicao/cidade"

cuidados(I,C,R) :- solucoes((Descr), cuidado_prestado(Id,Descr,I,C), R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utentes_inst_serv: I,Serv,R -> {V,F}
%
% Identificar os utentes de uma instituicao/servico
% se a identificacao do servico for por ID entao podemos retirar a primeira verificacao
%

utentes_inst_serv(I,Serv, R) :- cuidado_prestado(IdServ,Serv,I,_),
								solucoes((IdUt), ato_medico(_,IdUt,IdServ,_), K),
								uts(K,R).

/*
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado idsUt: ListaID, R -> {V,F}
%
%	ListaID - lista de ID's de servicos
%	R		- Lista resultante com todos os utentes que recorreram ao servicoes em ListaID
%
idsUt([],[]).
idsUt([Id|T], R) :- solucoes((IdUt), ato_medico(_,IdUt,Id,_), K),
					idsUt(T,X),
					concat(K,X,R).
*/
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado uts: Ids,Nomes -> {V,F}
%
% Converte uma lista de ids de utentes para uma lista dos nomes desses
% utentes
%
% Se permitirmos remover utentes sem precisar de remover atos medicos
% relacionados com ele, então isto nao deve funcionar
% Caso seja necessarios apagar os atos medicos de utente para o remover
% entao isto nao tem problema porque se existem atos medicos os ids de
% utentes sao validos e existem.
%

uts([],[]). 
uts([Id|T], R) :- utente(Id,Nome,_,_), uts(T,N), R = [Nome|N].

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado atos_medicos: IdUt,Inst,Serv,R -> {V,F}
%
% Identificar os atos medicos realizados, por utente/instituicao/servico
%

atos_medicos(IdUt, Inst, Serv, R) :- cuidado_prestado(IdServ,Serv,Inst,_),
									 solucoes((Data,Custo), ato_medico(Data,IdUt,IdServ,Custo), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado recorreu: IdUt,R -> {V,F}
%
% Identificar todas as instituicoes/servicos a que um utente
%

recorreu(IdUt, R) :- solucoes((IdServ), ato_medico(_,IdUt,IdServ,_), K),
					 instServTuplos(K,R).

instServTuplos([],[]).
instServTuplos([Id|T],R) :- solucoes((Inst,Serv), cuidado_prestado(Id,Serv,Inst,_), K),
							instServTuplos(T,X),
							concat(K,X,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado custo: IdUt,Serv,Inst,Data,R -> {V,F}
%
% Calcular o custo dos atos medicos por utente/servico/instituicao/data

custo(IdUt, Serv, Inst, Data, Custo) :- solucoes(C, (ato_medico(Data, IdUt, Serv, C), cuidado_prestado(Serv,_,Inst,_)), S),
										sum(S, Custo).

%custo(IdUt,Serv,Inst,Data,R) :- solucoes((IdServ), cuidado_prestado(IdServ,Serv,Inst,_), K),
%								teste(IdUt,Data,K, R).

%teste(_,_,[],0).
%teste(IdUt,Data,[Id,T],R) :- solucoes((Custo), ato_medico(Data,IdUt,Id,_), K),
%							 teste(IdUt,Data,T,X),
%							 sum(K,N),
%							 R is N+X.

sum([],0).
sum([N|Ns], T) :- sum(Ns,X), T is X+N.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concatenar: L1,L2,R -> {V,F}

concat([], L2, L2).
concat([H|T], L2, [H|L]) :- concat(T,L2,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: L,N -> {V,F}

comprimento([], 0).
comprimento([H|T], N) :- comprimento(T,K), N is K+1.

% comprimento(L,N) :- length(L,N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado nao: Q -> {V,F}

nao(Q) :- Q, !, fail.
nao(Q).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes: F, Q, S -> {V,F}

solucoes(F,Q,S) :- Q, assert(tmp(F)), fail.
solucoes(F,Q,S) :- construir(S, []).

% solucoes(F,Q,S) :- findall(F,Q,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado construir: S1,S2 -> {V,F}

construir(S1, S2) :- retract(tmp(X)), 
					 !, 
					 construir(S1, [X|S2]).

construir(S,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao(Termo) :- solucoes(Inv, +Termo::Inv, LInv),
				   assert(Termo),
				   testa(LInv).
evolucao(Termo) :- retract(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a involucao do conhecimento

involucao(Termo) :- Termo,
					solucoes(Inv, -Termo::Inv, LInv),
					assert(temp(Termo)),
					retract(Termo),
					testa(LInv),
					retract(temp(Termo)).
involucao(Termo) :- temp(X), retract(temp(X)), assert(X), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que testa uma lista de invariantes

testa([]).
testa([I|T]) :- I, testa(T).
