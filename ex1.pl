%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - EXERCICIO 1


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op(900, xfy, '::').
:- dynamic utente/4.
:- dynamic cuidado_prestado/4.
:- dynamic ato_medico/4.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}

utente(0, jose, 55, 'Rua dos Zecas').
utente(1, joao, 21, 'Rua de Baixo').
utente(2, manuel, 36, 'Rua Maria Albertina').
utente(3, carlos, 43, 'Rua da Fabrica').

% Invariante estrutural: nao permitir a insercao de conhecimento
%                        repetido
+utente(IdUt, _, _, _) :: (
	solucoes(IdUt, utente(IdUt, _, _, _), S),
	comprimento(S, N),
	N == 1
).

% Invariante ??????: a idade de cada utente tem de ser inteira e
%                    estar no intervalo fechado [0,150]
+utente(_, _, Idade, _) :: (
	integer(Idade),
	Idade >= 0, Idade =< 150
).

% Invariante referencial: nao permitir que se remova um utente enquanto
%                         existirem atos medicos associados a si
-utente(IdUt, _, _, _) :: (
	solucoes(IdUt, ato_medico(_, IdUt, _, _), S),
	comprimento(S, N),
	N == 0
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado_prestado: IdServ, Descricao, Instituicao, Cidade -> {V,F}

cuidado_prestado(0, radiografia, atal, braga).
cuidado_prestado(1, eletrocardiograma, atal, braga).
cuidado_prestado(2, cirurgia, outra, guimaraes).
cuidado_prestado(3, oncologia, clone, porto).

% Invariante estrutural: nao permitir a insercao de conhecimento
%                        repetido
+cuidado_prestado(IdServ, _, _, _) :: (
	solucoes(IdServ, cuidado_prestado(IdServ, _, _, _), S),
	comprimento(S, N),
	N == 1
).

% Invariante estrutural: nao permitir cuidados prestados com a mesma descricao,
%                        na mesma instituicao e cidade

+cuidado_prestado(_, Descr, Inst, Cidade) :: (
	solucoes((Descr, Inst, Cidade), cuidado_prestado(_, Descr, Inst, Cidade), S),
	comprimento(S, N),
	N == 1
).

% Invariante referencial: nao permitir que se remova um cuidado prestado enquanto
%                         existirem atos medicos a ele associados
-cuidado_prestado(IdServ, _, _, _) :: (
	solucoes(IdServ, ato_medico(_, _, IdServ, _), S),
	comprimento(S, N),
	N == 0
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ato_medico: Data, IdUt, IdServ, Custo -> {V,F}

ato_medico(data(3,3,2017), 3, 3, 30).
ato_medico(data(4,3,2017), 1, 2, 30).
ato_medico(data(5,3,2017), 0, 1, 30).
ato_medico(data(6,3,2017), 2, 2, 30).
ato_medico(data(7,3,2017), 1, 3, 30).
ato_medico(data(8,3,2017), 2, 0, 30).

% Invariante referencial: nao permitir a insercao de atos medicos
%                         relativos a servicos ou utentes inexistentes
+ato_medico(_, IdUt, IdServ, _) :: (
	solucoes(IdUt, utente(IdUt, _, _, _), R1),
	solucoes(IdServ, cuidado_prestado(IdServ, _, _, _), R2),
	comprimento(R1, N),
	comprimento(R2, M),
	N == 1, M == 1
).

% Invariante ???????: o primeiro argumento do ato_medico tem de
%                     ser o predicado data
+ato_medico(X, _, _, _) :: (e_data(X)).

% Invariante ???????: o custo dos atos medicos tem de ser um numero
%                     nao negativo
+ato_medico(_, _, _, Custo) :: (number(Custo), Custo >= 0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado data: D, M, A -> {V,F}

data(D, M, A) :-
	pertence(M, [1,3,5,7,8,10,12]),
	D >= 1,
	D =< 31.
data(D, M, A) :-
	pertence(M, [4,6,9,11]),
	D >= 1,
	D =< 30.
data(D, 2, A) :-
	A mod 4 =\= 0, % ano nao bissexto
	D >= 1,
	D =< 28.
data(D, 2, A) :-
	A mod 4 =:= 0,
	D >= 1,
	D =< 29.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado e_data: X -> {V,F}

e_data(data(D, M, A)) :- data(D, M, A).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado selecionar_utentes: IdUt, Nome, Idade, Morada, R -> {V,F}

selecionar_utentes(IdUt, Nome, Idade, Morada, R) :- 
	solucoes((IdUt, Nome, Idade, Morada), utente(IdUt, Nome, Idade, Morada), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado instituicoes: R -> {V,F}
%
% Instituicoes que prestam cuidados medicos em qualquer cidade

%instituicoes(R) :-
%	solucoes((Inst, Cidade), cuidado_prestado(Id, Descr, Inst, Cidade), L),
%	unicos(L,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado instituicoes: Cidade, R -> {V,F}
%
% Instituicoes que prestam cuidados medicos

instituicoes(Cidade, R) :-
	solucoes((Inst, Cidade), cuidado_prestado(_, _, Inst, Cidade), L),
	unicos(L,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidados: Instituicao, Cidade, R -> {V,F}
%
% Identificar os cuidados prestados por instituicao/cidade
% (segundo o Paulo Novais é este tipo de resolucao que é pedido com o "instituicao/cidade")

cuidados(Inst, Cidade, R) :-
	solucoes((Descr, Inst, Cidade), cuidado_prestado(_, Descr, Inst, Cidade), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utentes_inst_serv: Instituicao, Servico, R -> {V,F}
%
% Identificar os utentes de uma instituicao/servico
% se a identificacao do servico for por ID entao podemos retirar a primeira verificacao

%utentes_inst_serv(Inst, Serv, R) :-
%	solucoes(IdUt,
%		 (cuidado_prestado(IdServ, Serv, Inst, _),
%		     ato_medico(_, IdUt, IdServ, _)),
%		 Ids),
%	uts(Ids,L),
%	unicos(L,R).

% ALTERNATIVA

utentes_inst_serv(Inst, Serv, R) :-
	solucoes(Nome,
		 (cuidado_prestado(IdServ, Serv, Inst, _),
		     ato_medico(_, IdUt, IdServ, _),
		     utente(IdUt, Nome, _, _)),
		 L),
	unicos(L,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado recorreu: IdUt,R -> {V,F}
%
% Identificar todas as instituicoes/servicos a que um utente ja recorreu

%recorreu(IdUt, R) :-
%	solucoes(IdServ, ato_medico(_, IdUt, IdServ, _), K),
%	inst_serv_tuplos(K,L),
%	unicos(L,R).

% ALTERNATIVA
recorreu(IdUt, R) :-
	solucoes((Inst,Serv),
		 (ato_medico(_, IdUt, IdServ, _),
		     cuidado_prestado(IdServ, Serv, Inst, _)),
		 L),
	unicos(L,R).

% Se for para usar a alternativa este predicado torna-se desnecessario
% inst_serv_tuplos([],[]).
% inst_serv_tuplos([Id|T], R) :-
%	solucoes((Inst, Serv), cuidado_prestado(Id, Serv, Inst, _), K),
%	inst_serv_tuplos(T,X),
%	concat(K,X,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado atos_medicos: IdUt, Instituicao, Servico, R -> {V,F}
%
% Identificar os atos medicos realizados por utente/instituicao/servico
%

atos_medicos(IdUt, Inst, Serv, R) :-
	solucoes((Data, IdUt, Serv, Inst, Custo),
		 (cuidado_prestado(IdServ, Serv, Inst, _),
		     ato_medico(Data, IdUt, IdServ, Custo)),
		 R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado custo: IdUt,Serv,Inst,Data,R -> {V,F}
%
% Calcular o custo dos atos medicos por utente/servico/instituicao/data
%

custo(IdUt, Serv, Inst, Data, R) :-
	solucoes(
		C,
		(
			ato_medico(Data, IdUt, IdServ, Custo),
		    cuidado_prestado(IdServ, Serv, Inst,_)
		),
		S
	),
	soma(S, R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado
% atos_medicos_interv: IdUt, Instituicao, Servico, DataIni, DataFim, R -> {V,F}
%
% Identificar os atos medicos realizados por utente/instituicao/servico
% num intervalo de datas

atos_medicos_interv(IdUt, Inst, Serv, Di, Df, R) :-
	solucoes(
		(Data, IdUt, Serv, Inst, Custo),
		(
			cuidado_prestado(IdServ, Serv, Inst, _),
			ato_medico(Data, IdUt, IdServ, Custo),
			nao(cmp_datas(Data, Di, <)),
			nao(cmp_datas(Data, Df, >))
		),
		R
	).
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

%uts([],[]). 
%uts([Id|T], [Nome|Ns]) :- utente(Id,Nome,_,_), uts(T,Ns).

%custo(IdUt,Serv,Inst,Data,R) :- solucoes((IdServ), cuidado_prestado(IdServ,Serv,Inst,_), K),
%								teste(IdUt,Data,K, R).

%teste(_,_,[],0).
%teste(IdUt,Data,[Id,T],R) :- solucoes((Custo), ato_medico(Data,IdUt,Id,_), K),
%							 teste(IdUt,Data,T,X),
%							 soma(K,N),
%							 R is N+X.

soma([],0).
soma([N|Ns], T) :- soma(Ns,X), T is X+N.

%------------------------
% Extensao do predicado pertence: X, L -> {V,F}

pertence(H,[H|T]).
pertence(X,[H|T]) :-
	X \= H,
	pertence(X,T).

%-----------------------------------------------
% Extensao do predicado unicos: L, R -> {V,F}

unicos([],[]).
unicos([H|T], [H|R]) :-
	unicos(T,R),
	nao(pertence(H,R)).
unicos([H|T], R) :-
	unicos(T,R),
	pertence(H,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concat: L1,L2,R -> {V,F}

concat([], L2, L2).
concat([H|T], L2, [H|L]) :- concat(T,L2,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: L,N -> {V,F}

comprimento([], 0).
comprimento([H|T], N) :- comprimento(T,K), N is K+1.

% comprimento(L,N) :- length(L,N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cmp_datas: Data1, Data2, R -> {V,F}
%
% O predicado cmp_datas compara duas datas e produz como resultado:
%   <  se a primeira data for anterior à segunda;
%   =  se as datas foram iguais;
%   >  se a primeira data for posterior à segunda.
%
% Nota: cada data é dada pelo predicado data: D,M,A -> {V,F}

cmp_datas(data(_, _, A1), data(_, _, A2), R) :-
	A1 \= A2, compare(R, A1, A2).
cmp_datas(data(_, M1, A), data(_, M2, A), R) :-
	M1 \= M2, compare(R, M1, M2).
cmp_datas(data(D1, M, A), data(D2, M, A), R) :-
	compare(R, D1, D2).

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

construir(S1, S2) :-
	retract(tmp(X)), !, construir(S1, [X|S2]).

construir(S,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento

evolucao(Termo) :-
	solucoes(Inv, +Termo::Inv, LInv),
	assert(Termo),
	testa(LInv).

evolucao(Termo) :- retract(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a involucao do conhecimento

involucao(Termo) :-
	Termo,
	solucoes(Inv, -Termo::Inv, LInv),
	assert(temp(Termo)),
	retract(Termo),
	testa(LInv),
	retract(temp(Termo)).

involucao(Termo) :-
	temp(X),
	retract(temp(X)),
	assert(X), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que testa uma lista de invariantes

testa([]).
testa([I|T]) :- I, testa(T).
