:- consult('BaseConhecimento.pl').


%------------------Invariantes:----------------
%----------------------------------------------

:- op( 900,xfy,'::' ).


%-------------Invariante Estafeta--------------
%----------------------------------------------

%Não pode permitir inserção do conhecimento repetido
+estafeta(Est,EF,EP,P) :: (solucoes((Est,EF,EP,P),estafeta(Est,EF,EP,P),Sol),length(Sol,Tam), Tam == 1).

%Não pode existir mais do que um estafeta com o mesmo numero
+estafeta(Est,_,_,_) :: (solucoes(Est, estafeta(Est,_,_,_), Sol), length(Sol,Tam), Tam == 1).



%-------------Invariante Encomenda-------------
%----------------------------------------------
%Não pode permitir inserção de conhecimento repetido
+encomenda(Enc,C,Peso,Vol,TMax,Freguesia,Data) :: (solucoes((Enc,C,Peso,Vol,TMax,Freguesia,Data),encomenda(Enc,C,Peso,Vol,TMax,Freguesia,Data), S), length(S, L), L == 1).

%Não pode existir mais do que uma encomenda com um dado nome
+encomenda(Enc,_,_,_,_,_,_) :: (solucoes(Enc, encomenda(Enc,_,_,_,_,_,_), S), length(S, L), L == 1).

%Freguesia deve existir no sistema
+encomenda(_,_,_,_,_,Freguesia,_) :: (solucoes(Freguesia, estimativa(Freguesia,_), S), length(S, L), L == 1).

%Data indicada deve ser valida
+encomenda(Enc,_,_,_,_,_,Data) :: (solucoes(Enc, (encomenda(Enc,_,_,_,_,_,Data), validaData(Data)), S), length(S, L), L == 1).



%---------Invariante EntregaIntermedia---------
%----------------------------------------------
/*
evolucao(entregaIntermedia(e00001,bicicleta,13)). -> False
evolucao(entregaIntermedia(e00009,mota,15)). -> True
evolucao(entregaIntermedia(e00020,mota,12)). -> False
evolucao(entregaIntermedia(e00010,trotinete,12)). -> False
*/

%Não pode permitir inserção de conhecimento repetido
+entregaIntermedia(Enc,MT,Preco) :: (solucoes((Enc,MT,Preco),entregaIntermedia(Enc,MT,Preco), S), length(S, L), L == 1).

%Não pode existir nenhuma entrega para a mesma encomenda
+entregaIntermedia(Enc,_,_) :: (solucoes(Enc, entregaIntermedia(Enc,_,_), S), length(S, L), L == 1).

%Deve existir um encomenda como o mesmo código
+entregaIntermedia(Enc,_,_) :: (solucoes(Enc, encomenda(Enc,_,_,_,_,_,_), S), length(S, L), L == 1).

%transporte deve existir 
+entregaIntermedia(_,MT,_) :: (solucoes(MT, meioTransporte(MT,_,_,_), S), length(S, L), L == 1).




%--------------Invariante Entrega--------------
%----------------------------------------------
/*Exemplos:
evolucao(entrega(e00001,1.5,3,data(1,5,2021))). -> False
evolucao(entrega(e00020,1.5,3,data(1,5,2021))). -> False
evolucao(entrega(e00007,1.5,3,data(1,5,2021))). -> True
evolucao(entrega(e00007,12,20,data(1,5,2021))). -> False
evolucao(entrega(e00007,12,5,data(1,5,2021))). -> True
evolucao(entrega(e00007,12,4,data(40,5,2021))). -> False
*/

%Não pode permitir inserção de conhecimento repetido
+entrega(Enc,TNec,Class,Data) :: (solucoes((Enc,TNec,Class,Data),entrega(Enc,TNec,Class,Data), S), length(S, L), L == 1).

%Não pode existir nenhuma entrega para a mesma encomenda
+entrega(Enc,_,_,_) :: (solucoes(Enc, entrega(Enc,_,_,_), S), length(S, L), L == 1).

%O numero de entrega deve existir como encomenda
+entrega(Enc,_,_,_) :: (solucoes(Enc, encomenda(Enc,_,_,_,_,_,_), S), length(S, L), L == 1).

%O numero de entrega deve existir como entregaIntermedia
+entrega(Enc,_,_,_) :: (solucoes(Enc, entregaIntermedia(Enc,_,_), S), length(S, L), L == 1).

%Classificacao deve ser entre 1 e 5
+entrega(Enc,_,Class,_) :: (solucoes(Enc,(entrega(Enc,_,Class,_) , validaClassificacao(Class)), S), length(S, L), L == 1).

%Data indicada deve ser valida:
+entrega(Enc,_,_,Data) :: (solucoes(Enc, (entrega(Enc,_,_,Data), validaData(Data)), S), length(S, L), L == 1).




%--------------Funções Auxiliares--------------
%----------------------------------------------

evolucao( Termo ) :- findall(Invariante, +Termo::Invariante,Lista), insercao(Termo),teste(Lista),!.

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.

teste([]).
teste([H|T]) :- H, teste(T).

solucoes(X,Y,Z) :- findall(X,Y,Z).

validaData(data(D,M,_)) :- M==2, D>=1, D=<28.
validaData(data(D,M,_)) :- D>=1, D=<31, (M==1; M==3; M==5; M==7; M==8; M==10; M==12).
validaData(data(D,M,_)) :- D>=1, D=<30, (M==4; M==6; M==9; M==11).

validaClassificacao(Class) :- Class >= 0 , Class =< 5.