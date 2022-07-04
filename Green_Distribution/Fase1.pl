:- consult('BaseConhecimento.pl').


%---------------Green Distribution---------------

%----------Funções de Consulta de Dados-----------
%-------------------------------------------------

%MEIOTRANSPORTE
obterVertenteEcologica(Id,X) :- meioTransporte(Id,_,_,X).
obterPesoMaximo(Id,X) :- meioTransporte(Id,X,_,_).
obterVelocidaMedia(Id,X) :- meioTransporte(Id,_,X,_).

%ESTAFETA
obterEntregasEntregues(Est,L) :- estafeta(Est,L,_,_).
obterEncomendasPendentes(Est,L) :- estafeta(Est,_,L,_).
obterPrioridade(Est,P) :- estafeta(Est,_,_,P).

%ENCOMENDA
obterCliente(E,C):- encomenda(E,C,_,_,_,_,_).
obterPeso(E,P):- encomenda(E,_,P,_,_,_,_).
obterVolume(E,V):- encomenda(E,_,_,V,_,_,_).
obterTempoMaximo(E,T):- encomenda(E,_,_,_,T,_,_).
obterDistancia(E,D):- encomenda(E,_,_,_,_,F,_), estimativa(F,D). 
obterFreguesia(Id,Freguesia):- encomenda(Id,_,_,_,_,Freguesia,_).

%ENTREGAINTERMEDIO:
obterMeioTransporte(E,X) :- entregaIntermedia(E,X,_).
obterPreco(E,Preco) :- entregaIntermedia(E,_,Preco). 

%ENTREGA:
obterClassificacao(E,X):-entrega(E,_,X,_).
obterEntregaData(E,Data):- entrega(E,_,_,Data).



%---------------Funcionalidades----------------
%----------------------------------------------

obterTodosEstafetas(Sol):-findall(X,estafeta(X,_,_,_),Sol). 
%obterTodasEntregasEntregues(Sol):-findall(X,(estafeta(_,L,_,_),member(X,L)),Sol).
%obterTodasEncomendasEntregues(Sol) :- findall(X,(estafeta(_,L,_,_),member(D,L),obterNumeroEncomenda(D,X)),Sol).


%-----Ponto1: Determinar o estafeta que utilizou mais vezes um meio de transporte mais ecologico
obterMaisEcologico(Sol/Uso) :- findall(X,estafeta(X,_,_,_),[H|T]), obterQuantasUsaBicicleta(H,Custo), obterMaisBicicleta(T,H,Custo,Sol/Uso),Uso > 0.
obterMaisEcologico(Sol/Uso) :- findall(X,estafeta(X,_,_,_),[H|T]), obterQuantasUsaMota(H,Custo), obterMaisMota(T,H,Custo,Sol/Uso),Uso > 0.
obterMaisEcologico(Sol/Uso) :- findall(X,estafeta(X,_,_,_),[H|T]), obterQuantasUsaCarro(H,Custo), obterMaisCarro(T,H,Custo,Sol/Uso).

obterMaisBicicleta([],Estafeta,Usos, Estafeta/Usos).
obterMaisBicicleta([H|T],Estafeta,Usos, Sol) :- obterQuantasUsaBicicleta(H,Quanto), Usos >= Quanto, obterMaisBicicleta(T,Estafeta,Usos, Sol).
obterMaisBicicleta([H|T],_,Usos, Sol) :- obterQuantasUsaBicicleta(H,Quanto), Usos < Quanto, obterMaisBicicleta(T,H,Quanto,Sol).

obterMaisMota([],Estafeta,Usos, Estafeta/Usos).
obterMaisMota([H|T],Estafeta,Usos, Sol) :- obterQuantasUsaMota(H,Quanto), Usos >= Quanto, obterMaisMota(T,Estafeta,Usos, Sol).
obterMaisMota([H|T],_,Usos, Sol) :- obterQuantasUsaMota(H,Quanto), Usos < Quanto, obterMaisMota(T,H,Quanto,Sol).

obterMaisCarro([],Estafeta,Usos, Estafeta/Usos).
obterMaisCarro([H|T],Estafeta,Usos, Sol) :- obterQuantasUsaCarro(H,Quanto), Usos >= Quanto, obterMaisCarro(T,Estafeta,Usos, Sol).
obterMaisCarro([H|T],_,Usos, Sol) :- obterQuantasUsaCarro(H,Quanto), Usos < Quanto, obterMaisCarro(T,H,Quanto,Sol).

obterQuantasUsaBicicleta(Estafeta, Usa) :- findall(X, (obterEntregasEntregues(Estafeta,L),member(X,L),obterMeioTransporte(X,bicicleta)),Lista), length(Lista,Usa).
obterQuantasUsaMota(Estafeta, Usa) :- findall(X, (obterEntregasEntregues(Estafeta,L),member(X,L),obterMeioTransporte(X,mota)),Lista), length(Lista,Usa).
obterQuantasUsaCarro(Estafeta, Usa) :- findall(X, (obterEntregasEntregues(Estafeta,L),member(X,L),obterMeioTransporte(X,carro)),Lista), length(Lista,Usa).


%-----Ponto2: Identificar que estafetas entregaram determinadas encomendas
%NumeroEncomenda deve ser uma lista
quemEntregou(NumeroEncomenda,NumeroEstafeta) :- findall(X,(obterEntregasEntregues(X,L),member(D,L),member(D,NumeroEncomenda)),NumeroEstafeta).


%-----Ponto3: Identificar os clientes servidos por um determinado estafeta;
aQuemEntregou(NumeroEstafeta,ListaClientes):- findall(X,(obterEntregasEntregues(NumeroEstafeta,L),member(D,L),obterCliente(D,X)),ListaClientes).


%----Ponto4: Calcular o valor faturado pela Green Distribution num determinado dia;
totalFaturado(D,M,A,Total) :- 
	findall(P,(obterEntregaData(Enc,data(D,M,A)), obterPreco(Enc,P)),Precos),
	sumlist(Precos,Total).


%-----Ponto5: Identificar qual a freguesia com maior volume de entregas por parte da Green Distribution; 

obterFreguesiaComMaiorVolume(Freguesia) :- obterTodasFreguesias(Lista), ultimoElemento(Lista,Freguesia).
       
obterTodasFreguesias(Freguesias) :- obterTodosEstafetas(ListaEstafetas),
                                 findall(X,(member(Y,ListaEstafetas),obterFreguesiasEstafeta(Y,X)),Freguesia), flatten(Freguesia, FreguesiaM),
                                 findall(X-Y,(member(Y,FreguesiaM),occurrences_of(FreguesiaM,Y,X)),FreguesiaF), keysort(FreguesiaF,Freguesias).
            
obterFreguesiasEstafeta(Id,Sol) :- obterEntregasEntregues(Id,ListaEntregas), 
                                   findall(X,(member(Y,ListaEntregas),obterFreguesia(Y,X)),Sol).

occurrences_of(List, X, Count) :- aggregate_all(count, member(X, List), Count).

ultimoElemento([_-X],X).
ultimoElemento([_|T],Y):- ultimoElemento(T,Y). 


%-----Ponto6: Calcula a classificação media de satisfação de cliente para um determinado estafeta; 
classificacaoMedia(Nestafeta, Ncliente, Sol):-
    obterEntregasEntregues(Nestafeta,ListaEntregas),
    listaEntregasACliente(ListaEntregas, Ncliente,ListaEntregasAoCliente), 
    calculaMediaAvaliacao(ListaEntregasAoCliente,Sol).

%filtra a lista de encomendas e retorna apenas aquelas que foram entregues a um certo cliente
listaEntregasACliente(ListaEntregas, Ncliente, Listafinal):-
    findall(X,(member(X,ListaEntregas),obterCliente(X,Ncliente)),Listafinal).
 
calculaMediaAvaliacao([],0).
calculaMediaAvaliacao(ListaEntregas,Media):-
    calculaMediaAux(ListaEntregas,Soma,Elementos),Media is Soma/Elementos.

calculaMediaAux([],0,0).
calculaMediaAux([H|T],Soma,Elementos):-
    obterClassificacao(H,Class),calculaMediaAux(T,Soma1,Elementos1),Soma is Soma1+Class, Elementos is Elementos1 + 1.


%-----Ponto7: Identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo;
entregasPorTransporte(data(D1,M1,A1),data(D2,M2,A2),B,M,C) :- findall(X, (obterMeioTransporte(X,bicicleta), obterEntregaData(X,data(D,M,A)), comparaDatas(data(D,M,A),data(D1,M1,A1),data(D2,M2,A2))), Bs), length(Bs,B),
                                                              findall(X, (obterMeioTransporte(X,mota), obterEntregaData(X,data(D,M,A)), comparaDatas(data(D,M,A),data(D1,M1,A1),data(D2,M2,A2))), Ms), length(Ms,M),
                                                              findall(X, (obterMeioTransporte(X,carro), obterEntregaData(X,data(D,M,A)), comparaDatas(data(D,M,A),data(D1,M1,A1),data(D2,M2,A2))), Cs), length(Cs,C).

comparaDatas(data(D,M,A),data(D1,M,A),data(D2,M,A)):- D1 =< D , D =< D2, !.
comparaDatas(data(_,M,A),data(_,M1,A),data(_,M2,A)):- M1 < M, M < M2, !.
comparaDatas(data(D,M,A),data(D1,M1,A),data(_,M2,A)):- M1 =< M, M < M2, D1 =< D, !.
comparaDatas(data(D,M,A),data(_,M1,A),data(D2,M2,A)):- M1 < M, M =< M2, D =< D2, !.
comparaDatas(data(_,_,A),data(_,_,A1),data(_,_,A2)):- A1 < A, A < A2, !.
comparaDatas(data(D,M,A),data(D1,M1,A1),data(_,_,A2)):- A1 =< A, A < A2, M1 =< M, D1 =< D, !.
comparaDatas(data(D,M,A),data(_,_,A1),data(D2,M2,A2)):- A1 < A, A =< A2, M =< M2, D =< D2, !.


%-----Ponto8: Identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo; 
numEncomendasEntreguesTempo(data(D1,M1,A1), data(D2,M2,A2), Sol) :- findall(X,(entrega(X,_,_,data(D,M,A)),comparaDatas(data(D,M,A),data(D1, M1, A1),data(D2, M2, A2))),L), length(L,Sol).


%-----Ponto9: Calcula o numero de encomendas que não foram entregues num periodo de tempo ou que ainda não foram entregues
numEncomendasNaoEntreguesTempo(data(D1, M1, A1), data(D2, M2, A2), Sol) :- findall(X,(entrega(X,_,_,data(D,M,A)),comparaDatas(data(D,M,A),data(D1, M1, A1),data(D2, M2, A2))),L), 
                                                                            findall(Y,(encomenda(Y,_,_,_,_,_,_), not(member(Y,L))), L1), length(L1,Sol).


%-----Ponto10: Calcula o peso total transportado por estafeta num determinado dia

pesoTransportadoEstafetaDia(E, data(Dia, Mes, Ano), Sol) :- obterEntregasEntregues(E, Del), obterListaIdEncomendaPorDia(Del,data(Dia,Mes,Ano),NEncs), pesoTotal(NEncs,Sol).

obterListaIdEncomendaPorDia(Del, data(Dia,Mes,Ano),Sol) :- findall(X,(member(Y,Del), obterNumEncomendaPorDataEntrega(Y, data(Dia,Mes,Ano), X)), Sol).

pesoTotal([], 0).
pesoTotal([H|T], Sol) :- obterPeso(H,Peso), pesoTotal(T,Sol1), Sol is Sol1 + Peso. 

obterNumEncomendaPorDataEntrega(Del, data(Mes,Dia,Ano), X) :- encomenda(Del,_,_,_,_,_,data(Mes,Dia,Ano)), X = Del. 