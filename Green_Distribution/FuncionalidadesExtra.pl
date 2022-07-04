:- consult('BaseConhecimento.pl').
:- consult('Invariantes.pl').
:- consult('Fase2.pl').


%------------Funcionalidades Extra:------------
%----------------------------------------------


%----------------------------------------------
%Predicado que permite adicionar um novo estafeta à base de conhecimento:
adicionaEstafeta(NumeroEstafeta) :- evolucao(estafeta(NumeroEstafeta,[],[],1)).


%----------------------------------------------
%Predicado que permite adicionar uma nova encomenda à base de conhecimento:
adicionaEncomenda(NumeroEncomenda,NumeroCliente,Peso,Vol,TMax,Freg,Data) :-
		evolucao(encomenda(NumeroEncomenda,NumeroCliente,Peso,Vol,TMax,Freg,Data)).


%----------------------------------------------
%Predicado que permite começar a entrega de uma encomenda, ou seja, 
%obter um circuito, um meio transporte e atribui essa entrega a um estafeta
comecaEntrega(Encomenda,Estafeta,Percurso,Dist,MT,Tempo,Preco) :-
		gerarPercursoAEstrela(Encomenda,Percurso,Dist,MT,Tempo),

		%Adiciona a entregaIntermedia associada a esta Encomenda:				
		detPreco(Encomenda,MT,Preco),
		evolucao(entregaIntermedia(Encomenda,MT,Preco)),

		%Determina estafeta e atribui-lhe a entrega gerada:
		detEstafeta(Estafeta),
		atualizaEstafeta(Estafeta,Encomenda),

		%Atualiza os percurso da base de conhecimento:
		obterPeso(Encomenda,Peso),obterVolume(Encomenda,Volume),
		atualizaPercurso(percurso(Percurso,Volume,Peso)).


%Extensão do predicado detPreco: Encomenda, MeioTransporte, Preço -> {True,False}
detPreco(Encomenda,Mt,Sol) :- 
		obterPeso(Encomenda, Peso), 
		taxaPeso(Peso,TaxaPeso),
        obterTempoMaximo(Encomenda, Tempo), 
        taxaPrazoTempoMaximo(Tempo, TaxaPrazo),
        obterDistancia(Encomenda, Distancia), 
        taxaDistancia(Distancia,Mt,TaxaDist),
        Sol is TaxaPeso + TaxaPrazo + TaxaDist.

taxaPeso(Peso,5) :- Peso < 5, !.
taxaPeso(Peso,10) :- Peso < 20, !.
taxaPeso(Peso,15) :- Peso < 50, !.
taxaPeso(Peso,20) :- Peso < 100, !.
taxaPeso(_,30) :- !.

taxaPrazoTempoMaximo(Tempo, 10) :-	Tempo < 12, !.
taxaPrazoTempoMaximo(Tempo, 7) :-	Tempo < 24, !.
taxaPrazoTempoMaximo(Tempo, 3) :-	Tempo < 48, !.
taxaPrazoTempoMaximo(_, 0) :- !.

taxaDistancia(Dist,bicicleta,Sol) :- Sol is 0.5*Dist, !.
taxaDistancia(Dist,mota,Sol) :- Sol is 0.3*Dist, !.
taxaDistancia(Dist,carro,Sol) :- Sol is 0.4*Dist, !.


%Determina o estafeta que fará uma entrega
detEstafeta(Sol) :- detEstafetaLivres(Temp), detEstafetaPrioritario(Temp,Sol).

detEstafetaLivres(Sol) :- findall(X,(obterEncomendasPendentes(X,L),length(L,Tam),Tam =< 0),Sol).

detEstafetaPrioritario([H|T], Sol) :-  obterPrioridade(H,Prior),detEstafetaPrioritarioAux(T,Prior,H,Sol).

detEstafetaPrioritarioAux([],_,Estafeta,Estafeta).
detEstafetaPrioritarioAux([H|T],Prioridade,Estafeta,Sol):- obterPrioridade(H,VAux),VAux>=Prioridade,detEstafetaPrioritarioAux(T,Prioridade,Estafeta,Sol).
detEstafetaPrioritarioAux([H|T],Prioridade,_,Sol):- obterPrioridade(H,VAux),VAux<Prioridade,detEstafetaPrioritarioAux(T,VAux,H,Sol).


atualizaEstafeta(Estafeta, Encomenda) :- 
		obterEntregasEntregues(Estafeta,Entregues),
		obterEncomendasPendentes(Estafeta,Pendentes),
		obterPrioridade(Estafeta,Prioridade),

		%Faz retract do detEstafeta
		retract(estafeta(Estafeta,Entregues,Pendentes,Prioridade)),

		%Adiciona o estafeta com a nova entrega: 
		adicionaElementoLista(Pendentes,Encomenda,NovoPendentes),
		evolucao(estafeta(Estafeta, Entregues,NovoPendentes,Prioridade)).

adicionaElementoLista([],Encomenda,[Encomenda]).
adicionaElementoLista([H|T], Encomenda, Sol) :-	adicionaElementoLista(T,Encomenda,Resultado), append([H],Resultado,Sol). 


%----------------------------------------------
concluiEntrega(Encomenda,TempoUsado,Classificacao,Data) :-
		%Descobre quem é o estafeta que está a entregar:
		quemEstaEntregar(Encomenda,Lista), length(Lista,Tam), Tam == 1, 
		obtemEstafeta(Lista,Estafeta),
		write(Estafeta),

		atualizarListasEstafeta(Estafeta,Encomenda,TempoUsado),

		evolucao(entrega(Encomenda,TempoUsado,Classificacao,Data)).


quemEstaEntregar(Encomenda,Sol):-
		findall(Est,(estafeta(Est,_,Pendentes,_), member(Encomenda,Pendentes)),Sol).


obtemEstafeta([H|_],H).


atualizarListasEstafeta(Estafeta, Encomenda,TempoUsado) :-
		obterEncomendasPendentes(Estafeta,Pendentes),
		obterEntregasEntregues(Estafeta,Entregues),
		obterPrioridade(Estafeta,Prioridade),
		retract(estafeta(Estafeta,Entregues,Pendentes,Prioridade)),

		removeEncomendaPendentes(Pendentes,Encomenda,NovaListaP),
		adicionaEncomendaEntregues(Entregues,Encomenda,NovaListaE),

		obterTempoMaximo(Encomenda,TempoMaximo),
		(TempoUsado > TempoMaximo -> NovaPrioridade is Prioridade + 1; NovaPrioridade is Prioridade),


		evolucao(estafeta(Estafeta,NovaListaE,NovaListaP,NovaPrioridade)).



removeEncomendaPendentes([H|T],H,T).
removeEncomendaPendentes([H|T],X,[H|Sol]):- H \= X, removeEncomendaPendentes(T,X,Sol).  

adicionaEncomendaEntregues([],X,[X]).
adicionaEncomendaEntregues([H|T],X,[H|Sol]):- adicionaEncomendaEntregues(T,X,Sol).