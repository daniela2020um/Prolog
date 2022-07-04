:- consult('BaseConhecimento.pl').
:- consult('Fase1.pl').


%---------------Green Distribution---------------
%---------------------Fase 2---------------------
adjacente(Rua, ProxRua, C) :- 
	viagem(Rua, ProxRua, C).
adjacente(Rua, ProxRua, C) :- 
	viagem(ProxRua, Rua, C).

adjacente2([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
    adjacente(Nodo,ProxNodo,PassoCusto),
	\+member(ProxNodo, Caminho),
	NovoCusto is Custo + PassoCusto,
	estimativa(ProxNodo, Est).

expande(Caminho, ExpCaminhos) :-
	findall(NovoCaminho, adjacente2(Caminho,NovoCaminho), ExpCaminhos).	

%----------------------DFS---------------------
%----------------------------------------------
resolve_DFS(Objetivo,Rua,Caminho1,Custo) :-
    dfs(Objetivo,Rua,[Rua],Caminho,Custo),
    reverse([Rua|Caminho],Caminho1).

dfs(Objetivo,Objetivo,_, [], 0).
dfs(Objetivo,Rua, Historico, [ProxRua|Caminho], C) :-
    adjacente(Rua, ProxRua, C1),
    \+(member(ProxRua, Historico)),
    dfs(Objetivo,ProxRua, [ProxRua|Historico], Caminho, C2), C is C1 + C2.  

tempoDFS(RunTime,Inicio,Fim,Percurso,Distancia) :-
        statistics(runtime,[Start|_]),
        resolve_DFS(Inicio,Fim,Percurso,Distancia),
        statistics(runtime,[Stop|_]),
        RunTime is Stop - Start.


%----------------------BFS---------------------
%----------------------------------------------
resolve_BFS(Objetivo,Rua,Caminho,C) :- bfs(Objetivo,[[Rua/0]],Caminho,C).

bfs(Objetivo,[[Objetivo/C|T]|_],Lista,C) :- removeTermo([Objetivo/C|T], Lista).
bfs(Objetivo,[LA|Outros],Caminho,C) :- 
                LA = [Act/CustoUltimo|_], 
                (Act \= Objetivo),
                findall([RuaX/Custo|LA], (adjacente(Act,RuaX,CustoNovo),\+(member(RuaX,LA)),Custo is CustoUltimo + CustoNovo),Novos),
                append(Outros,Novos,Todos),
                bfs(Objetivo,Todos,Caminho,C).

removeTermo([],[]).
removeTermo([L/_|T],[L|T2]) :- removeTermo(T,T2).

tempoBFS(RunTime,Inicio,Fim,Percurso,Distancia) :-
    statistics(runtime,[Start|_]),
    resolve_BFS(Inicio,Fim,Percurso,Distancia),
    statistics(runtime,[Stop|_]),
    RunTime is Stop - Start.


%---------Busca iterativa limitada em profundidade-----------
%------------------------------------------------------------
resolve_BILP(Objetivo,Rua,Caminho1,Custo,Limite) :-
    resolve_bilp2(Objetivo,Rua,Caminho,Custo,0,Limite),
    reverse([Rua|Caminho],Caminho1).

resolve_bilp2(Objetivo,Rua,Caminho,Custo,Profundidade,_):- dfs2(Objetivo,Rua,[Rua],Caminho,Custo,0,Profundidade).
resolve_bilp2(Objetivo,Rua,Caminho,Custo,Profundidade,Limite) :-
    Profundidade1 is Profundidade +1,
    Profundidade1 =< Limite,
    resolve_bilp2(Objetivo,Rua,Caminho,Custo,Profundidade1,Limite).

dfs2(Objetivo,Objetivo,_,[],0,_,_).
dfs2(Objetivo,Rua,Historico,[ProxRua|Caminho],C,Iteracao,Limite) :-
    Iteracao =< Limite,
    Iteracao1 is Iteracao + 1,
    adjacente(Rua, ProxRua, C1),
    \+(member(ProxRua, Historico)),
    dfs2(Objetivo,ProxRua,[ProxRua|Historico],Caminho,C2,Iteracao1,Limite), C is C1 + C2.

tempoBILP(RunTime,Inicio,Fim,Percurso,Distancia,Limite) :-
    statistics(runtime,[Start|_]),
    resolve_BILP(Inicio,Fim,Percurso,Distancia,Limite),
    statistics(runtime,[Stop|_]),
    RunTime is Stop - Start.

%---------------------GULOSA-------------------
%----------------------------------------------
resolve_gulosa(Objetivo,Rua, Caminho/Custo) :-
	estimativa(Rua, Estima),
	agulosa(Objetivo, [[Rua]/0/Estima], Caminho/Custo/_).

agulosa(Objetivo, Caminhos, Caminho) :-
	obtem_melhor_g(Caminhos, Caminho),
	Caminho = [Objetivo|_]/_/_.

agulosa(Objetivo, Caminhos, SolucaoCaminho) :-
	obtem_melhor_g(Caminhos, MelhorCaminho),
	select(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande(MelhorCaminho, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    agulosa(Objetivo, NovoCaminhos, SolucaoCaminho).		

obtem_melhor_g([Caminho], Caminho) :- !.

obtem_melhor_g([Caminho1/Custo1/Est1,_/_/Est2|Caminhos], MelhorCaminho) :-
	Est1 =< Est2, !,
	obtem_melhor_g([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
	
obtem_melhor_g([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_g(Caminhos, MelhorCaminho).

tempoGulosa(RunTime,Inicio,Fim,Percurso,Distancia) :-
    statistics(runtime,[Start|_]),
    resolve_gulosa(Inicio,Fim,Percurso/Distancia),
    statistics(runtime,[Stop|_]),
    RunTime is Stop - Start.

%----------------------A*----------------------
%----------------------------------------------
resolve_aestrela(Objetivo,Nodo, Caminho/Custo) :-
	estimativa(Nodo, Estima),
	aestrela(Objetivo, [[Nodo]/0/Estima], Caminho/Custo/_).

aestrela(Objetivo, Caminhos, Caminho) :-
	obtem_melhor(Caminhos, Caminho),
	Caminho = [Objetivo|_]/_/_.

aestrela(Objetivo, Caminhos, SolucaoCaminho) :-
	obtem_melhor(Caminhos, MelhorCaminho),
	select(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande(MelhorCaminho, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrela(Objetivo, NovoCaminhos, SolucaoCaminho).	

obtem_melhor([Caminho], Caminho) :- !.
obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Custo1 + Est1 =< Custo2 + Est2, !,
	obtem_melhor([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho). 
obtem_melhor([_|Caminhos], MelhorCaminho) :- 
	           obtem_melhor(Caminhos, MelhorCaminho).

tempoAEstrela(RunTime,Inicio,Fim,Percurso,Distancia) :-
    statistics(runtime,[Start|_]),
    resolve_aestrela(Inicio,Fim,Percurso/Distancia),
    statistics(runtime,[Stop|_]),
    RunTime is Stop - Start.


%---------------Função Complementar------------
%----------------------------------------------
%Determinar o meio de transporte que será utilizado e o tempo que será necessário para executar um determinado circuito
detMeioTransporte(Encomenda,Distancia,TempoAnterior,MeioTransporte,TempoNecessario) :-
            obterTempoMaximo(Encomenda,TempoMaximoPreDefinido),
            TempoMaximo is TempoMaximoPreDefinido - TempoAnterior,
            obterPeso(Encomenda,Peso),
            detMeioTransporteAux(Peso,Distancia,TempoMaximo,MeioTransporte,TempoNecessario).


detMeioTransporteAux(Peso,Distancia,TempoMaximo,bicicleta,Tempo) :- 
            Peso =< 5, 
            calculaTempoBicicleta(Peso,Distancia,Tempo),
            Tempo < TempoMaximo, !.
detMeioTransporteAux(Peso,Distancia,TempoMaximo,mota,Tempo) :- 
            Peso =< 20, 
            calculaTempoMota(Peso,Distancia,Tempo), 
            Tempo < TempoMaximo, !.
detMeioTransporteAux(Peso,Distancia,TempoMaximo,carro,Tempo) :- 
            Peso =< 100, 
            calculaTempoCarro(Peso,Distancia,Tempo), 
            Tempo < TempoMaximo, !.


calculaTempoBicicleta(Peso,Distancia,Sol) :- 
                    obterVelocidaMedia(bicicleta,Velocidade),
                    VelocidadeFinal is Velocidade - 0.7*Peso,
                    Sol is Distancia/VelocidadeFinal.

calculaTempoMota(Peso,Distancia,Sol) :- 
                    obterVelocidaMedia(mota,Velocidade),
                    VelocidadeFinal is Velocidade - 0.5*Peso,
                    Sol is Distancia/VelocidadeFinal.


calculaTempoCarro(Peso,Distancia,Sol) :- 
                    obterVelocidaMedia(carro,Velocidade),
                    VelocidadeFinal is Velocidade - 0.1*Peso,
                    Sol is Distancia/VelocidadeFinal.

calculaTempoUsado(Distancia, MeioTransporte, Tempo) :-
            obterVelocidaMedia(MeioTransporte, Velocidade),
            Tempo is Distancia/Velocidade.



%----------------------------TESTES--------------------------
%------------------------------------------------------------
% 1) Predicado que permite gerar um circuito para uma encomenda usando o DFS
gerarPercursoDFS(Encomenda,Percurso,Distancia,MT,Tempo) :-
    %IDA:
        obterFreguesia(Encomenda, Rua),
        resolve_DFS(sede, Rua, Percurso1,Distancia1),
        detMeioTransporte(Encomenda,Distancia1,0,MT,TempoIda),
    %VOLTA:
        resolve_DFS(Rua,sede,[_|Percurso2],Distancia2),
        calculaTempoUsado(Distancia2,MT,TempoVolta),
        append(Percurso1,Percurso2,Percurso),
        Distancia is Distancia1 + Distancia2,
        Tempo is TempoIda+TempoVolta.

% 2) Predicado que permite gerar um circuito para várias encomenda usando o DFS
gerarPercursoDFSVariasEncomendas(Encomenda,PercursoFinal,DistanciaFinal,MTFinal,TempoFinal):-
        obterValoresDFS(sede,Encomenda,PercursoFinal,DistanciaFinal,0,TempoFinal,bicicleta,MTFinal),  %Começa com bicicleta porque é o "elemento neutro"
        write(PercursoFinal).


obterValoresDFS(Origem,[],PercursoObtido,DistanciaObtida,TIEtapa,TFEtapa,MTAnterior,MTAnterior) :-
        resolve_DFS(Origem,sede,PercursoObtido,DistanciaObtida),
        calculaTempoUsado(DistanciaObtida,MTAnterior,TempoVolta),
        TFEtapa is TIEtapa + TempoVolta.
        
obterValoresDFS(Origem,[Encomenda|Restantes],PercursoTotal,DistTotal,TIEtapa,TFinal,MTAnterior,MTFinal) :-
        obterFreguesia(Encomenda,Paragem), %Descobre qual a freguesia onde se deve entregar a encomenda  
        resolve_DFS(Origem,Paragem,PercursoAtual,DistanciaAtual), %Obtem o percurso que será feito nesta etapa

        detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTNecessario,DuracaoEtapaPrevista), %Obtem o meio de transporte recomendado, e o tempo necessário para executar esta etapa
        escolheMeioTransporteNecessario(MTAnterior,MTNecessario,MTProximo),

        (MTNecessario \= MTProximo -> calculaTempoUsado(DistanciaAtual,MTProximo,DuracaoEtapa), !; DuracaoEtapa is DuracaoEtapaPrevista), %Se não se usar o Meio de transporte obtido é porque esta etapa será feita em menos tempo
        
        TempoAtual is TIEtapa + DuracaoEtapa,   
        obterValoresDFS(Paragem,Restantes,[_|PercursoResultante],DistResultante,TempoAtual,TFEtapa,MTProximo,MTFinal),

        (MTFinal \= MTProximo -> 
            detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTFinal,TempoReal), TFinal is TFEtapa - DuracaoEtapa + TempoReal,!;
            TFinal is TFEtapa, !),

        append(PercursoAtual,PercursoResultante,PercursoTotal),
        DistTotal is DistanciaAtual + DistResultante.



%------------------------------------------------------------
% 1) Predicado que permite gerar um percurso para uma encomenda usando o BFS
gerarPercursoBFS(Encomenda,Percurso,Distancia,MT,Tempo) :-
    %IDA:
        obterFreguesia(Encomenda, Rua), 
        resolve_BFS(sede, Rua, Percurso1,Distancia1),
        detMeioTransporte(Encomenda,Distancia1,0,MT,TempoIda),
    %VOLTA:
        resolve_BFS(Rua,sede,[_|Percurso2],Distancia2),
        calculaTempoUsado(Distancia2,MT,TempoVolta),
        append(Percurso1,Percurso2,Percurso),
        Distancia is Distancia1 + Distancia2,
        Tempo is TempoIda+TempoVolta.


% 2) Predicado que permite gerar um circuito para várias encomenda usando o BFS
gerarPercursoBFSVariasEncomendas(Encomenda,PercursoFinal,DistanciaFinal,MTFinal,TempoFinal):-
        obterValoresBFS(sede,Encomenda,PercursoFinal,DistanciaFinal,0,TempoFinal,bicicleta,MTFinal),  %Começa com bicicleta porque é o "elemento neutro"
        write(PercursoFinal).


obterValoresBFS(Origem,[],PercursoObtido,DistanciaObtida,TIEtapa,TFEtapa,MTAnterior,MTAnterior) :-
        resolve_BFS(Origem,sede,PercursoObtido,DistanciaObtida),
        calculaTempoUsado(DistanciaObtida,MTAnterior,TempoVolta),
        TFEtapa is TIEtapa + TempoVolta.
        
obterValoresBFS(Origem,[Encomenda|Restantes],PercursoTotal,DistTotal,TIEtapa,TFinal,MTAnterior,MTFinal) :-
        obterFreguesia(Encomenda,Paragem), %Descobre qual a freguesia onde se deve entregar a encomenda  
        resolve_BFS(Origem,Paragem,PercursoAtual,DistanciaAtual), %Obtem o percurso que será feito nesta etapa

        detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTNecessario,DuracaoEtapaPrevista), %Obtem o meio de transporte recomendado, e o tempo necessário para executar esta etapa
        escolheMeioTransporteNecessario(MTAnterior,MTNecessario,MTProximo),

        (MTNecessario \= MTProximo -> calculaTempoUsado(DistanciaAtual,MTProximo,DuracaoEtapa), !; DuracaoEtapa is DuracaoEtapaPrevista), %Se não se usar o Meio de transporte obtido é porque esta etapa será feita em menos tempo
        
        TempoAtual is TIEtapa + DuracaoEtapa,   
        obterValoresBFS(Paragem,Restantes,[_|PercursoResultante],DistResultante,TempoAtual,TFEtapa,MTProximo,MTFinal),
        
        (MTFinal \= MTProximo -> 
            detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTFinal,TempoReal), TFinal is TFEtapa - DuracaoEtapa + TempoReal,!;
            TFinal is TFEtapa, !),

        append(PercursoAtual,PercursoResultante,PercursoTotal),
        DistTotal is DistanciaAtual + DistResultante.



%------------------------------------------------------------
% 1) Predicado que permite gerar um percurso para uma encomenda usando o BILP
gerarPercursoBILP(Encomenda,Percurso,Distancia,MT,Tempo,Limite) :-
    %IDA:
        obterFreguesia(Encomenda, Rua), 
        resolve_BILP(sede,Rua,PercursoIda,DistanciaIda,Limite), 
        detMeioTransporte(Encomenda,DistanciaIda,0,MT,TempoIda),
    %VOLTA:
        resolve_BILP(Rua,sede,[_|PercursoVolta],DistanciaVolta,Limite), 
        calculaTempoUsado(DistanciaVolta,MT,TempoVolta),
        append(PercursoIda,PercursoVolta,Percurso),
        Distancia is DistanciaIda + DistanciaVolta,
        Tempo is TempoIda+TempoVolta.


% 2) Predicado que permite gerar um circuito para várias encomenda usando o BILP
gerarPercursoBILPVariasEncomendas(Encomenda,PercursoFinal,DistanciaFinal,MTFinal,TempoFinal):-
        obterValoresBILP(sede,Encomenda,PercursoFinal,DistanciaFinal,0,TempoFinal,bicicleta,MTFinal),  %Começa com bicicleta porque é o "elemento neutro"
        write(PercursoFinal).


obterValoresBILP(Origem,[],PercursoObtido,DistanciaObtida,TIEtapa,TFEtapa,MTAnterior,MTAnterior) :-
        resolve_BILP(Origem,sede,PercursoObtido,DistanciaObtida,10),
        calculaTempoUsado(DistanciaObtida,MTAnterior,TempoVolta),
        TFEtapa is TIEtapa + TempoVolta.
        
obterValoresBILP(Origem,[Encomenda|Restantes],PercursoTotal,DistTotal,TIEtapa,TFinal,MTAnterior,MTFinal) :-
        obterFreguesia(Encomenda,Paragem), %Descobre qual a freguesia onde se deve entregar a encomenda  
        resolve_BILP(Origem,Paragem,PercursoAtual,DistanciaAtual,10), %Obtem o percurso que será feito nesta etapa

        detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTNecessario,DuracaoEtapaPrevista), %Obtem o meio de transporte recomendado, e o tempo necessário para executar esta etapa 
        escolheMeioTransporteNecessario(MTAnterior,MTNecessario,MTProximo),

        (MTNecessario \= MTProximo -> calculaTempoUsado(DistanciaAtual,MTProximo,DuracaoEtapa), !; DuracaoEtapa is DuracaoEtapaPrevista), %Se não se usar o Meio de transporte obtido é porque esta etapa será feita em menos tempo
        
        TempoAtual is TIEtapa + DuracaoEtapa,   
        obterValoresBILP(Paragem,Restantes,[_|PercursoResultante],DistResultante,TempoAtual,TFEtapa,MTProximo,MTFinal),

        (MTFinal \= MTProximo -> 
            detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTFinal,TempoReal), TFinal is TFEtapa - DuracaoEtapa + TempoReal,!;
            TFinal is TFEtapa, !),

        append(PercursoAtual,PercursoResultante,PercursoTotal),
        DistTotal is DistanciaAtual + DistResultante.



%------------------------------------------------------------
% 1) Predicado que permite gerar um percurso para uma encomenda usando a Gulosa
gerarPercursoGulosa(Encomenda,Percurso,Distancia,MT,Tempo) :-
    %IDA:
        obterFreguesia(Encomenda, Rua), 
        resolve_gulosa(sede, Rua, PercursoIda/DistanciaIda),
        detMeioTransporte(Encomenda,DistanciaIda,0,MT,TempoIda),
    %VOLTA:
        resolve_gulosa(Rua, sede, [_|PercursoVolta]/DistanciaVolta),
        calculaTempoUsado(DistanciaVolta,MT,TempoVolta),
        append(PercursoIda,PercursoVolta,Percurso),
        Distancia is DistanciaIda + DistanciaVolta,
        Tempo is TempoIda+TempoVolta.


% 2) Predicado que permite gerar um circuito para várias encomenda usando o Gulosa
gerarPercursoGulosaVariasEncomendas(Encomenda,PercursoFinal,DistanciaFinal,MTFinal,TempoFinal):-
        obterValoresGulosa(sede,Encomenda,PercursoFinal,DistanciaFinal,0,TempoFinal,bicicleta,MTFinal),  %Começa com bicicleta porque é o "elemento neutro"
        write(PercursoFinal).


obterValoresGulosa(Origem,[],PercursoObtido,DistanciaObtida,TIEtapa,TFEtapa,MTAnterior,MTAnterior) :-
        resolve_gulosa(Origem,sede,PercursoObtido/DistanciaObtida),
        calculaTempoUsado(DistanciaObtida,MTAnterior,TempoVolta),
        TFEtapa is TIEtapa + TempoVolta.
        
obterValoresGulosa(Origem,[Encomenda|Restantes],PercursoTotal,DistTotal,TIEtapa,TFinal,MTAnterior,MTFinal) :-
        obterFreguesia(Encomenda,Paragem), %Descobre qual a freguesia onde se deve entregar a encomenda  
        resolve_gulosa(Origem,Paragem,PercursoAtual/DistanciaAtual), %Obtem o percurso que será feito nesta etapa

        detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTNecessario,DuracaoEtapaPrevista), %Obtem o meio de transporte recomendado, e o tempo necessário para executar esta etapa
        escolheMeioTransporteNecessario(MTAnterior,MTNecessario,MTProximo),

        (MTNecessario \= MTProximo -> calculaTempoUsado(DistanciaAtual,MTProximo,DuracaoEtapa), !; DuracaoEtapa is DuracaoEtapaPrevista), %Se não se usar o Meio de transporte obtido é porque esta etapa será feita em menos tempo

        TempoAtual is TIEtapa + DuracaoEtapa,   
        obterValoresGulosa(Paragem,Restantes,[_|PercursoResultante],DistResultante,TempoAtual,TFEtapa,MTProximo,MTFinal),

        (MTFinal \= MTProximo -> 
            detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTFinal,TempoReal), TFinal is TFEtapa - DuracaoEtapa + TempoReal,!;
            TFinal is TFEtapa, !),

        append(PercursoAtual,PercursoResultante,PercursoTotal),
        DistTotal is DistanciaAtual + DistResultante.


%------------------------------------------------------------
% 1) Predicado que permite gerar um percurso para uma encomenda usando o A*
gerarPercursoAEstrela(Encomenda,Percurso,Distancia,MT,Tempo) :-
    %IDA:
        obterFreguesia(Encomenda, Rua), 
        resolve_aestrela(sede, Rua, PercursoIda/DistanciaIda),
        detMeioTransporte(Encomenda,DistanciaIda,0,MT,TempoIda),
    %VOLTA:
        resolve_aestrela(Rua, sede, [_|PercursoVolta]/DistanciaVolta),
        calculaTempoUsado(DistanciaVolta,MT,TempoVolta),
        append(PercursoIda,PercursoVolta,Percurso),
        Distancia is DistanciaIda + DistanciaVolta,
        Tempo is TempoIda+TempoVolta.


% 2) Predicado que permite gerar um circuito para várias encomenda usando o A*
gerarPercursoAEstrelaVariasEncomendas(Encomenda,PercursoFinal,DistanciaFinal,MTFinal,TempoFinal):-
        obterValoresAEstrela(sede,Encomenda,PercursoFinal,DistanciaFinal,0,TempoFinal,bicicleta,MTFinal),  %Começa com bicicleta porque é o "elemento neutro"
        write(PercursoFinal).


obterValoresAEstrela(Origem,[],PercursoObtido,DistanciaObtida,TIEtapa,TFEtapa,MTAnterior,MTAnterior) :-
        resolve_aestrela(Origem,sede,PercursoObtido/DistanciaObtida),
        calculaTempoUsado(DistanciaObtida,MTAnterior,TempoVolta),
        TFEtapa is TIEtapa + TempoVolta.
        
obterValoresAEstrela(Origem,[Encomenda|Restantes],PercursoTotal,DistTotal,TIEtapa,TFinal,MTAnterior,MTFinal) :-
        obterFreguesia(Encomenda,Paragem), %Descobre qual a freguesia onde se deve entregar a encomenda  
        resolve_aestrela(Origem,Paragem,PercursoAtual/DistanciaAtual), %Obtem o percurso que será feito nesta etapa

        detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTNecessario,DuracaoEtapaPrevista), %Obtem o meio de transporte recomendado, e o tempo necessário para executar esta etapa
        escolheMeioTransporteNecessario(MTAnterior,MTNecessario,MTProximo),

        (MTNecessario \= MTProximo -> calculaTempoUsado(DistanciaAtual,MTProximo,DuracaoEtapa), !; DuracaoEtapa is DuracaoEtapaPrevista), %Se não se usar o Meio de transporte obtido é porque esta etapa será feita em menos tempo

        TempoAtual is TIEtapa + DuracaoEtapa,   
        obterValoresAEstrela(Paragem,Restantes,[_|PercursoResultante],DistResultante,TempoAtual,TFEtapa,MTProximo,MTFinal),

        (MTFinal \= MTProximo -> 
            detMeioTransporte(Encomenda,DistanciaAtual,TIEtapa,MTFinal,TempoReal), TFinal is TFEtapa - DuracaoEtapa + TempoReal,!;
            TFinal is TFEtapa, !),

        append(PercursoAtual,PercursoResultante,PercursoTotal),
        DistTotal is DistanciaAtual + DistResultante.


escolheMeioTransporteNecessario(carro,_,carro) :- !.
escolheMeioTransporteNecessario(_,carro,carro) :- !.
escolheMeioTransporteNecessario(mota,_,mota) :- !.
escolheMeioTransporteNecessario(_,mota,mota) :- !.
escolheMeioTransporteNecessario(_,_,bicicleta) :- !.

ordenaEncomendasTempo([],[]).
ordenaEncomendasTempo(Encomendas, L) :- ordenaLista(Encomendas,L).

ordenaLista(Encomendas,L) :- findall((E,T),(member(E,Encomendas),obterTempoMaximo(E,T)),L1), sort(2, @=<, L1, L2), ordenaListaAux(L2,L).

ordenaListaAux(L1,L) :- findall(E, (member(X,L1), left(X,E)), L).

left((L,_), L).


%----------Predicados de Comparação de Circuitos-------------
%------------------------------------------------------------
copiarLista(L,R) :- copiar(L,R).
copiar([],[]).
copiar([H|T1],[H|T2]) :- copiar(T1,T2). 

%Predicado que permite determinar o percurso mais rápido(critério: Distancia)
gerarPercursoMaisRapido(Encomenda,Percurso,D) :-
                    gerarPercursoBFS(Encomenda, Percurso1, D1,_,_),
                
                    gerarPercursoDFS(Encomenda, Percurso2, D2,_,_),
                
                    (D1 < D2 -> copiarLista(Percurso1,PercursoF), D is D1; copiarLista(Percurso2,PercursoF), D is D2),
                    
                    gerarPercursoGulosa(Encomenda, Percurso3, D3,_,_),
                  
                    (D3 < D -> copiarLista(Percurso3,PercursoF1), D is D3; copiarLista(PercursoF,PercursoF1)),
                    
                    gerarPercursoAEstrela(Encomenda, Percurso4, D4,_,_),
                    (D4 < D -> copiarLista(Percurso4,PercursoF2), D is D4;copiarLista(PercursoF1,PercursoF2)),
                    
                    gerarPercursoBILP(Encomenda, Percurso5, D5,_,_,10),
                    (D5 < D -> copiarLista(Percurso5,PercursoF3), D is D5;copiarLista(PercursoF2,PercursoF3)),
                    copiarLista(PercursoF3,Percurso). 

%Predicado que permite determinar o percurso mais ecologico(critério: Tempo)
gerarPercursoMaisEcologico(Encomenda,Percurso,T) :-
                    gerarPercursoBFS(Encomenda, Percurso1,_,_,T1),
                
                    gerarPercursoDFS(Encomenda, Percurso2,_,_,T2),
                
                    (T1 < T2 -> copiarLista(Percurso1,PercursoF), T is T1; copiarLista(Percurso2,PercursoF), T is T2),
                    
                    gerarPercursoGulosa(Encomenda, Percurso3,_,_,T3),
                  
                    (T3 < T -> copiarLista(Percurso3,PercursoF1), T is T3; copiarLista(PercursoF,PercursoF1)),
                    
                    gerarPercursoAEstrela(Encomenda, Percurso4,_,_,T4),
                    (T4 < T -> copiarLista(Percurso4,PercursoF2), T is T4;copiarLista(PercursoF1,PercursoF2)),
                    
                    gerarPercursoBILP(Encomenda, Percurso5,_,_, T5, 10),
                    (T5 < T -> copiarLista(Percurso5,PercursoF3), T is T5;copiarLista(PercursoF2,PercursoF3)),
                    copiarLista(PercursoF3,Percurso).                     


%---------Predicados que interagem com o percurso------------
%------------------------------------------------------------
getVol((_,V,_), V).
getPeso((_,_,P), P).
getPercurso((Perc,_,_), Perc).

maxVol([(_,V,_)],V) :- !.
maxVol([(_,V,_)|Xs], M):- maxVol(Xs, M), M >= V.
maxVol([(_,V,_)|Xs], V):- maxVol(Xs, M), V >  M.

maxPeso([(_,_,P)],P) :- !.
maxPeso([(_,_,P)|Xs], M):- maxPeso(Xs, M), M >= P.
maxPeso([(_,_,P)|Xs], P):- maxPeso(Xs, M), P >  M.

findPercursoWithVol([H|T],Vol,Percurso) :- getVol(H,Vol1), (Vol1 == Vol -> getPercurso(H,Percurso),! ; findPercursoWithVol(T,Vol,Percurso)).

findPercursoWithPeso([H|T],Peso,Percurso) :- getPeso(H,Peso1), (Peso1 == Peso -> getPercurso(H,Percurso),! ; findPercursoWithPeso(T,Peso,Percurso)).

circuitoComMaisEntregasPorVolume(Percurso,V) :- findall((A,B,C), percurso(A,B,C), L),
                                                maxVol(L,V),
                                                findPercursoWithVol(L,V,Percurso).


circuitoComMaisEntregasPorPeso(Percurso,P) :- findall((A,B,C), percurso(A,B,C), L),
                                                maxPeso(L,P),
                                                findPercursoWithPeso(L,P,Percurso).

%Percurso: Não pode existir nenhum percurso igual
getInfPercurso([(P,V,Peso)], (P,V,Peso)).

atualizaPercurso(percurso(P,V,Peso)) :- findall((P,Vol,Peso1),percurso(P,Vol,Peso1),L),length(L,N), N>0,getInfPercurso(L,(P,Vol,Peso1)), VolF is V+Vol, PesoF is Peso+Peso1,evolucaoPercurso(percurso(P,Vol,Peso1),percurso(P,VolF,PesoF)).
atualizaPercurso(percurso(P,V,Peso)) :- assert(percurso(P,V,Peso)).

evolucaoPercurso(PercursoOriginal,PercursoNovo) :- retract(PercursoOriginal),assert(PercursoNovo). 