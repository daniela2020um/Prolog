%---------------Definição dos dados:---------------
%--------------------------------------------------

:- dynamic estafeta/4.
:- dynamic encomenda/7.
:- dynamic entregaIntermedia/3.
:- dynamic entrega/4.
:- dynamic percurso/3.

%Extensao do predicado meioTransporte: Transporte, PesoMaximo, VelocidadeMédia, VertenteEcologica -> {V,F}
meioTransporte(bicicleta,5,10,3).
meioTransporte(mota,20,35,2).
meioTransporte(carro,100,25,1).

%Extensao do predicad estafeta: Codigo, EntregasFeitas, EntregasIntermedias, Prioridade -> {V,F}
estafeta(t00001,[e00001,e00003],[],2).
estafeta(t00002,[e00002],[],1).
estafeta(t00003,[e00004],[e00005],4).
estafeta(t00004,[],[e00006],1).

%Extensao do predicado encomenda: NumeroEncomenda, NumeroCliente, Peso, Volume, TempoMaximo, Freguesia, Data -> {V,F}
%Data: Dia, Mes, Ano -> {V,F}
%Peso: kg
%Volume: dm³
%TempoMaximo: horas
encomenda(e00001,c00001,5,30,2,celeirosAveledaEVimieiro,data(1,5,2021)).
encomenda(e00002,c00002,8,21,8,gualtar,data(1,5,2021)).
encomenda(e00003,c00001,4,14,4,merelimSaoPedroEFrossos,data(1,6,2021)).
encomenda(e00004,c00003,21,15,48,nogueiroETenoes,data(13,7,2021)).
encomenda(e00005,c00004,12,34,24,gualtar,data(13,7,2021)).
encomenda(e00006,c00002,3,9,96,nogueiroETenoes,data(4,12,2021)).
encomenda(e00007,c00005,8,12,18,merelimSaoPedroEFrossos,data(5,12,2021)).
encomenda(e00008,c00006,2,8,72,gualtar,data(28,11,2021)).
encomenda(e00009,c00001,4,7,48,lomarEArcos,data(28,11,2021)).
encomenda(e00010,c00007,17,32,96,realDumeESemelhe,data(27,11,2021)).

%Extensao do predicado entregaIntermedia: NumeroEncomenda, MeioTransporte, Preco -> {V,F}
entregaIntermedia(e00001,bicicleta,22.15).
entregaIntermedia(e00002,mota,22.04).
entregaIntermedia(e00003,bicicleta,17.75).
entregaIntermedia(e00004,carro,17.44).
entregaIntermedia(e00005,mota,15.04).
entregaIntermedia(e00006,bicicleta,8.05).

%Extensao do predicado entrega: NumeroEncomenda, TempoNecessario, Classificacao, Data -> {V,F}
entrega(e00001,1.5,3,data(1,5,2021)).
entrega(e00002,6,3,data(1,5,2021)).
entrega(e00003,1.5,3,data(1,6,2021)).
entrega(e00004,56,4,data(13,7,2021)).


%----------------------Grafo-----------------------
%--------------------------------------------------
%Estensão do predicado viagem: FreguesiaOrigem, FreguesiaDestino, Distancia -> {V,F}
viagem(sede, maximinosSeECividade, 0).
viagem(saoVitor, saoVicente, 4.9).
viagem(saoVitor, nogueiroETenoes, 3.1).
viagem(saoVitor, nogueiraFraiaoELamacaes, 3.9).
viagem(saoVitor, saoJoseDeSaoLazaroESaoJoaoDoSouto, 3).
viagem(nogueiraFraiaoELamacaes,saoJoseDeSaoLazaroESaoJoaoDoSouto,3.9).
viagem(nogueiraFraiaoELamacaes,nogueiroETenoes,4.3).
viagem(merelimSaoPedroEFrossos, realDumeESemelhe,3.1).
viagem(realDumeESemelhe,maximinosSeECividade,3.7).
viagem(realDumeESemelhe,merelimSaoPaioPanoiasEParadaDeTibaes,4.2).
viagem(realDumeESemelhe,mireDeTibaes,3.9).
viagem(realDumeESemelhe,sequeira,7.5).
viagem(realDumeESemelhe,ferreirosEGondizalves,4.9).
viagem(ferreirosEGondizalves,sequeira,3.8).
viagem(ferreirosEGondizalves,lomarEArcos,2.8).
viagem(ferreirosEGondizalves,celeirosAveledaEVimieiro,3.6).
viagem(celeirosAveledaEVimieiro,lomarEArcos,4.4).
viagem(celeirosAveledaEVimieiro,vilacaEFradelos,3.2).
viagem(vilacaEFradelos,sequeira,1.7).
viagem(mireDeTibaes,merelimSaoPaioPanoiasEParadaDeTibaes,4.4).
viagem(merelimSaoPedroEFrossos,merelimSaoPaioPanoiasEParadaDeTibaes,1.2).
viagem(nogueiraFraiaoELamacaes,lomarEArcos,2.6).
viagem(lomarEArcos,maximinosSeECividade,3.1).
viagem(saoJoseDeSaoLazaroESaoJoaoDoSouto,lomarEArcos,2.9).
viagem(saoVicente,realDumeESemelhe,1.6).
viagem(saoVitor,gualtar,2.8).
viagem(gualtar,nogueiroETenoes,4.9).
viagem(sequeira,celeirosAveledaEVimieiro,3.5).
viagem(saoJoseDeSaoLazaroESaoJoaoDoSouto,saoVicente,6).
viagem(maximinosSeECividade,ferreirosEGondizalves,1.6).
viagem(maximinosSeECividade,saoJoseDeSaoLazaroESaoJoaoDoSouto,2.2).
viagem(maximinosSeECividade,saoVicente,5).


%-------------------Estimativa---------------------
%--------------------------------------------------
%Extensao do predicado estimativa: Freguesia, Distancia -> {V,F}
estimativa(sede,0).
estimativa(maximinosSeECividade,0).
estimativa(mireDeTibaes,5.5).
estimativa(merelimSaoPaioPanoiasEParadaDeTibaes,6.7).
estimativa(merelimSaoPedroEFrossos,5.5).
estimativa(saoVicente,5).
estimativa(saoJoseDeSaoLazaroESaoJoaoDoSouto,2.2).
estimativa(lomarEArcos,3.1).
estimativa(realDumeESemelhe,3.7).
estimativa(gualtar,6.8).
estimativa(nogueiroETenoes,6.1).
estimativa(nogueiraFraiaoELamacaes,4.5).
estimativa(saoVitor,3.6).
estimativa(sequeira,4.5).
estimativa(vilacaEFradelos,3.6).
estimativa(celeirosAveledaEVimieiro,4.3).
estimativa(ferreirosEGondizalves,1.6).


%--------------------Percurso----------------------
%--------------------------------------------------
%Extensao do predicado percurso: Percurso, Volume, Peso -> {V,F}
%Percursos gerados pelo algoritmo AEstrela

%encomenda e00001:
percurso([sede, maximinosSeECividade, ferreirosEGondizalves, celeirosAveledaEVimieiro, ferreirosEGondizalves, maximinosSeECividade, sede],30,5).

%encomenda e00002, e00005:
percurso([sede, maximinosSeECividade, saoJoseDeSaoLazaroESaoJoaoDoSouto, saoVitor, gualtar, saoVitor, saoJoseDeSaoLazaroESaoJoaoDoSouto, maximinosSeECividade, sede],55,20).

%encomenda e00003:
percurso([sede, maximinosSeECividade, realDumeESemelhe, merelimSaoPedroEFrossos, realDumeESemelhe, maximinosSeECividade, sede],14,4).

%encomenda e00004, e00006:
percurso([sede, maximinosSeECividade, saoJoseDeSaoLazaroESaoJoaoDoSouto, saoVitor, nogueiroETenoes, saoVitor, saoJoseDeSaoLazaroESaoJoaoDoSouto, maximinosSeECividade, sede],24,24).