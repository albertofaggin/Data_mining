# Un grande supermercato, con un'area di circa 12000mq, ha registrato tutte le 
# transazioni effettuate dai 7301 possessori di una carta fedeltà in 75 giorni 
# consecutivi. Ad ogni carta fedeltà sono stati associati tutti i prodotti 
# acquistati nel periodo considerato, e questi sono stati poi raggruppati in 
# 493 categorie merceologiche.

# Gli statistici del supermercato, per una prima serie di analisi si concentrano 
# sulle 20 categorie merceologiche più frequenti, e a tal fine 
# sono state selezionate le 6739 carte fedeltà i cui clienti avessero acquisto 
# prodotti appartenenti a queste 20 categorie. Nel file mba.txt sono presenti 
# per ciascuna di queste carte fedeltà (righe) l'indicatore di quali prodotti 
# (colonne) sono stati acquistati nel periodo considerato.

# E' interesse della divisione commerciale del supermercato comprendere la 
# struttura associativa nei comportamenti di acquisto, per intraprendere azioni 
# di marketing adeguate.

# Si vuole sapere, cioè, quali prodotti i singoli clienti acquistano maggiormente insieme.

# Una seconda richiesta dell'ufficio marketing del supermercato riguarda il 
# confronto tra diversi procedure di analisi. Il responsabile vi chiede infatti 
# di effettuare l'analisi seguendo almeno due diverse procedure che si basano su
# diversi strumenti statistici (sceglieteli voi tra quelli che conoscete adatti 
# al problema), e confrontare poi i risultati ottenuti. Sono analoghi? Commentare
# le analogie e differenze e cercare di dare una spiegazione di eventuali differenze.

###############################################################################
####################### Lettura e pulizia del data - set #######################
################################################################################
rm(list=ls())
setwd("~/Desktop/Esami/Data mining/05. Data set")
source("funzioni.R")

dati = read.csv("mba.txt", header = T, sep = "")
dati1 = dati
str(dati)
View(dati)

# Dopo la lettura del dataset, nella fase iniziale dell’analisi si vuole 
# assicurare che tutte le variabili siano state lette e codificate correttamente. 
# Si inizia dunque con la fase di ricodifica preliminare e preanalisi. 

# Come prima cosa è possibile osservare che nel data - set è presente una variabile 
# che rappresenta il codice identificativo, definita da (COD_CARD).
# Non presentando un apporto significativo ed informativo nella fase di previsione
# del modello, è stata tolta.
dati$COD_CARD = NULL

# Successivamente, osserviamo se sono presenti dei valori mancanti:
find.NA(dati)
find.miss(dati)

# Le variabili in questione sono "Product_tomato", "source", "Product_tunny", 
# "Product_water" e "Product_yoghurt" presentano tanti valori mancanti quante
# sono le osservazioni. Per tale ragione è possibile togliere direttamente
# le variabili in questione 

dati = dati[,-c(21:26)]
str(dati)

# Osservando le caratteristiche del data - set proposto (20 variabili in cui sono
# presenti solamente osservazioni binarie), per osservare la relazione che sussiste
# tra l'acquisto di diversi prodotti considerare le reti, poiché una rete è
# caratterizzatta da un insieme di connessioni (archi) che misurano relazioni 
# tra un gruppo di unità (nodi), come in questo caso. Per tale ragione 
# rasformo il data set in una matrice:
market = as.matrix(dati)
View(market)
# In questo modo sarà possibile definire una rete binaria che va a misurare
# le relazioni tra i diversi prodotti del supermercato. Per effettuare l'analizi,
# però, devo considerare solamente matrici quadrate. 
market = market[1:20, 1:20]

library(igraph)
net = graph_from_adjacency_matrix(market, mode = "undirected",
                                  weighted = NULL,diag = F) 

# La funzione "graph_from_adjacency_matrix" prende come input la matrice "market", 
# consideriamo rete di tipo indiretto e non pesata, 

# Per rete di tipo indiretto si intende archi non rappresentano una relazione 
# diretta tra i nodi. Questo tipo di rete è particolarmente utizzata per:
# • per analizzare la diffusione di informazione/influenza all'interno della rete
# • per identificare i nodi chiave per la diffusione delle informazioni

# Invece, per quanto riguarda una rete non pesata, si intende un tipo di rete in cui ogni 
# arco (cioè, ogni connessione tra due nodi) non ha un peso associato ad esso.
# Questo per rendere più agevole l'analisi e permettendomi di considerare i beni
# come complementare

# Inoltre, non è di interesse valutare il tipo di relazione tra un prodotto con
# sé stesso.

# Possiamo vedere l'insieme di archi e dei nodi della rete:
V(net) # --> Abbiamo 20 nodi che corrisponde ad un diverso prodotto
E(net) # --> Un arco è rappresentato come una coppia di elementi 
       #     che appartengono all'insieme dei vertici. 
# In particolare, abbiamo 61 connessioni.

# La prima statistica che possiamo calcolare a livello di rete è la densità
edge_density(net) 
# Fa l'operazione di calcolare la frequenza relativa di quante connessioni 
# possiamo osservare rispetto al totale. In questo caso di tutte le possibili 
# relazioni di acquisto di un prodotto collegato ad un'altro è del 32%

# Statistica di nodo: il grado
degree(net)
# calcola per ogni nodo della nostra rete a quanti altri nodi è connesso. 
# Potremmo assumere questo valore come grado di "popolarità" del noto.

# Interessante osservare che il grado di un nodo può essere utilizzato per
# analizzare la struttura della rete, poiché una rete con un grado medio elevato 
# indica che i nodi sono fortemente connessi tra loro, mentre una rete con un 
# grado medio basso indica che i nodi sono meno connessi tra loro
mean(degree(net))
# In questo caso possiamo osservare un grado medio di 6 connessioni, ad indicare
# un grado medio.



################################################################################
####################### Analisi esplorative e descrittive ######################
################################################################################

# Dopo questa fase di pre-analisi, è stata effettuata sull’analisi descrittiva 
# ed esplorativa. In questa fase, si vuole dare un primo 
# sguardo al fenomeno d’interesse 
# e alle variabili che ci si aspetta possano essere rilevanti. 


# Osserviamo la distribuzione del grado:
hist(degree(net))

# Otteniamo un tipico andamento: power low o esponenziale negativo. Ossia 
# abbiamo tanti paesi che presentano poche connessioni e, via via, che aumenta 
# il grado del noto/il numero di connessioni, diminuisce il numero di paesi che 
# soddisfa tale condizione.


# Un elemento importante per capire la centralità di un nodo è osservare il livello
# di betweenness. Il livello di betweenness di un nodo in una rete rappresenta 
# la quantità di volte in cui quel nodo appare sui percorsi più brevi tra tutte 
# le coppie di nodi della rete. In altre parole, questo è utile perché ci dice 
# il ruolo chepuò avere un certo prodotto rispetto agli altri.
betweenness(net)
betweenness(net, normalized = T)
plot(net)

# Nel caso in analisi possiamo osservare che 6 variabili presentano 
# un livello di betweenness superiore alla media (PRODUCT_beer, PRODUCT_biscuits,
# PRODUCT_ice, PRODUCT_pasta, PRODUCT_milk e PRODUCT_tinned)
mean(betweenness(net))

# Nel caso in cui volessimo migliorare l'interpretazione grafica, possiamo 
# modificare la dimensione dei nodi all'interno della 
V(net)$size = sqrt(betweenness(net))
plot(net)

# Un'altra caratteristica naturale delle reti è quella di formare dei gruppi
# di nodi simili. Per tale ragione vogliamo trovare i gruppi sulla base delle 
# informazioni di connessione
comm = multilevel.community(net)
# Questa è una lista che contiene due elementi importanti:
#     - membership: è l'indicatore di gruppo, quindi nodi con lo stesso numero 
#                   saranno assegnati alla stessa comunità
#     - modularità: importante misura di coesione di gruppi, misura che possiamo 
#                   utilizzare per valutare quanto buona è la divisione in gruppi 
#                   che il nostro algoritmo ha identificato.

# Estraiamo i primi gruppi:
gr = membership(comm)
# Andiamo a valutare quanto buone o cattive sono queste comunità:
modularity(net, gr)

# La modularità è una misura dimensionale che va da -1 a 1, con valori vicini a 
# 1 che indicano una forte struttura di comunità all'interno della rete e valori 
# vicini a -1 che indicano una debole struttura di comunità.

# Può essere utilizzata per identificare automaticamente le comunità all'interno 
# di una rete, utilizzando algoritmi di ottimizzazione come quello di Girvan-Newman 
# o l'algoritmo di Louvain. Questi algoritmi cercano di suddividere la rete 
# in gruppi di nodi il più possibile densamente connessi tra loro e il meno 
# possibile connessi con i nodi al di fuori del gruppo, per ottenere un valore
# di modularità il più alto possibile.

# Questo può esssere una misura utile per analizzare la struttura di una rete e 
# può essere utilizzata per identificare gruppi di nodi che svolgono funzioni 
# simili o sono coinvolti in processi simili all'interno della rete. 

# Per confrontare questo valore dobbiamo utilizzare:
assortativity(net, gr)
# L'assortatività non è altro che la modularità normalizzata. Se questo valore
# si avvisina ad 1, indica una forte coesione tra i gruppi, questo si traduce in una
# regola empirica che dice che se l'assortatività è >0.30 allora i gruppi trovati 
# sono non banali.

# Usiamo queste quantità anche all'interno di una rappresentazione grafica
V(net)$color = as.vector(gr)
layout_in_circle # se vogliamo rappresentare i nodi su un cerchio equispaziati 
net_lo = layout_with_graphopt(net)
plot(net, layout=net_lo)



################################################################################
################################# Modellazione #################################
################################################################################


######################### Modello con la sola intercetta #######################

library(ergm)
net2 = as.network(market, directed=F)

m0 = ergm(net2~edges, estimate = 'MPLE')
summary(m0)
# La variabile risposta è "net", la nostra rete, le covariate sono le statistiche 
# sufficienti che possono essere specificate come termini. "MPLE" stima di pseudo 
# verosimiglianza, vuol dire che costruiamo una funzione di verosimiglianza 
# costruita mettendo insieme i vari contributi delle distribuzioni condizionate. 
# Quindi abbiamo considerato la distribuzione di ogni arco dato tutto il resto della rete

# "edges" ci da una misura di densità della rete. "triangle" ci da una misura 
# di cosa succede quando un nodo forma un triangolo sulla probabilità di 
# osservare una connessione. L'idea è che se aggiungo un nodo, questo nodo mi 
# va a chiudere il triangolo

# Modelliamo la probabilità di osservare una connessione considerando il numero 
# totale di connessioni che osserviamo → quindi abbiamo di fatto un modello binomiale. 
# Questo nel caso più semplice in cui consideriamo la covariate "edges"

# Probabilità globale di osservare un arco all'interno della nostra rete 
exp(coef(m0))/(1+exp(coef(m0))) # --> stima della densità della rete

# Come la regressione logistica, il parametro associato all'intercetta ci da 
# un'indicazione della probabilità globale della variabile risposta, in questo 
# caso il modello sta facendo la stessa cosa.

############################# Secondo con covariate ############################
m1 = ergm(net2~edges+triangles, estimate = 'MPLE')
summary(m1)

# Se un arco non porta alcun triangolo
plogis(coef(m1)[1]) 
# Probabilità globale nella rete di osservare una connessione 0.1146177

# Se un nodo porta un triangolo
plogis(c(1,1)%*%coef(m1)) 
# Probabilità globale nella rete di osservare una connessione 0.1827003

# Se un nodo porta due triangoli
plogis(c(1,2) %*% coef(m1))
# Andiamo a vedere come funziona la funzione:
ergmMPLE(net2~edges+triangles)
matr = ergmMPLE(net2~edges+triangles)

# Su queste stime possiamo calcolare una regressione binomiale
glm(matr$response ~ -1 + matr$predictor, family = 'binomial',
                weights = matr$weights)

# Quando lavoriamo con la pseudo verosimiglianza abbiamo interessanti proprietà 
# dal punto di vista di riscrittuara del modello. Con la pseudo verosimiglianza 
# basta andare a valutare, per ogni arco, come cambiano le statistiche suff. quando 
# fai finta di cambiare il valore che hai osservato. Per quanto riguarda gli 
# edges osserviamo sempre il valore 1 perché è come se avessimo solo l'intercetta. 
# Invece, per i triangle la questione è differente perché esistono, in questo caso, 
# prodotti più centrali hanno più connessioni/triangoli e andando a rimuoverlo rompo 
# molte strutture e connessioni. Ogni riga della matrice corrisponde ad un arco 
# (response 0/1, presenza assenza). Valuto la change statistic, cioè come 
# cambia g(y) quando y_ij passa da 1 a 0 (o viceversa). Metto insieme tutti 
# quelli per cui questo accade, e metto il totale in weights.


################################################################################
###################### ANALISI DEI RISULTATI E CONCLUSIONI #####################
################################################################################

####################### Valutare la bontà di adattamento #######################

# Per valutare questi modelli è più complicato rispetto le tecniche usate per 
# altri modelli. Non è possibile utilizzare la curva Lift o Roc, perché i dati 
# presentano molta dipendenza, ci servirebbero delle misure ad hoc/degli 
# aggiustamenti che tengono conto della dipendenza che osservo all'interno della mia rete.

# Simulare delle reti è molto semplice per qualsiasi valore dei coefficienti 
# vogliamo utilizzare. Sfrutto questo aspetto e genero un numero elevato di 
# reti artificiali che provengono dalla stima di pseudo massima verosimiglianza. 
# Mi aspetto che se la stima di massima pseudo verosimiglianza è ragionevole 
# le reti che ho generato e la rete che ho osservato si assomigliano. 
# Per vedere se le reti calcolate artificialmente presentano valori distanti 
# da quella calcolata utilizzo delle statistiche, per esempio: il "grado", 
# cioè vado a vedere quante osservazioni ho per ogni grado della funzione, ecc. 
# Avrò tante statistiche quante sono le reti.

par(mfrow = c(2,2))
gf = gof(m1)
plot(gf)
# scala logit
plot(gf,plotlogodds = T)
par(mfrow = c(1,1))

# Interpretazione del grafico:
# La linea nera rappresenta i valori che ho osservato con la mia rete, mentre
# i box - plot indicano la variabilità rispetto alle simulazioni (le reti simulate).
# Dobbiamo vedere se gli andamenti siano simili tra le due curve.



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# Una seconda richiesta dell'ufficio marketing del supermercato riguarda il 
# confronto tra diversi procedure di analisi. Il responsabile vi chiede infatti 
# di effettuare l'analisi seguendo almeno due diverse procedure che si basano su
# diversi strumenti statistici (sceglieteli voi tra quelli che conoscete adatti 
# al problema), e confrontare poi i risultati ottenuti. Sono analoghi? Commentare
# le analogie e differenze e cercare di dare una spiegazione di eventuali differenze.

# Costruisco all'interno del data - set una nuova variabile "y" che rappresenta il 
# numero totale di prodotti acquistati da un cliente. 

library(dplyr)
dati1 <- dati1 %>%
  mutate(y = PRODUCT_beer+PRODUCT_biscuits+PRODUCT_brioches+PRODUCT_coffee+
           PRODUCT_coke+PRODUCT_crackers+PRODUCT_frozen+fish+PRODUCT_frozen.1+
           vegetables+PRODUCT_ice+cream+PRODUCT_juices+PRODUCT_milk+
           PRODUCT_mozzarella+PRODUCT_oil+PRODUCT_pasta+PRODUCT_rice+PRODUCT_tinned+meat)
View(dati1)

save(file = "38. Dati esame.Rdata", list = ls(all.names = T))
