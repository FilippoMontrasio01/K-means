%Montrasio Filippo 875551



/*kmeans(Objects, K,Result):  funzione che avvia l'algoritmo, prende
in input una lista di oggetti che devono essere clusterizzati in K
cluster, Result continee la lista dei cluster finali
*/

kmeans(Objects, K, Result) :-
    random_el(Objects, K, InitialCentroids),
    partitions(Objects, InitialCentroids, InitialClusters),
    km(Objects, [], InitialClusters, Result).


 

/*km(Observation, Clusters, NewClusters, NewClusters):
predicato che itera ricorsivamente e viene utilizzato per aggiornare
i cluster fino a quando converge alla soluzione finale*/

km(_, Clusters, NewClusters, NewClusters) :-
    clustersEqual(Clusters, NewClusters), !.
km(Observations, _, Clusters, Result) :-
    updateCentroids(Clusters, NewCentroids),
    partitions(Observations, NewCentroids, NewClusters2),
    km(Observations, Clusters, NewClusters2, Result).


/*aggiunge un nuovo vettore ai fatti, prende in input il nome del vettore
e il vettore stesso, il vettore viene memorizzato nella base di conoscenza*/
new_vector(Name, Vector) :-
    assert(vector(Name, Vector)).



/*vminus(V1, V2, V3) calcola la differnza tra v1 e v2 e ritorna il risultato
v3*/

vminus([], [], []) :- !.
vminus([X1 | L1], [X2 | L2], [Y | L]) :-
    Y is X1 - X2,
    vminus(L1, L2, L).




/*vplus(V1, V2, V3) somma i vettori. Calcola V1 + V2*/
vplus([], [], []) :- !.
vplus([X1 | L1], [X2 | L2], [Y | L]) :-
    Y is X1 + X2,
    vplus(L1, L2, L).


/*
innerprod(V1,V2,R) calcola il prodotto scalare tra due vettori e restituisce
il risultato in un terzo valore, che ï¿½ uno scalare
*/
innerprod([], [], 0) :- !.
innerprod([H1 | T1], [H2 | T2], R) :-
    innerprod(T1, T2, R2),
    R is R2 + H1 * H2.

/*norm(Vector, Norm) calcola la norma di un vettore e restituisce il
risultato */
norm(Vector, Norm) :-
    innerprod(Vector, Vector, NormSquared),
    sqrt(NormSquared, Norm).



/*pdistance (Point1, Point2, Distance) calcola la distanza tra due punti
nello spazio, operando sulle componenti dei punti, definiti tramite
vettori */
pdistance(Point1, Point2, Distance) :-
    vminus(Point1, Point2, Subtraction), 
    norm(Subtraction, Distance).


/*il predicato viene utilizzato per trovare l'elemento piï¿½ vicino a un
punto specifico in una lista di punti, restituendolo */

getClosest(_, [Point], Point) :- !.
getClosest(X, [Head | Tail], Closest) :-
    pdistance(Head, X, Dist1),
    getClosest(X, Tail, Closest),
    pdistance(X, Closest, Dist2),
    Dist2 < Dist1, !.
getClosest(_, [Head | _], Head).



/*scale_vector(V1,X,V2) calcola il prodotto tra un vettore e uno scalare
il risultato è un vettore */
scale_vector([], _, []) :- !.
scale_vector([Element | Rest], Scalar, [ResultHead | ResultTail]) :-
    ResultHead is Element * Scalar,
    scale_vector(Rest, Scalar, ResultTail).


/*il predicato viene utilizzato per confrontare due vettori, analizzando
componente per componente dei due vettori */
vector_compare([], []) :- !.
vector_compare([X | Xs], [Y | Ys]) :- X = Y, vector_compare(Xs, Ys).


/* il predicato viene utilizzato per sommare tutti i vettori presenti in una
lista di vettori e restituire il risultato*/
collectVectors([Vector], Vector) :- !.
collectVectors([Head | Tail], Result) :-
    collectVectors(Tail, Result1), 
    vplus(Head, Result1, Result).


/*viene utilizzato per calcolare il centroide di un cluster, rappresentato
da una lista di punti (vettori).il centroide rappresenta il punto medio ed è
restituito come risultato del predicato*/
centroid(Vectors, Centroid) :-
    collectVectors(Vectors, X),
    length(Vectors, Size),
    InverseSize is 1 / Size,
    scale_vector(X, InverseSize, Centroid).



/*il predicato viene utilizzato per verificare se un elemento è presente
all'interno di una lista.*/
contains([Head | _], Head) :- !.
contains([_ | Tail], X) :- 
    contains(Tail, X).


/*updateCentroids(V1, V2) viene utilizzato per ricomputare i centroidi dei
cluster dati. Il centroide calcolato viene aggiunto alla testa
della lista dei nuovi centroidi e il predicato procede ricorsivamente con
il resto della lista */
updateCentroids([], []) :- !.
updateCentroids([Vector | RestVectors], [Centroid | RestCentroids]) :-
    centroid(Vector, Centroid),
    updateCentroids(RestVectors, RestCentroids).



/*il predicato viene utilizzato per ottenere l'indice di un valore
all'interno della lista, ritorna l'indice se il valore è presente, -1
altrimenti */
find_index([], _, _, -1) :- !.
find_index([Value | _], Value, Index, Index) :- !.
find_index([_ | Tail], Value, InitialIndex, Index) :-
    NewIndex is InitialIndex + 1,
    find_index(Tail, Value, NewIndex, Index).




find_index2([], _, _, -1) :- !.
find_index2([Head | _], Value, InitialIndex, InitialIndex) :-
    contains(Head, Value), !.
find_index2([_ | Tail], Value, InitialIndex, Index) :-
    NewIndex is InitialIndex + 1,
    find_index2(Tail, Value, NewIndex, Index).



/*il predicato viene utilizzato per ottenere l'elemento di posizione N da una
lista. Restituisce l'elemento e la lista senza elemento */
nth_element([Head | Tail], 0, (Head, Tail)) :- !.
nth_element([Head | Tail], N, (Element, [Head | RestTail])) :-
    N1 is N - 1,
    nth_element(Tail, N1, X),
    (Element, RestTail) = X.



/*compare(cl1, cl2) viene utilizzato per confrontare due cluster, confronta
fino a quando tutti i punti di cl1 vengono confrontati con i punti
corrispondenti in cl2, o finchè i due cluster differiscono */
compareClusters([], []) :- !.
compareClusters([Elem1 | Rest1], Cluster2) :-
    find_index(Cluster2, Elem1, 0, Index),
    Index \== -1,
    nth_element(Cluster2, Index, X),
    (_, Rest) = X,
    compareClusters(Rest1, Rest).




/*il predicato viene utilizzato per estrarre K elementi casuali da una lista
L*/
random_el(_, 0, []) :- !.
random_el(List, K, [RandomElem | RestResult]) :-
    length(List, ListSize),
    random(0, ListSize, RandomIndex),
    nth_element(List, RandomIndex, X),
    (RandomElem, RemainingList) = X,
    K2 is K - 1,
    random_el(RemainingList, K2, RestResult).



/*il predicato verifica se due cluster sono uguali, prende in input due
cluster */
clustersEqual([], []) :- !.
clustersEqual([[Head1 | Tail1] | Rest], Cluster2) :-
    find_index2(Cluster2, Head1, 0, Index),
    Index \== -1,
    nth_element(Cluster2, Index, X),
    (Head2, Tail2) = X,
    compareClusters([Head1 | Tail1], Head2),
    clustersEqual(Rest, Tail2).



consecutiveEquals([X], [X]) :- !.
consecutiveEquals([(Index1, Value1), (Index1, Value2) | Tail],
		  [(Index1, Value1) | Rest]) :-
    consecutiveEquals([(Index1, Value2) | Tail], Rest), !.
consecutiveEquals([X | _], [X]).



/*il predicato rimuove i primi N elementi da una lista e restituisce il
risultato in Result */
removeFirstN(List, 0, List) :- !.
removeFirstN([_ | Tail], N, Result) :-
    N1 is N - 1,
    removeFirstN(Tail, N1, Result).


/* il predicato viene utilizzato per trovare la coppia più vicina di punti
tra una lista di punti e una lista di cluster*/
nearestPairs([], _, []) :- !.
nearestPairs([Head | Tail], Clusters, [(Closest, Head) | RestPairs]) :-
    getClosest(Head, Clusters, Closest),
    nearestPairs(Tail, Clusters, RestPairs).


removeFirstEl([], []) :- !.
removeFirstEl([(_, X) | Tail], [X | Tail2]) :-
    removeFirstEl(Tail, Tail2).


/*il predicato viene utilizzato per rimuovere il primo elemento di
ogni cluster in una lista di cluster. Continua ricorsivamente finchè tutti
i primi elementi dei cluster sono rimossi, ottenendo la lista risultante
senza i primi elementi di ogni cluster*/
rmclustHead([], []) :- !.
rmclustHead([Head | Tail], [Head2 | Tail2]) :-
    removeFirstEl(Head, Head2),
    rmclustHead(Tail, Tail2).

/*partitions(Observation, Cluster, Result) suddivide le osserazioni in K
cluster dati i centroidi CS */
partitions(Obs, Clusters, Result) :-
    nearestPairs(Obs, Clusters, Obs2),
    sort(Obs2, Obs3),
    partition0(Obs3, Obs4),
    rmclustHead(Obs4, Result).

partition0([], []) :- !.
partition0(List, [Equals | RestPartitions]) :-
    consecutiveEquals(List, Equals),
    length(Equals, N),
    removeFirstN(List, N, Remaining),
    partition0(Remaining, RestPartitions).




