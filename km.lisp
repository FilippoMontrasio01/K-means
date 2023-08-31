;;;; Montrasio Filippo 875551



;;;;(kmeans observations k)--> restituisce lista di cluster, error se
;;;;                           observations < k
(defun kmeans (observations k)
  (if (< (length observations) k)
      (error "il numero di osservazioni e' minore di k")
      (km observations
	  '()
	  (partition observations (random_el observations k))
	  )
      )
  )

;;;;(km observations clusters newClusters) --> aggiorna i cluster fino alla
;;;;convergenza, restituisce la lista dei cluster aggiornati
(defun km (observations clusters newClusters)
  (cond
    ((clustersEqual clusters newClusters) clusters)
    (T
     (km
      observations
      newClusters
      (partition observations (updateCentroids newClusters))
      )
     )
    )
  )


;;;; (vplus vector1 vector2) --> v3 che rappresenta la somma di
;;;; vector1 + vector2
(defun vplus (vector1 vector2)
  (cond
    ((= 0 (length vector1)) nil)
    (T (cons (+ (first vector1) (first vector2))
	     (vplus (rest vector1) (rest vector2))))
    )
  )

;;;; (vminus vector1 vector2) --> v3 che rappresenta la differenza tra
;;;; vector1 e vector2
(defun vminus (vector1 vector2)
  (cond
    ((= 0 (length vector1)) nil)
    (T (cons (- (first vector1) (first vector2))
	     (vminus (rest vector1) (rest vector2))))
    )
  )

;;;;(innerprod vector1 vector2) --> restituisce uno scalare che e' il valore
;;;;del prodotto interno tra vector1 e vector2. Il prodotto e' calcolato
;;;;sommando il prodotto delle corrispondenti componenti dei due vettori
(defun innerprod (vector1 vector2)
  (cond
    ((= 0 (length vector1)) 0)
    (T (+ (* (first vector1) (first vector2))
	  (innerprod (rest vector1) (rest vector2))))
    )
  )


;;;;(scale_vector vector scalarNumber)--> restituisce vettore ottenuto
;;;;moltiplicando ogni componente di vector con scalarNumber
(defun scale_vector (vector scalarNumber)
  (cond
    ((= 0 (length vector)) nil)
    (T (cons (* scalarNumber (first vector))
	     (scale_vector (rest vector) scalarNumber)))
    )
  )

;;;;(centroid observations) --> restituisceil vettore che rappresente
;;;;il centroide calcolato per l'insieme di osservazioni. la funzione
;;;;somma i vettori di observations e calcola il punto medio
(defun centroid (observations)
  (scale_vector (reduce #'vplus observations)
		 (/ 1.0 (length observations)))
  )

;;;; (vectorEqual vector1 vector2)--> verifica se due vettori sono uguali,
;;;; restituisce T se lo sono, NIL altrimenti. Nel caso il primo vettore
;;;; non fosse vuoto, la funzione confronta ricorsivamente ogni componente
;;;; del vettore
(defun vectorEqual (vector1 vector2)
  (cond
    ((null vector1) T)
    ((= (first vector1) (first vector2))
     (vectorEqual (rest vector1) (rest vector2)))
    (T '())
    )
  )

;;;;(vectorCompare (vector1 vector2)--> T se vector1 e'  minore di vector2
;;;;in tutte le componenti, NIL altrimenti.Confronta ricorsivamente
;;;; ogni componente
(defun vectorCompare (vector1 vector2)
  (cond
    ((null vector1) nil)
    ((< (first vector1) (first vector2)) T)
    ((> (first vector1) (first vector2)) nil)
    (T (vectorCompare (rest vector1) (rest vector2)))
    )
  )



;;;;(norm vector)-->restituisce il valore della norma 
(defun norm (vector)
  (sqrt (innerprod vector vector) )
  )


;;;;(partition obs clus)--> restituisce una lista di cluster, ognuno
;;;;contenente le osservazioni. Date le osservazioni usa getClosest per
;;;; trovare il centroide pi� vicino, crea coppie contenenti il centroide
;;;; pi� vicino e l'osservazione. Attraverso sort le coppie vengono
;;;;ordinate, tramite vectorCompare. Tramite partition0 vengono creati i
;;;;cluster effettivi partendo dalle coppie
(defun partition (obs clus)
  (mapcar
   #'(lambda (x) (mapcar #'(lambda (y) (second y)) x))
   (partition0 (sort (mapcar #'(lambda (x) (list (getClosest x clus)x))obs)
		     #'(lambda (x y) (vectorCompare (first x) (first y)))))
   )
  )

;;;;(partition0 list)-->lista di cluster contenente le osservazioni. La
;;;;funzione effettua la partizione delle osservazioni in cluster sulla
;;;;base dei centroidi
(defun partition0 (list)
  (cond
    ((null list) nil)
    (T (cons (filterEl list #'(lambda (x) (vectorEqual (car x)
						       (caar list))))
	     (partition0 (removeEl list #'(lambda (x)
					 (vectorEqual (car x)
						      (caar list)))))))
    )
  )


;;;;(nth_element lst n)--> restituisce una lista contenente l'elemento
;;;;all'n-esima posizione e la lista senza n-esimo elemento.
(defun nth_element(lst n)
  (cond
    ((= 0 n) (list (first lst) (rest lst)))
    (T (let ((result (nth_element (rest lst) (- n 1))))
	 (list (first result) (cons (first lst) (second result)))
	 )
       )
    )
  )

;;;;(random_el lst numEl)--> restituisce una lista di numEl elementi scelti
;;;;casulamente dalla lista lst tramite random
(defun random_el (lst numEl)
  (cond
    ((= numEl 0) nil)
    (T (let ((nl (nth_element lst (random (length lst)))))
	 (cons (first nl) (random_el (second nl) (- numEl 1)))
	 )
       )
    )
  )


;;;;(clusterEqual cluster1 cluster2) --> confronta due liste di cluster e
;;;;ritorna T se cluster1 = cluster2, NIL altrimenti
(defun clustersEqual (cluster1 cluster2)
  (cond
    ((= (length cluster1) (length cluster2))
     (cond
       ((null cluster1) T)
       (T
	(let ((idx (find_index cluster2 (caar cluster1)
			       (curry #'hasEl #'vectorEqual) 0)))
	  (cond
	    ((= idx -1) nil)
	    (T
	     (let ((newList (nth_element cluster2 idx)))
	       (and
		(clusterEqual (first cluster1) (first newList))
		(clustersEqual (rest cluster1) (car (cdr newList)))
		)
	       )
	     )
	    )
	  )
	)
       )
     )
    (T nil)
    )
  )


;;;;(clusterEqual cluster1 cluster2) --> confronta due cluster,
;;;;restituisce T se i cluster sono ugali, NIL altrimenti
(defun clusterEqual (cluster1 cluster2)
  (cond
    ((= (length cluster1) (length cluster2))
	(cond
	  ((null cluster1) T)
	  (T
	   (let ((idx (find_index cluster2 (first cluster1) #'vectorEqual 0)))
	     (cond
	       ((= idx -1) nil)
	       (T (let ((newList (nth_element cluster2 idx)))
		    (clusterEqual (rest cluster1) (second newList))
		    )
		  )
	       )
	     )
	   )
	  )
     )
    (T nil)
    )
  )

;;;; (find_index list vector function idx) --> trova l'indice del vettore
;;;; all'interno di una lista. Restituisce l'indice se viene trovato,
;;;; -1 altrimenti
(defun find_index (list vector function idx)
  (cond
    ((null list) -1)
    ((funcall function (first list) vector) idx)
    (T (find_index (rest list) vector function (+ idx 1)))
    )
  )


;;;; (hasEl function list vector)--> verifica se un vettore e'
;;;; presente in una lista. ritorna T se e'  presente, NIL altrimenti
(defun hasEl (function list vector)
  (cond
    ((null list) nil)
    ((funcall function vector (first list)) T)
    (T (hasEl function (rest list) vector))
    )
  )


;;;; (defun curry function argument)--> restituisce una funzione curried
;;;; ossia una funzione con l'argomento fisso preapplicato
(defun curry (function argument)
  (lambda (&rest args) (apply function argument args)))


;;;;(updateCentroids cls)--> restituisce una lista dei nuovi centroidi
;;;; calcolati per ciascun cluster, dati in input una lista dei cluster.
;;;; calcola il centroide di ciascun cluster
(defun updateCentroids (cls)
  (mapcar #'(lambda (x) (centroid x)) cls)
  )



;;;;(filterEl (list function)-->restituisce una lista contenente solo
;;;; gli elementi che soddisfano la condizione di filtraggio definita da
;;;; function
(defun filterEl (list function)
  (cond
    ((null list) nil)
    ((funcall function (first list))
     (cons (first list) (filterEl (rest list) function)))
    (T '())
    )
  )




;;;;(removeEl list function) --> rimuove gli elementi di una lista usando
;;;; una funzione di rimozione e restituisce la lista contenente gli
;;;; elementi che non sono stati rimossi
(defun removeEl (list function)
  (cond
    ((null list) nil)
    ((funcall function (first list)) (removeEl (rest list) function))
    (T list)
    )
  )



;;;; (pdistance point1 point2)--> calcola la distanza tra due punti. I punti
;;;; in input sono rappresentati tramite liste, la distanza e' calcolata
;;;; sottraendo le due liste, componente per componenete, tramite vminus,
;;;; calcola la norma del vettore differenza tramite norm
(defun pdistance (point1 point2)
  (norm (vminus point1 point2))
  )



(defun getClosest (x cluster)
  (cond
    ((null (rest cluster)) (first cluster))
    (T (let
	   ((distance1 (pdistance x (first cluster)))
	    (distance2 (pdistance x (getClosest x (rest cluster))))
	    )
	 (cond
	   ((< distance1 distance2) (first cluster))
	   (T (getClosest x (rest cluster)))
	   )
	 )
       )
    )
  )
	  



 
