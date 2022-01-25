-Libreria Lisp per implementazione di un Minimum spanning tree & Minheap
	Questa libreria permette tramite una serie di funzioni di rappresentare
	un vero e proprio grafo indiretto, inserendo nella base di dati i relativi
	vertici, archi, e potendo creare diversi grafi tutti contemporaneamente
	presenti nella base di dati, Inoltre permette di dare questi grafi in input
	ad un algoritmo per la crezione di un MST associato al grafo, e tramite 
	un altra funzione sarà possibile ottenere un attraverasmento preorder dell'MST
	con ordinamento lessicografico (nel caso di archi con pesi uguali) stampando
	una lista degli archi che compongono l'MST generato.
	L'algoritmo che genera l'mst necessita di un min-heap funzionante che è incluso
	nella libreria e utilizzabile anche per altre necessità, tutte le funzioni utili 
	per accedere ed utilizzare il min-heap sono elencate in seguito, l'unico limite 
	è nell'utilizzo dell'heap con nome h111 in quanto è riservato all'algoritmo mst,
	e un suo utilizzo potrebbe essere potenziale fonte di bug.

	Questa libreria farà uso delle hashtable di lisp per memorizzare tutti i dati 
	realtivi alla rappresentazione dei grafi indiretti, degli mst, e dei MinHeap
	Per descrivere una funzione useremo la seguente formattazione
		NOME_FUNZIONE (*inputs*) [*output*]

-La libreria contiene numerose funzioni volte alla rapressentazione di un grafo 
 non diretto e connesso, con pesi non negativi e sono i seguenti:
	-is-graph (graph-id) [graph-id or NIL]
		Questa funzione ritorna il graph-id stesso se questo grafo è già 
		stato creato, oppure NIL se no.
	-new-graph (graph-id) [graph-id]
		Questa funzione genera un nuovo grafo e lo inserisce nel 
		database dei grafi, e ritorna lo stesso
	-delete-graph (graph-id) [NIL]
		Rimuove l’intero grafo dal sistema (vertici archi etc), 
		ovvero rimuove tutte le istanze
		presenti nei data base (ovvero nelle hash-tables) del sistema.
	-new-vertex (graph-id vertex-id) [vertex-rep]
		Aggiunge un nuovo vertice vertex-id al grafo graph-id. 
		Un vertice con uno specifico nome può appartenere a piu grafi
	-graph-vertices (graph-id) [vertex-rep-list]
		Questa funzione torna una lista di vertici del grafo, e sarà 
		una lista di liste così composte (VERTEX NOEM_GRAFO NOME_VERTICE)
	-new-arc (graph-id vertex-id vertex-id weight) [arc-rep]
		Questa funzione aggiunge un arco del grafo graph-id nel datatbase,
		e ritronarà una lista composta da ARC piu gli elementi dati in input.
		L'ultimo paramentro è opzionale e se non viene inserito è uguale a 1
	-graph-arcs (graph-id) [arc-rep-list]
		Questo funzione ritorna una lista una lista di tutti gli archi 
		presenti in graph-id, ciascun elemento di questa lista 
		avrà questa composizione (ARC G V1 V2 W)
	-graph-vertex-neighbors (graph-id vertex-id) [arc-rep-list] 
		Questa funzione ritorna una lista contenente gli archi 
		rappresentati in questo modo (arc graph-id vertex-id N W), 
		che portano ai vertici N immediatamente raggiungibili da vertex-id.
	-graph-vertex-adjacent (graph-id vertex-id) [vertex-rep-list]
		Questa funzione ritorna una lista contenente i vertici 
		rappresentati in questo modo (vertex graph-id vertex-id-x)
		adiacenti a vertex-id.
	-graph-print (graph-id)
		Questa funzione stampa nella console dell’interprete 
		Common Lisp una lista dei vertici e degli archi del grafo graph-id.

-La libreria contiene inoltre 4 funzioni per la risoluzione del problema MST, 
 e sono le seguenti:
	-mst-vertex-key (graph-id vertex-id) [k]
		Questa funzione, dato un vertex-id di un grafo graph-id ritorna, 
		durante e dopo l’esecuzione dell’algoritmo di Prim,
		il peso minimo di un arco che connette vertex-id nell’albero minimo
		se questo arco non esiste allora k è MOST-POSITIVE-DOUBLE-FLOAT.
	-mst-previous (graph-id V) [U]
		Questa funzione, durante e dopo l’esecuzione dell’algoritmo di Prim, 
		ritorna il vertice U 
		che il vertice “genitore” (“precedente”, o “parent”) di V nel MST.
	-mst-prim (graph-id source) [NIL]
		Questa funzione in ogni caso ritorna NIL, ma come effetto collaterale 
		genera un MST per il grafo graph a partire dal nodo source,
		e la rappresentazione di questo MST la troviamo nel database, 
		in particolare nella hashtable *vertex-keys* e *previous*
	-mst-get (graph-id source) [preorder-mst]
		Questa funzione ritorna preorder-mst che è una lista degli archi 
		del MST ordinata secondo un attraversamento preorder dello stesso,
		fatta rispetto al peso dell’arco, gli archi con pari peso saranno
		ordinati secondo l’ordinamento “lessicografico” del vertice “target”.
		Ovviamente questa funzione andrà eseguita dopo l'eseczione si una 
		mst-prim con gli stessi paramentri utilizzati per questa funzione.

-La libreria contiene infine tutta una serie di funzioni volte alla 
 creazione e all'utilizzo di un MinHeap :
	-new-heap (H capacity) [heap-rep]
		Questa funzione crea un nuovo heap H di dimensione capacity, 
		e restituisce una lista che rappresentarà l'heap nel database,
		e che conterrà tutte le componenti di questo heap.
		Capacity è un parametro opzionale e nel caso è omesso 
		viene inizializzato a 42.
	-heap-delete (heap-id) [T]
		Rimuove tutto lo heap indicizzato da heap-id.
	-heap-empty (heap-id) [boolean]
		Questo predicato è vero quando lo heap heap-id non contiene elementi.
	-heap-not-empty (heap-id) [boolean]
		Questo predicato è vero quando lo heap heap-id contiene almeno un elemento.
	-heap-head (heap-id) [(K V)]
		Ritorna una lista di due elementi dove K è la chiave minima e 
		V il valore associato, per l'heap heap-id
	-heap-insert (heap-id K V) [boolean]
		Inserisce l’elemento V nello heap heap-id con chiave K.
	-heap-extract (heap-id) [(K V)]
		La funzione heap-extract ritorna la lista con K, V e con K minima, 
		e la coppia è rimossa dallo heap heap-id.
	-heap-modify-key (heap-id new-key old-key V) [boolean]
		Sostituisce la chiave OldKey (associata al valore V) con NewKey, 
		nello heap heap-id
	-heap-print (heap-id) [boolean]
		Questa funzione stampa sulla console lo stato interno dello heap heap-id.