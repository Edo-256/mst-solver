-Libreria Prolog per implementazione di un Minimum spanning tree & Minheap
	Questa libreria permette tramite una serie di predicati di rappresentare
	un vero e proprio grafo indiretto, inserendo nella base di dati i relativi
	vertici, archi, e potendo creare diversi grafi tutti contemporaneamente
	presenti nella base di dati, Inoltre permette di dare questi grafi in input
	ad un algoritmo per la crezione di un MST associato al grafo, e tramite 
	un altro predicato sarà possibile ottenere un attraverasmento preorder dell'MST
	con ordinamento lessicografico (nel caso di archi con pesi uguali) stampando
	una lista degli archi che compongono l'MST generato.
	L'algoritmo che genera l'mst necessita di un min-heap funzionante che è incluso
	nella libreria e utilizzabile anche per altre necessità, tutti i predicati utili 
	per accedere ed utilizzare il min-heap sono elencati in seguito, l'unico limite 
	è nell'utilizzo dell'heap con nome h111 in quanto è riservato all'algoritmo mst,
	e un suo utilizzo potrebbe essere potenziale fonte di bug.

-La libreria contiene numerosi predicati volti alla rapressentazione di un grafo non 
 diretto e connesso, con pesi non negativi e sono i seguenti:
	-new_graph(G), sempre vera, e permette di creare il grafo G se non già 
		presente nella base di dati
	-delete_graph(G), sempre vera, cancella completamento un grafo 
		dalla base di conoscenza
	-new_vertex(G, V), sempre vera, aggiunge un vertice ad un grafo, 
		e se questo non esiste, lo crea
	-graph_vertices(G, Vs), Vero se Vs è un lista di vertici del grafo G
	-list_vertices(G), sempre vera, stampa la lista dei vertici di un grafo, 
		stampando i predicati vertex/2
	-new_arc(G, U, V, Weight), aggiunge un arco alla base di dati, un arco nel grafo G, 
		che va dal vertice U al vertice V, con peso Weight, 
		questo ultimo parametro è opzionale e se omesso è uguale a 1.
	-graph_arcs(G, Es), Vero se Es è una lista di tutti gli archi presenti nel grafo G.
	-vertex_neighbors(G, V, Ns) Questo predicato è vero quando V è un vertice di G e Ns 
		è una lista contenente gli archi arc/4 che portano ai 
		vertici immediatamente raggiungibili da V.
	-adjs(G, V, Vs) Questo predicato è vero quando V è un vertice di G e Vs è una lista 
		contenente i vertici nel formato vertex/2, 
		considerando che i grafi sono intesi come indiretti.
	-list_arcs(G) Questo predicato stampa alla console dell’interprete 
		Prolog una lista degli archi del grafo G.
	-list_graph(G) Questo predicato stampa alla console dell’interprete 
		Prolog una lista dei vertici e degli archi del grafo G.
	-read_graph(G, FileName) Questo predicato legge un “grafo” G, 
		da un file FileName e lo inserisce nel data base di Prolog, il file csv
		utilizzerà come separatore il carattere TABULAZIONE 
		anzichè il carattere standard ",".
	-write_graph(G, FileName, Type) Questo predicato è vero quando 
		G viene scritto sul file FileName secondo il valore dell’argomento Type. 
		Type può essere graph o edges. Se Type è graph, allora G dovrà essere un 
		grafo della base di dati Prolog, 
		in FileName saranno scritti gli archi del grafo secondo il formato descitto 
		per read_graph/2. Se Type è edges, 
		allora G dovrà essere una lista di archi, 
		ognuno dei quali viene stampato su FileName, 
		sempre secondo il formato descritto per read_graph/2,
		esiste anche una versione a due parametri di questo predicato, 
		che assume con 3 parametro di default "graph".
	
-La libreria contiene inoltre 4 predicati per la risoluzione del problema MST,
 e sono i seguenti:
	-mst_prim(G, Source) Questo predicato ha successo con un effetto collaterale. 
		Dopo la sua prova, la base-dati Prolog ha al suo interno i predicati 
		vertex_key(G, V, k) per ogni V appartenente a G; la base-dati Prolog 
		contiene anche i predicati vertex_previous(G, V, U) per ogni V, 
		ottenuti durante le iterazioni dell’algoritmo di Prim.
		Naturalmente i predicati vertex_key(G, V, K) e vertex_previous(G, V, U) 
		devono essere corretti rispetto alla soluzione del problema MST.
		Se grafo dato in input contiene nodi non connessi alla parte di 
		grafo connessa al nodo sorgente, questi non avranno vertex_previous,
		e la loro vertex_key sarà uguale a inf.
	-vertex_key(G, V, K) Questo predicato è vero quando V è un vertice di G e,
		durante e dopo l’esecuzione dell’algoritmo di Prim, 
		contiene il peso minimo di un arco che connette V
		nell’albero minimo; se questo arco non esiste
		(ed all’inizio dell’esecuzione) allora K è inf.
	-vertex_previous(G, V, U) Questo predicato è vero quando V ed U sono 
		vertici di G e, durante e dopo l’esecuzione dell’algoritmo di Prim, 
		il vertice U è il vertice “genitore” (“precedente”, o “parent”) 
		di V nel MST generato da prim.
	-mst_get(G, Source, PreorderTree) Questo predicato è vero quando 
		PreorderTree è una lista degli archi del MST ordinata secondo 
		un attraversamento preorder dello stesso, fatta rispetto al peso dell’arco.
		Notare come questo predicato ha 2 vincoli principali impliciti 
		per il suo funzionamento:
		Il primo è il fatto che va eseguito dopo il predicato mst_prim
		Il secondo è che il Source deve coincidere con il Source 
		utilizzato nella chiamata a mst_prim.

-La libreria contiene infine tutta una serie di predicati volti alla creazione 
 e all'utilizzo di un MinHeap:
	che rispetta tutte le caratteristiche di tale struttura, e sono i seguenti:
	-new_heap(H) Questo predicato inserisce un nuovo heap nella base-dati Prolog.
	-delete_heap(H) Rimuove interamente uno heap dalla base-dati Prolog.
	-heap_has_size(H, S) Questo predicato è vero quanto S è la 
		dimensione corrente dello heap H.
	-heap_empty(H) Questo predicato è vero quando lo heap H non contiene elementi.
	-heap_not_empty(H) Questo predicato è vero quando lo heap H 
		contiene almeno un elemento.
	-heap_head(H, K, V) Questo predicato è vero quando l’elemento 
		dello heap H con chiave minima K è V.
	-heap_insert(H, K, V) Questo predicato è vero quando l’elemento V 
		è inserito nello heap H con chiave K
	-heap_extract(H, K, V) Questo predicato è vero quando la coppia K, V 
		con K minima, è rimossa dallo heap H.
	-modify_key(H, NewKey, OldKey, V) Questo predicato è vero quando 
		la chiave OldKey (associata al valore V) è sostituita da NewKey, nell'heap H.
	-list_heap(H) Questo predicato stampa sulla console 
		Prolog lo stato interno dello heap.