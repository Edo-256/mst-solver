%%% -*- Mode: Prolog -*-
%Edoardo 856450
%%%% mst.pl

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.


del_all(T) :-
    T = 1,
    retractall(vertex(_, _)),
    retractall(arc(_, _, _, _)),
    retractall(graph(_)),
    retractall(vertex_previous(_, _, _)),
    retractall(vertex_key(_, _, _)),
    retractall(heap(_, _)),
    retractall(heap_entry(_, _, _, _)),
    !.

?- del_all(1).

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

delete_graph(G) :-
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)),
    retractall(graph(G)),
    retractall(vertex_previous(G, _, _)),
    retractall(vertex_key(G, _, _)),
    !.

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :-
    !,
    new_graph(G),
    assert(vertex(G, V)).

graph_vertices(G, Vs) :-
    graph(G),
    findall(vertex(G, C), vertex(G, C), Vs).

list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).

new_arc(G, U, V) :- new_arc(G, U, V, 1), !.
new_arc(_, V, V, _) :- !.
new_arc(G, U, V, Weight) :- arc(G, U, V, Weight), !.
new_arc(G, U, V, Weight) :-
    arc(G, V, U, Weight),
    !,
    retractall(arc(G, V, U, Weight)),
    new_arc(G, U, V, Weight).
new_arc(G, U, V, Weight) :-
    arc(G, U, V, Weight_old),
    !,
    retractall(arc(G, U, V, Weight_old)),
    new_arc(G, U, V, Weight).
new_arc(G, U, V, Weight) :-
    arc(G, V, U, Weight_old),
    !,
    retractall(arc(G, V, U, Weight_old)),
    new_arc(G, U, V, Weight).
new_arc(G, U, V, Weight) :-
    !,
    new_vertex(G, U),
    new_vertex(G, V),
    assert(arc(G, U, V, Weight)).

graph_arcs(G, Es) :-
    graph(G),
    findall(arc(G, U, V, W),
	    arc(G, U, V, W),
	    Es).

vertex_neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), First),
    findall(arc(G, V, Z, W), arc(G, Z, V, W), Second),
    append(First, Second, Ns).

adjs(G, V, Vs) :-
    vertex(G, V),
    findall(vertex(G, X), arc(G, V, X, W), First),
    findall(vertex(G, Z), arc(G, Z, V, W), Second),
    append(First, Second, Vs).

list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).

list_graph(G) :-
    list_vertices(G),
    list_arcs(G).

pp_create_all_arcs(_, []).
pp_create_all_arcs(G, [X | Xs]) :-
    X = line(A1, A2, A3),
    new_arc(G, A1, A2, A3),
    pp_create_all_arcs(G, Xs),
    !.

read_graph(G, FileName) :-
    delete_graph(G),
    new_graph(G),
    csv_read_file(FileName,
		  Data,
		  [functor(line),separator(0'\t)]),
    pp_create_all_arcs(G, Data).

pp_conversion_arc_line([], []).
pp_conversion_arc_line([X | Xs], [Y | Ys]) :-
    X = arc(_, V, N, W),
    Y = line(V, N, W),
    pp_conversion_arc_line(Xs, Ys),
    !.

write_graph(G, FileName) :-
    write_graph(G, FileName, graph), !.
write_graph(G, FileName, graph) :-
    graph(G),
    findall(line(V, N, W), arc(G, V, N, W), OutList),
    csv_write_file(FileName,
		   OutList,
		   [functor(line),separator(0'\t)]),
    !.  
write_graph(InputList, FileName, edges) :-
    pp_conversion_arc_line(InputList, OutputList),
    csv_write_file(FileName,
		   OutputList,
		   [functor(line),separator(0'\t)]),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pp_min_list(Xs, Minimo) :-
    min_list(Xs, Min),
    member(Min, [1.0Inf]),
    !,
    Minimo = inf.
pp_min_list(Xs, Minimo) :-
    min_list(Xs, Min),
    !,
    Minimo = Min.

new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.
new_heap(H, S) :-
    retractall(heap(H, _)),
    assert(heap(H, S)), !.

delete_heap(H) :-
    retractall(heap(H, _)),
    retractall(heap_entry(H, _, _, _)).

heap_has_size(H, S) :- heap(H, S).

heap_empty(H) :-
    heap_has_size(H, 0).

heap_not_empty(H) :-
    heap_has_size(H, Q),
    Q > 0.

list_heap(H) :- listing(heap_entry(H, _, _, _)).

heap_head(H, K, V) :- heap_entry(H, 1, K, V).

heap_insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    nonvar(V),
    heap_entry(H, _, K, V),
    !.

heap_insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    nonvar(V),
    !,
    heap(H, S),
    Som is S + 1,
    retractall(heap(H, _)),
    new_heap(H, Som),
    assert(heap_entry(H, Som, K, V)),
    pp_rev_heapify(H, Som),
    !.

pp_rev_heapify(_H, K) :-
    K = 1, !.
pp_rev_heapify(H, K) :-
    heap_entry(H, K, K1, _V1),
    Div is floor(K/2),
    heap_entry(H, Div, K2, _V2),
    K1 >= K2, !.
pp_rev_heapify(H, K) :-
    heap_entry(H, K, K1, V1),
    Div is floor(K/2),
    heap_entry(H, Div, K2, V2),
    K1 < K2,
    retract(heap_entry(H, K, K1, V1)),
    retract(heap_entry(H, Div, K2, V2)),
    assert(heap_entry(H, K, K2, V2)),
    assert(heap_entry(H, Div, K1, V1)),
    pp_rev_heapify(H, Div).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pp_scambio(H, P1, P2, K1, K2) :-
    heap_entry(H, P1, K1, V1),
    heap_entry(H, P2, K2, V2),
    retractall(heap_entry(H, P1, K1, V1)),
    retractall(heap_entry(H, P2, K2, V2)),
    assert(heap_entry(H, P2, K1, V1)),
    assert(heap_entry(H, P1, K2, V2)).

pp_heapify(H, Pos1) :-
    Pos2 is Pos1 * 2,
    Pos3 is Pos2 + 1,
    heap_entry(H, Pos1, K1, _V1),
    heap_entry(H, Pos2, K2, _V2),
    heap_entry(H, Pos3, K3, _V3),
    !,
    pp_min_list([K1, K2, K3], Kmin),
    pp_sost([Pos1, Pos2, Pos3], [K1, K2, K3], Kmin).
pp_heapify(H, Pos1) :-
    Pos2 is Pos1 * 2,
    heap_entry(H, Pos1, K1, _),
    heap_entry(H, Pos2, K2, _),
    !,
    pp_min_list([K1, K2], Kmin),
    pp_sost([Pos1, Pos2], [K1, K2], Kmin).
pp_heapify(_, _) :- !.

pp_sost([_, _, _], [K1, _, _], Kmin) :-
    Kmin = K1,
    !.
pp_sost([Pos1, Pos2, _], [K1, K2, _], Kmin) :-
    Kmin = K2,
    !,
    pp_scambio(H, Pos1, Pos2, K1, K2),
    pp_heapify(H, Pos2).
pp_sost([Pos1, _, Pos3], [K1, _, K3], Kmin) :-
    Kmin = K3,
    !,
    pp_scambio(H, Pos1, Pos3, K1, K3),
    pp_heapify(H, Pos3).
pp_sost([_, _], [K1, _], Kmin) :-
    Kmin = K1,
    !.
pp_sost([Pos1, Pos2], [K1, K2], Kmin) :-
    Kmin = K2,
    !,
    pp_scambio(H, Pos1, Pos2, K1, K2),
    pp_heapify(H, Pos2).

heap_extract(H, K, V) :-
    nonvar(H),
    var(K),
    var(V),
    heap(H, S),
    S = 0,
    !,
    fail.
heap_extract(H, K, V) :-
    nonvar(H),
    var(K),
    var(V),
    heap(H, S),
    S = 1,
    !,
    heap_entry(H, 1, K, V),
    retract(heap_entry(H, 1, K, V)),
    Sott is S - 1,
    new_heap(H, Sott).
heap_extract(H, K, V) :-
    nonvar(H),
    var(K),
    var(V),
    heap(H, S),
    S > 1,
    !,
    heap_entry(H, 1, K, V),
    heap(H, S),
    retract(heap_entry(H, 1, K, V)),
    retract(heap_entry(H, S, Z, X)),
    assert(heap_entry(H, 1, Z, X)),
    Sott is S - 1,
    new_heap(H, Sott),
    pp_heapify(H, 1),
    !.

modify_key(H, NewKey, OldKey, V) :-    
    heap_entry(H, P, OldKey, V),
    P = 1,
    !,
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    pp_heapify(H, P).

modify_key(H, NewKey, OldKey, V) :-    
    heap_entry(H, P, OldKey, V),
    Pdad is truncate(P / 2),
    heap_entry(H, Pdad, DadKey, _),
    DadKey < NewKey,
    !,
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    pp_heapify(H, P).

modify_key(H, NewKey, OldKey, V) :-    
    heap_entry(H, P, OldKey, V),
    Pdad is truncate(P / 2),
    heap_entry(H, Pdad, DadKey, _),
    DadKey > NewKey,
    !,
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    pp_rev_heapify(H, P).

modify_key(H, NewKey, OldKey, V) :-    
    heap_entry(H, P, OldKey, V),
    Pdad is truncate(P / 2),
    heap_entry(H, Pdad, DadKey, _),
    DadKey = NewKey,
    !,
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pp_ordina_lista(Xs, Zs) :-
    sort(3, @=<, Xs, Ys),
    sort(4, @=<, Ys, Zs).

pp_archi_portano_ai_figli(G, Source, LLs) :-
    findall(arc(G, Source, V, W),
	    (vertex_previous(G, V, Source),
	     vertex_key(G, V, W)),
	    LLs).

mst_get(G, Source, PreorderTree) :-
    nonvar(G),
    nonvar(Source),
    pp_archi_portano_ai_figli(G, Source, LLs),
    pp_ordina_lista(LLs, LLLs),
    procedura(G, LLLs, PreorderTree),
    !.

procedura(_, [], []).
procedura(G, [X | Xs], PreorderTree) :-
    X = arc(G, _, T, _),
    pp_archi_portano_ai_figli(G, T, Ls),
    pp_ordina_lista(Ls, Zs),
    procedura(G, Zs, Tree_X),
    procedura(G, Xs, Last_list),
    append([X], Tree_X, L1),
    append(L1, Last_list, PreorderTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pp_insert_list_atinf_hp(H, [Z | Zs]) :-
    heap_insert(H, inf, Z),
    pp_insert_list_atinf_hp(H, Zs),
    !.
pp_insert_list_atinf_hp(_, []) :- !.
pp_init_minheap(H, G, Source) :-
    new_heap(H),
    pp_graph_vertic_v2(G, Ns),
    delete(Ns, Source, Vs),
    heap_insert(H, 0, Source),
    pp_insert_list_atinf_hp(H, Vs).

pp_adjs_v2(G, V, Vs) :-
    vertex(G, V),
    findall(X, arc(G, V, X, W), First),
    findall(Z, arc(G, Z, V, W), Second),
    append(First, Second, Vs).

pp_graph_vertic_v2(G, Ns) :-
    graph(G),
    findall(C, vertex(G, C), Ns).

pp_weigth_tra_uev(G, U, V, K) :-
    arc(G, U, V, K), !.
pp_weigth_tra_uev(G, U, V, K) :-
    arc(G, V, U, K), !.

mst_prim(G, Source) :-
    nonvar(G),
    nonvar(Source),
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)),
    vertex(G, Source),
    pp_init_minheap(hhh1, G, Source),
    pp_ciclo_heap(G, hhh1),
    !.

pp_ciclo_heap(G, H) :-
    heap_not_empty(H),
    heap_head(H, KU, _),
    KU = inf,
    !,
    heap_extract(H, K, U),
    assert(vertex_key(G, U, K)),
    retractall(vertex_previous(G, U, _)),
    retractall(vertex_previous(G, _, U)),
    pp_ciclo_heap(G, H).  
pp_ciclo_heap(G, H) :-
    heap_not_empty(H),
    !,
    heap_extract(H, K, U),
    assert(vertex_key(G, U, K)),
    pp_adjs_v2(G, U, Vs),
    pp_ciclo_agg(G, H, U, Vs),
    pp_ciclo_heap(G, H).
pp_ciclo_heap(_, H) :-
    heap_empty(H),
    !.

pp_ciclo_agg(G, H, U, [V | Vs]) :-
    heap_entry(H, _, K, V),
    pp_weigth_tra_uev(G, U, V, K1),
    K > K1,
    !,
    modify_key(H, K1, K, V),
    retractall(vertex_previous(G, V, _)),
    assert(vertex_previous(G, V, U)),
    pp_ciclo_agg(G, H, U, Vs).
pp_ciclo_agg(G, H, U, [_ | Vs]) :-
    !,
    pp_ciclo_agg(G, H, U, Vs).
pp_ciclo_agg(_, _, _, []) :- !.

%%%% end of file -- mst.pl
