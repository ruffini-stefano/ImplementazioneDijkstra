new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :- assert(vertex(G, V)), !.

vertices(G, Vs) :- findall(vertex(G, X), vertex(G, X), Vs).

list_vertices(G) :-
    vertices(G, Vs),
    write(Vs).

delete_all_vertex(G) :- retractall(vertex(G, _)).

new_arc(G, U, V, W) :- arc(G, U, V, W), !.
new_arc(G, U, V, W) :- assert(arc(G, U, V, W)), !.
new_arc(G ,U, V) :- new_arc(G, U, V, 1).

arcs(G, Es) :- findall(arc(G, U, V, W), arc(G, U, V, W), Es).

list_arcs(G) :-
    arcs(G, Es),
    write(Es).

delete_all_arc(G)  :- retractall(arc(G, _, _, _)).

neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ns), !.

neighborsV(G, V, Ns) :-
    vertex(G, V),
    findall(N, arc(G,V,N,_), Ns), !.

list_graph(G) :-
    vertices(G, Vs),
    arcs(G, Es),
    append(Vs, Es, L),
    write(L).

delete_graph(G) :-
    delete_all_vertex(G),
    delete_all_arc(G),
    retract(graph(G)).

new_heap(H) :- heap(H, _), !.
new_heap(H) :- assert(heap(H, 0)), !.

heap_size(H, S) :- heap(H, S).

empty(H) :- heap(H, 0).

not_empty(H) :- not(empty(H)).

head(H, K, V) :- heap_entry(H, 1, K, V).

delete_heap(H) :-
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).

heapify(_, 1, 0).
heapify(H, P1, P2) :-
    heap_entry(H, P1, K1, _),
    heap_entry(H, P2, K2, _),
    K1 >= K2.
heapify(H, P1, P2) :-
    heap_entry(H, P1, K1, V1),
    heap_entry(H, P2, K2, V2),
    K1 < K2,
    assert(heap_entry(H, P1, K2, V2)),
    assert(heap_entry(H, P2, K1, V1)),
    retract(heap_entry(H, P1, K1, V1)),
    retract(heap_entry(H, P2, K2, V2)),
    P3 is floor(P2/2),
    heapify(H,P2,P3).

insert(H, K, V) :-
    heap_size(H, P),
    P1 is P + 1,
    assert(heap(H, P1)),
    retract(heap(H, P)),
    assert(heap_entry(H, P1, K, V)),
    P2 is floor(P1/2),
    heapify(H, P1, P2),
    !.

insertAll(_, [], []).
insertAll(H, [K|Ks], [V|Vs]) :-
    insert(H, K, V),
    insertAll(H, Ks, Vs),
    !.

list(H, Ks, Vs) :-
    findall(K, heap_entry(H, _, K, _),Ks),
    findall(V, heap_entry(H, _, _, V), Vs).

list_heap(H) :-
    findall(heap_entry(H, P, K, V), heap_entry(H, P, K, V), L),
    write(L).

extract(H, K, V) :-
    heap_entry(H, 1, K, V),
    heap_size(H, S),
    ext(H, S),
    !.

ext(H, 1) :-
    retract(heap_entry(H, 1, _, _)),
    retract(heap(H, 1)),
    assert(heap(H, 0)),
    !.
ext(H, 2) :-
    retract(heap_entry(H, 1, _, _)),
    retract(heap_entry(H, 2, K, V)),
    assert(heap_entry(H, 1, K, V )),
    retract(heap(H, 2)),
    assert(heap(H, 1)),
    !.
ext(H, 3) :-
    heap_entry(H, 2, K2, V2),
    heap_entry(H, 3, K3, V3),
    K2 < K3,
    retract(heap_entry(H, 1, _, _)),
    retract(heap_entry(H, 2, K2, V2)),
    retract(heap_entry(H, 3, K3, V3)),
    assert(heap_entry(H, 1, K2, V2)),
    assert(heap_entry(H, 2, K3, V3)),
    retract(heap(H, 3)),
    assert(heap(H, 2)),
    !.

ext(H, 3) :-
    heap_entry(H, 2, K2, V2),
    heap_entry(H, 3, K3, V3),
    K2 >= K3,
    retract(heap_entry(H, 1, _, _)),
    retract(heap_entry(H, 2, K2, V2)),
    retract(heap_entry(H, 3, K3, V3)),
    assert(heap_entry(H, 1, K3, V3)),
    assert(heap_entry(H, 2, K2, V2)),
    retract(heap(H, 3)),
    assert(heap(H, 2)),
    !.
ext(H, S) :-
    S > 3,
    retract(heap_entry(H, 1, _, _)),
    list(H, Ks, Vs),
    delete_heap(H),
    assert(heap(H, 0)),
    insertAll(H, Ks, Vs),
    !.

modify_key(H, NewKey, OldKey, V) :-
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    list(H, Ks, Vs),
    delete_heap(H),
    assert(heap(H, 0)),
    insertAll(H, Ks, Vs),
    insert(H, NewKey, V),
    !.

change_dist(G, V, NewDist) :-
    retractall(dist(G, V, _)),
    assert(dist(G, V, NewDist)).

change_previous(G, V, U) :-
    retractall(previous(G, V, _)),
    assert(previous(G, V, U)).

inserth(G, H, W) :-
    heap_entry(G, _, K, H),
    modify_key(G, W, K, H),
    !.
inserth(G, H, W) :-
    insert(G, W, H),
    !.

aggiorna(_, _, []).
aggiorna(G, S, L) :-
    L = [H|T],
    dist(G, H, W1),
    dist(G, S, W2),
    arc(G, S, H, W3),
    W4 is W2 + W3,
    W1  =< W4,
    aggiorna(G, S, T),
    !.
aggiorna(G, S, L) :-
    L = [H|T],
    dist(G, H, W1),
    dist(G, S, W2),
    arc(G, S, H, W3),
    W4 is W2 + W3,
    W1 > W4,
    change_dist(G, H, W4),
    change_previous(G, H, S),
    inserth(G, H, W4),
    aggiorna(G, S, T),
    !.
aggiorna(G, S, L) :-
    L = [H|T],
    dist(G, S, W1),
    arc(G, S, H, W2),
    W3 is W1 + W2,
    change_dist(G, H, W3),
    change_previous(G, H, S),
    inserth(G, H, W3),
    aggiorna(G, S, T),
    !.

sssp(G, Source) :-
    sp1(G, Source),
    sp2(G).

sp1(G, Source) :-
    vertex(G, Source),
    assert(dist(G, Source, 0)),
    assert(visited(G, Source)),
    new_heap(G),
    neighborsV(G, Source, L),
    aggiorna(G, Source, L),
    !.

sp2(G) :-
    not_empty(G),
    extract(G, _, V),
    assert(visited(G, V)),
    neighborsV(G, V, L),
    aggiorna(G, V, L),
    sp2(G),
    !.
sp2(G) :-
    empty(G),
    !.

add(X, [], [X]).
add(X, Y, Z) :- append(Y, [X], Z), !.

f(G, S, N, L1, L2, L3) :-
    N \= S,
    add(N, L1, L2),
    previous(G, N, X),
    f(G, S, X, L2, _, L3),
    !.
f(_, _, N, L1, _, L3) :-
    add(N, L1, L3).

archi(G, L, L1, L3) :-
    L = [H1, H2|T],
    findall(arc(G, H2, H1, W), arc(G, H2, H1, W), R1),
    append(R1, L1, R2),
    archi(G, [H2|T], R2, L3),
    !.
archi(_, _, L1, L3) :-
    L3 = L1,
    !.

shortest_path(G, Source, V, Path) :-
    f(G, Source, V, [], _, R1),
    archi(G, R1, [], Path).