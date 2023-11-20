valor(1).
valor(2).
valor(3).
valor(4).

restricao(X) :- valor(X), X =\= 1, X =\= 2.

exemplo(Y) :- restricao(Y).
    
