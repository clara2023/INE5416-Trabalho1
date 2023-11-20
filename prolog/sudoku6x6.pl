valor(1).
valor(2).
valor(3).
valor(4).
valor(5).
valor(6).

%4 OPERADORES
quatroMaior(X) :- valor(X), X =\= 1, X =\= 2, X =\= 3, X =\= 4.
quatroMenor(X) :- valor(X), X =\= 3, X =\= 4, X =\= 5, X =\= 6.
umMenorTresMaior(X) :- valor(X), X =\= 6, X =\= 1, X =\= 2, X =\= 3.
umMaiorTresMenor(X) :- valor(X), X =\= 1, X =\= 4, X =\= 5, X =\= 6.
doisMaiorDoisMenor(X) :- X =\= 1, X =\= 2, X =\= 5, X =\= 6.

%3 OPERADORES
tresMaior(X) :- valor(X), X =\= 1, X =\= 2, X =\= 3.
tresMenor(X) :- valor(X), X =\= 4, X =\= 5, X =\= 6.
umMenorDoisMaior(X) :- valor(X), X =\= 6, X =\= 1, X =\= 2.
umMaiorDoisMenor(X) :- valor(X), X =\= 1, X =\= 5, X =\= 6.

%2 OPERADORES
doisMaior(X) :- valor(X), X =\= 1, X =\= 2.
doisMenor(X) :- valor(X), X =\= 5, X =\= 6.
umMenorUmMaior(X) :- valor(X), X =\= 6, X =\= 1.

maior(X,Y) :- X > Y.
menor(X,Y) :- X < Y.

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

completa([A, B, C, D, E, F]) :-
    valor(A), valor(B), valor(C), valor(D), valor(E), valor(F),
    todosDiferentes([A, B, C, D, E, F]).

solucao(Tabuleiro) :-
    Tabuleiro = [
        [X11, X12, X13, X14, X15, X16],
        [X21, X22, X23, X24, X25, X26],
        [X31, X32, X33, X34, X35, X36],
        [X41, X42, X43, X44, X45, X46],
        [X51, X52, X53, X54, X55, X56],
        [X61, X62, X63, X64, X65, X66]
    ],

    %LINHAS
    completa([X11, X12, X13, X14, X15, X16]),
    completa([X21, X22, X23, X24, X25, X26]),
    completa([X31, X32, X33, X34, X35, X36]),
    completa([X41, X42, X43, X44, X45, X46]),
    completa([X51, X52, X53, X54, X55, X56]),
    completa([X61, X62, X63, X64, X65, X66]),

    %COLUNAS
    completa([X11, X21, X31, X41, X51, X61]),
    completa([X12, X22, X32, X42, X52, X62]),
    completa([X13, X23, X33, X43, X53, X63]),
    completa([X14, X24, X34, X44, X54, X64]),
    completa([X15, X25, X35, X45, X55, X65]),
    completa([X16, X26, X36, X46, X56, X66]),

    %AREAS
    completa([X11, X12, X21, X22, X31, X32]),
    completa([X13, X14, X23, X24, X33, X34]),
    completa([X15, X16, X25, X26, X35, X36]),
    completa([X41, X42, X51, X52, X61, X62]),
    completa([X43, X44, X53, X54, X63, X64]),
    completa([X45, X46, X55, X56, X65, X66]),

    %OPERADORES
    menor(X11,X12),
    maior(X21,X22),
    menor(X31,X32),
    menor(X11,X21),
    maior(X12,X22),
    maior(X21,X31),
    maior(X22,X32),

    maior(X13,X14),
    maior(X23,X24),
    menor(X33,X34),
    menor(X13,X23),
    menor(X14,X24),
    maior(X23,X33),
    menor(X24,X34),

    maior(X15,X16),
    maior(X25,X26),
    menor(X35,X36),
    menor(X15,X25),
    maior(X16,X26),
    menor(X25,X35),
    menor(X26,X36),

    menor(X41,X42),
    maior(X51,X52),
    maior(X61,X62),
    menor(X41,X51),
    menor(X42,X52),
    maior(X51,X61),
    maior(X52,X62),

    menor(X43,X44),
    menor(X53,X54),
    menor(X63,X64),
    maior(X43,X53),
    maior(X44,X54),
    menor(X53,X63),
    menor(X54,X64),

    menor(X45,X46),
    menor(X55,X56),
    maior(X65,X66),
    menor(X45,X55),
    maior(X46,X56),
    menor(X55,X65),
    maior(X56,X66).

    


