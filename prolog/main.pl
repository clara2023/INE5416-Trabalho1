valor(1).
valor(2).
valor(3).
valor(4).
valor(5).
valor(6).
valor(7).
valor(8).
valor(9).

%4 OPERADORES
quatroMaior(X) :- valor(X), X =\= 1, X =\= 2, X =\= 3, X =\= 4.
quatroMenor(X) :- valor(X), X =\= 6, X =\= 7, X =\= 8, X =\= 9.
umMenorTresMaior(X) :- valor(X), X =\= 9, X =\= 1, X =\= 2, X =\= 3.
umMaiorTresMenor(X) :- valor(X), X =\= 1, X =\= 7, X =\= 8, X =\= 9.
doisMaiorDoisMenor(X) :- X =\= 1, X =\= 2, X =\= 8, X =\= 9.

%3 OPERADORES
tresMaior(X) :- valor(X), X =\= 1, X =\= 2, X =\= 3.
tresMenor(X) :- valor(X), X =\= 7, X =\= 8, X =\= 9.
umMenorDoisMaior(X) :- valor(X), X =\= 9, X =\= 1, X =\= 2.
umMaiorDoisMenor(X) :- valor(X), X =\= 1, X =\= 8, X =\= 9.

%2 OPERADORES
doisMaior(X) :- valor(X), X =\= 1, X =\= 2.
doisMenor(X) :- valor(X), X =\= 8, X =\= 9.
umMenorUmMaior(X) :- valor(X), X =\= 9, X =\= 1.

maior(X,Y) :- X > Y.
menor(X,Y) :- X < Y.

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

completa([A, B, C, D, E, F, G, H, I]) :-
    valor(A), valor(B), valor(C), valor(D), valor(E), valor(F), valor(G), valor(H), valor(I),
    todosDiferentes([A, B, C, D, E, F, G, H, I]).

solucao(Tabuleiro) :-
    Tabuleiro = [
        [X11, X12, X13, X14, X15, X16, X17, X18, X19],
        [X21, X22, X23, X24, X25, X26, X27, X28, X29],
        [X31, X32, X33, X34, X35, X36, X37, X38, X39],
        [X41, X42, X43, X44, X45, X46, X47, X48, X49],
        [X51, X52, X53, X54, X55, X56, X57, X58, X59],
        [X61, X62, X63, X64, X65, X66, X67, X68, X69],
        [X71, X72, X73, X74, X75, X76, X77, X78, X79],
        [X81, X82, X83, X84, X85, X86, X87, X88, X89],
        [X91, X92, X93, X94, X95, X96, X97, X98, X99]
    ],

    %LINHAS
    completa([X11, X12, X13, X14, X15, X16, X17, X18, X19]),
    completa([X21, X22, X23, X24, X25, X26, X27, X28, X29]),
    completa([X31, X32, X33, X34, X35, X36, X37, X38, X39]),
    completa([X41, X42, X43, X44, X45, X46, X47, X48, X49]),
    completa([X51, X52, X53, X54, X55, X56, X57, X58, X59]),
    completa([X61, X62, X63, X64, X65, X66, X67, X68, X69]),
    completa([X71, X72, X73, X74, X75, X76, X77, X78, X79]),
    completa([X81, X82, X83, X84, X85, X86, X87, X88, X89]),
    completa([X91, X92, X93, X94, X95, X96, X97, X98, X99]),

    %COLUNAS
    completa([X11, X21, X31, X41, X51, X61, X71, X81, X91]),
    completa([X12, X22, X32, X42, X52, X62, X72, X82, X92]),
    completa([X13, X23, X33, X43, X53, X63, X73, X83, X93]),
    completa([X14, X24, X34, X44, X54, X64, X74, X84, X94]),
    completa([X15, X25, X35, X45, X55, X65, X75, X85, X95]),
    completa([X16, X26, X36, X46, X56, X66, X76, X86, X96]),
    completa([X17, X27, X37, X47, X57, X67, X77, X87, X97]),
    completa([X18, X28, X38, X48, X58, X68, X78, X88, X98]),
    completa([X19, X29, X39, X49, X59, X69, X79, X89, X99]),

    %AREAS
    completa([X11, X12, X13, X21, X22, X23, X31, X32, X33]),
    completa([X14, X15, X16, X24, X25, X26, X34, X35, X36]),
    completa([X17, X18, X19, X27, X28, X29, X37, X38, X39]),
    completa([X41, X42, X43, X51, X52, X53, X61, X62, X63]),
    completa([X44, X45, X46, X54, X55, X56, X64, X65, X66]),
    completa([X47, X48, X49, X57, X58, X59, X67, X68, X69]),
    completa([X71, X72, X73, X81, X82, X83, X91, X92, X93]),
    completa([X74, X75, X76, X84, X85, X86, X94, X95, X96]),
    completa([X77, X78, X79, X87, X88, X89, X97, X98, X99]),

    %OPERADORES
    %Regiao 1
    menor(X11,X12),
    maior(X11,X21),
    menor(X12,X13),
    maior(X12,X22),
    maior(X13,X23),
    menor(X21,X22),
    menor(X21,X31),
    menor(X22,X23),
    maior(X22,X32),
    menor(X23,X33),
    maior(X31,X32),
    menor(X32,X33),

    %Regiao 2
    menor(X14,X15),
    menor(X14,X24),
    menor(X15,X16),
    menor(X15,X25),
    maior(X16,X26),
    maior(X24,X25),
    maior(X24,X34),
    maior(X25,X26),
    maior(X25,X35),
    menor(X26,X36),
    maior(X34,X35),
    menor(X35,X36),

    %Regiao 3
    menor(X17,X18),
    menor(X17,X27),
    menor(X18,X19),
    maior(X18,X28),
    maior(X19,X29),
    maior(X27,X28),
    menor(X27,X37),
    menor(X28,X29),
    menor(X28,X38),
    maior(X29,X39),
    maior(X37,X38),
    menor(X38,X39),

    %Regiao 4
    menor(X41,X42),
    maior(X41,X51),
    maior(X42,X43),
    maior(X42,X52),
    menor(X43,X53),
    menor(X51,X52),
    menor(X51,X61),
    maior(X52,X53),
    maior(X52,X62),
    menor(X53,X63),
    menor(X61,X62),
    maior(X62,X63),

     %Regiao 5
    menor(X44,X45),
    menor(X44,X54),
    maior(X45,X46),
    maior(X45,X55),
    menor(X46,X56),
    maior(X54,X55),
    maior(X54,X64),
    menor(X55,X56),
    maior(X55,X65),
    maior(X56,X66),
    menor(X64,X65),
    maior(X65,X66),

    %Regiao 6
    menor(X47,X48),
    menor(X47,X57),
    maior(X48,X49),
    maior(X48,X58),
    menor(X49,X59),
    maior(X57,X58),
    menor(X57,X67),
    menor(X58,X59),
    menor(X58,X68),
    menor(X59,X69),
    maior(X67,X68),
    maior(X68,X69),

    %Regiao 9
    maior(X71,X72),
    maior(X71,X81),
    menor(X72,X73),
    maior(X72,X82),
    maior(X73,X83),
    maior(X81,X82),
    menor(X81,X91),
    maior(X82,X83),
    menor(X82,X92),
    menor(X83,X93),
    menor(X91,X92),
    maior(X92,X93),

    %Regiao 8
    maior(X74,X75),
    menor(X74,X84),
    menor(X75,X76),
    menor(X75,X85),
    menor(X76,X86),
    menor(X84,X85),
    maior(X84,X94),
    maior(X85,X86),
    maior(X85,X95),
    maior(X86,X96),
    menor(X94,X95),
    maior(X95,X96),

    %Regiao 9
    maior(X77,X78),
    maior(X77,X87),
    menor(X78,X79),
    maior(X78,X88),
    maior(X79,X89),
    maior(X87,X88),
    maior(X87,X97),
    maior(X88,X89),
    menor(X88,X98),
    maior(X89,X99),
    menor(X97,X98),
    maior(X98,X99).
    

