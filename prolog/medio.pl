:-  use_module(library(clpfd)), initialization(main).


comparadoresPorCelula(C) :-
    C = [[['.', '.', '<', '>'], ['>', '.', '>', '<'], ['<', '.', '.', '<'], ['.', '.', '<', '>'], ['>', '.', '<', '>'], ['>', '.', '.', '>'], ['.', '.', '>', '<'], ['<', '.', '>', '>'], ['<', '.', '.', '<']],
    [['.', '<', '<', '<'], ['>', '>', '<', '<'], ['>', '>', '.', '<'], ['.', '<', '<', '<'], ['>', '<', '>', '>'], ['<', '<', '.', '>'], ['.', '>', '>', '>'], ['<', '<', '<', '>'], ['>', '>', '.', '>']],
    [['.', '>', '<', '.'], ['>', '>', '<', '.'], ['>', '>', '.', '.'], ['.', '>', '<', '.'], ['>', '<', '>', '.'], ['<', '<', '.', '.'], ['.', '<', '>', '.'], ['<', '<', '<', '.'], ['>', '<', '.', '.']],
    [['.', '.', '>', '>'], ['<', '.', '>', '<'], ['<', '.', '.', '<'], ['.', '.', '>', '>'], ['<', '.', '<', '>'], ['>', '.', '.', '>'], ['.', '.', '<', '<'], ['>', '.', '>', '>'], ['<', '.', '.', '<']],
    [['.', '<', '>', '>'], ['<', '>', '<', '<'], ['>', '>', '.', '<'], ['.', '<', '>', '<'], ['<', '<', '<', '<'], ['>', '<', '.', '>'], ['.', '>', '>', '>'], ['<', '<', '>', '>'], ['<', '>', '.', '>']],
    [['.', '<', '<', '.'], ['>', '>', '>', '.'], ['<', '>', '.', '.'], ['.', '>', '<', '.'], ['>', '>', '>', '.'], ['<', '<', '.', '.'], ['.', '<', '>', '.'], ['<', '<', '<', '.'], ['>', '<', '.', '.']],
    [['.', '.', '<', '>'], ['>', '.', '>', '>'], ['<', '.', '.', '>'], ['.', '.', '>', '<'], ['<', '.', '>', '>'], ['<', '.', '.', '>'], ['.', '.', '<', '<'], ['>', '.', '>', '<'], ['<', '.', '.', '<']],
    [['.', '<', '>', '<'], ['<', '<', '<', '<'], ['>', '<', '.', '>'], ['.', '>', '>', '>'], ['<', '<', '<', '>'], ['>', '<', '.', '<'], ['.', '>', '<', '>'], ['>', '>', '<', '<'], ['>', '>', '.', '>']],
    [['.', '>', '<', '.'], ['>', '>', '>', '.'], ['<', '<', '.', '.'], ['.', '<', '<', '.'], ['>', '<', '<', '.'], ['>', '>', '.', '.'], ['.', '<', '<', '.'], ['>', '>', '>', '.'], ['<', '<', '.', '.']]].



% Acessa a posição dentro de uma lista normal
nthLista(0, [H|_], H).
nthLista(Linha, [_|T], X) :- Linha2 is Linha - 1, nthLista(Linha2, T, X).
% Acessa a posição dentro de uma matriz
nthMatriz(Linha, Coluna, Lista, X) :- nthLista(Linha, Lista, R), nthLista(Coluna, R, X).


/*Define a comparação a ser feita a cada posição dentro do tabuleiro*/
compara(_, _, _, _, '.').
compara(Num1, Tabuleiro, Linha, Coluna, '>') :- nthMatriz(Linha, Coluna, Tabuleiro, Num2), Num1 #> Num2.
compara(Num1, Tabuleiro, Linha, Coluna, '<') :- nthMatriz(Linha, Coluna, Tabuleiro, Num2), Num1 #< Num2.


/* Define o que é um número válido para a posição (Linha, Coluna)*/
define_numero_valido(Num, Linha, Coluna, Tabuleiro, Comparacoes) :- 
    % Pega a ComparadoresPorCelula que define as comparações entre os vizinhos de (Linha, J)
    nthMatriz(Linha, Coluna, Comparacoes, ComparadoresPorCelula),
    % Pega as comparações das 4 direções (left, top, right, bottom)
    nthLista(0, ComparadoresPorCelula, C1),
    nthLista(1, ComparadoresPorCelula, C2),
    nthLista(2, ComparadoresPorCelula, C3),
    nthLista(3, ComparadoresPorCelula, C4),
    % Define onde estão os vizinhos dentro da matriz
    Left is Coluna - 1,
    Top is Linha - 1,
    Right is Coluna + 1,
    Bottom is Linha + 1,
    % Define as comparações a serem feitas em cada direção
    compara(Num, Tabuleiro, Linha, Left, C1),
    compara(Num, Tabuleiro, Top, Coluna, C2),
    compara(Num, Tabuleiro, Linha, Right, C3),
    compara(Num, Tabuleiro, Bottom, Coluna, C4).


/* 
    Percorre todas posições da matriz e define as comparações referentes a 
    cada posição.
*/
regras(_, _, 9, _).
regras(Tabuleiro, Comparadores, Linha, 9) :- N is Linha + 1, regras(Tabuleiro, Comparadores, N, 0).
regras(Tabuleiro, Comparadores, Linha, Coluna) :- 
    nthMatriz(Linha, Coluna, Tabuleiro, Num),
    define_numero_valido(Num, Linha, Coluna, Tabuleiro, Comparadores),
    N is Coluna + 1,
    regras(Tabuleiro, Comparadores, Linha, N).


/*
 Definição o que é um tabuleiro válido de vergleich.
 Um possível tabuleiro válido de vergleich é uma matriz de
 ordem 9 onde cada linha, coluna e região contem todos os números 
 de 1 a 9 sem repetições. Além disso, cada posição deve satisfazer
 certas comparações (maior ou menor) com seus vizinhos de região.
 */
vergleich(Tabuleiro, Comparadores) :-
    length(Tabuleiro, 9), % O tabuleiro tem 9 linhas
    maplist(same_length(Tabuleiro), Tabuleiro), % Cada linha 9 colunas
    append(Tabuleiro, TodosOsNumeros), % Concatena todos os números do tabuleiro em TodosOsNumeros
    TodosOsNumeros ins 1..9, % O tabuleiro tem apenas números entre 1 e 9
    regras(Tabuleiro, Comparadores, 0, 0), % O tabuleiro segue a regra do Tabuleiro vergleichs
    maplist(all_distinct, Tabuleiro), % Todas as linhas possuem números entre 1 e 9 sem repetições
    transpose(Tabuleiro, Columns), % Inverte as linhas e colunas
    maplist(all_distinct, Columns),  % Todas as colunas possuem números entre 1 e 9 sem repetições
    Tabuleiro = [Linha1, Linha2, Linha3, Linha4, Linha5, Linha6, Linha7, Linha8, Linha9], % Nomeia todas as linhas da Matriz
    % A partir das linhas nomeadas, divide em 3 blocos de 3 linhas cada
    % Define que cada bloco contem 3 regiões que devem conter todos os números entre 1 e 9 sem repetições
    regiao(Linha1, Linha2, Linha3),
    regiao(Linha4, Linha5, Linha6),
    regiao(Linha7, Linha8, Linha9).


/*
    Recebe 3 linhas, cada uma com tamanhos iguais entre si que podem ser 9, 6, 3 ou 0
    Vai andando em blocos de 3 por essas linhas até chegar ao final.
    Pega os 3 primeiros itens de cada linha e diz que todos os 9 itens precisam ser diferentes.
    Ao final repete o processo para a calda ou o que sobrou da lista.
*/
regiao([], [], []).
regiao([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    regiao(Ns1, Ns2, Ns3).


main :- comparadoresPorCelula(Comparadores),
    vergleich(P, Comparadores),
    maplist(label, P), maplist(portray_clause, P),
    halt.