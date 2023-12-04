:-  use_module(library(clpfd)), initialization(main).


comparadoresPorCelula(C) :-
    C = [
    [['.', '>', '>', '.'], ['.', '>', '<', '<'], ['.', '.', '<', '<'], ['.', '<', '>', '.'], ['.', '>', '>', '>'], ['.', '.', '>', '<'], ['.', '<', '<', '.'], ['.', '>', '>', '>'], ['.', '.', '<', '<']],
    [['<', '<', '<', '.'], ['>', '<', '>', '>'], ['>', '.', '>', '>'], ['<', '<', '<', '.'], ['<', '>', '<', '>'], ['<', '.', '<', '<'], ['>', '>', '>', '.'], ['<', '<', '>', '<'], ['>', '.', '>', '>']],
    [['>', '>', '.', '.'], ['<', '>', '.', '<'], ['<', '.', '.', '<'], ['>', '>', '.', '.'], ['>', '<', '.', '<'], ['>', '.', '.', '>'], ['<', '>', '.', '.'], ['<', '<', '.', '<'], ['<', '.', '.', '>']],
    [['.', '<', '<', '.'], ['.', '<', '<', '>'], ['.', '.', '<', '>'], ['.', '>', '>', '.'], ['.', '<', '>', '<'], ['.', '.', '>', '>'], ['.', '>', '>', '.'], ['.', '>', '<', '<'], ['.', '.', '>', '<']],
    [['>', '<', '>', '.'], ['>', '<', '>', '>'], ['>', '.', '>', '>'], ['<', '<', '>', '.'], ['<', '<', '>', '>'], ['<', '.', '>', '>'], ['<', '<', '<', '.'], ['>', '>', '<', '>'], ['<', '.', '<', '<']],
    [['<', '<', '.', '.'], ['<', '>', '.', '>'], ['<', '.', '.', '<'], ['<', '<', '.', '.'], ['<', '<', '.', '>'], ['<', '.', '.', '>'], ['>', '<', '.', '.'], ['>', '>', '.', '>'], ['>', '.', '.', '<']],
    [['.', '>', '>', '.'], ['.', '<', '>', '<'], ['.', '.', '>', '>'], ['.', '>', '>', '.'], ['.', '<', '<', '<'], ['.', '.', '<', '>'], ['.', '>', '>', '.'], ['.', '>', '>', '<'], ['.', '.', '<', '<']],
    [['<', '>', '<', '.'], ['<', '<', '<', '<'], ['<', '.', '<', '>'], ['<', '<', '>', '.'], ['>', '<', '>', '>'], ['>', '.', '>', '>'], ['<', '>', '>', '.'], ['<', '<', '>', '<'], ['>', '.', '<', '>']],
    [['>', '<', '.', '.'], ['>', '>', '.', '>'], ['>', '.', '.', '<'], ['<', '>', '.', '.'], ['<', '>', '.', '<'], ['<', '.', '.', '<'], ['<', '>', '.', '.'], ['<', '<', '.', '<'], ['>', '.', '.', '>']]
    ].

%ACHAR O I-ÉSIMO DA LISTA
%O 0-ésimo item da lista é a cabeça da lista, se ela tiver um ou mais itens.
%O I-ésimo item da lista é o (I-1)-ésimo item da cauda da lista.
%Armazena a resposta em H/X
nthLista(0, [H|_], H).
nthLista(Linha, [_|T], X) :- Linha2 is Linha - 1, nthLista(Linha2, T, X).

%ACHAR O I-ÉSIMO DA MATRIZ
%Primeiro encontra a lista X em Matriz
%Depois encontra o valor Y na lista
%Armazena a resposta em Z
nthMatriz(Linha, Coluna, Lista, X) :- nthLista(Linha, Lista, R), nthLista(Coluna, R, X).


%COMPARACOES ENTRE CELULAS
%Se o comparador for '.' não faz nada
compare_sign(_, _, _, _, '.').
compare_sign(Num1, Board, Linha, Coluna, '>') :- nthMatriz(Linha, Coluna, Board, Num2), Num1 #> Num2.
compare_sign(Num1, Board, Linha, Coluna, '<') :- nthMatriz(Linha, Coluna, Board, Num2), Num1 #< Num2.

% Checa se todos os vizinhos estão ok com os sinais
% sign_valid(Tabuleiro, Linha, Coluna)

%Verifica se o valor é válido para a célula na linha X, coluna Y
is_placement_valid(Num, Linha, Coluna, Board, Comparacoes) :- 
    %Comparadores da célula na linha X, coluna Y
    %Busca na matriz de comparadores (Comparadores)
    %Salva os comparadores em ComparadoresCelula
    nthMatriz(Linha, Coluna, Comparacoes, ComparadoresPorCelula),

    %Divide os comparadores por direção
    %Salva em Comparador[Direção]
    nthLista(0, ComparadoresPorCelula, ComparaAcima),
    nthLista(1, ComparadoresPorCelula, ComparaDireita),
    nthLista(2, ComparadoresPorCelula, ComparaAbaixo),
    nthLista(3, ComparadoresPorCelula, ComparaEsquerda),

    %Direcoes
    Left is Coluna - 1,
    Top is Linha - 1,
    Right is Coluna + 1,
    Bottom is Linha + 1,

    %Define as comparações por direção
    compare_sign(Num, Board, Top, Coluna, ComparaAcima),
    compare_sign(Num, Board, Linha, Right, ComparaDireita),
    compare_sign(Num, Board, Bottom, Coluna, ComparaAbaixo),
    compare_sign(Num, Board, Linha, Left, ComparaEsquerda).


%Percorre todas posições da matriz e define as comparações referentes a cada posição.
solve(_, _, 9, _).
solve(Board, Sign_board, Linha, Coluna) :- 
    nthMatriz(Linha, Coluna, Board, Num),
    is_placement_valid(Num, Linha, Coluna, Board, Sign_board),
    N is Coluna + 1,
    solve(Board, Sign_board, Linha, N).
%Caso esteja na última coluna
solve(Board, Sign_board, Linha, 9) :- N is Linha + 1, solve(Board, Sign_board, N, 0).

%Regras do Jogo
vergleich(Board, Sign_board, Size) :-
    %9 linhas
    length(Board, Size),

    %9 colunas
    maplist(same_length(Board), Board),

    %Concatena todos os números do tabuleiro em NumerosTotal
    append(Board, TodosOsNumeros),

    %O tabuleiro tem apenas números entre 1 e 9
    %ins(Vars, Domain): Vars-Lista de variáveis; Domain:Valores que Vars pode assumir
    TodosOsNumeros ins 1..Size, % O tabuleiro tem apenas números entre 1 e 9

    %O tabuleiro segue a regra do Tabuleiro vergleich
    solve(Board, Sign_board, 0, 0), % O tabuleiro segue a regra do Tabuleiro vergleichs

    %Sem repetição de valores por linha
    maplist(all_distinct, Board), % Todas as linhas possuem números entre 1 e 9 sem repetições

    %Inverte as linhas e colunas
    transpose(Board, Columns), % Inverte as linhas e colunas

    %Sem repetição de valores por coluna (agora linha)
    maplist(all_distinct, Columns),  % Todas as colunas possuem números entre 1 e 9 sem repetições

    %A partir das linhas nomeadas, divide em 3 regioes de 3 linhas cada
    %Sem repetição de valores por regiao
    Board = [Linha1, Linha2, Linha3, Linha4, Linha5, Linha6, Linha7, Linha8, Linha9], % Nomeia todas as linhas da Matriz

    % region_size_x is integer(sqrt(size)),
    % region_size_y is size // region_size_x,

    %A partir das linhas nomeadas, divide em 3 regioes de 3 linhas cada
    %Sem repetição de valores por regiao
    % regiao(Linha1, Linha2, Linha3, region_size_x),
    % regiao(Linha4, Linha5, Linha6, region_size_x),
    % regiao(Linha7, Linha8, Linha9, region_size_x),
    regiao(Linha1, Linha2, Linha3),
    regiao(Linha4, Linha5, Linha6),
    regiao(Linha7, Linha8, Linha9).


%Recebe 3 linhas, cada uma com tamanhos iguais entre si que podem ser 9, 6, 3 ou 0
%Vai andando em blocos de 3 por essas linhas até chegar ao final.
%Pega os 3 primeiros itens de cada linha e diz que todos os 9 itens precisam ser diferentes.
%Ao final repete o processo para a calda ou o que sobrou da lista.
regiao([], [], []).
regiao([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    regiao(Ns1, Ns2, Ns3).

% regiao([], [], [], _).
% regiao([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3], Linhas) :-
%     length([N1,N2,N3,N4,N5,N6,N7,N8,N9], Comprimento),
%     all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
%     LinhasRestantes is Linhas - 1,
%     regiao(Ns1, Ns2, Ns3, LinhasRestantes),
%     Comprimento =:= size.

size(9).

main :- comparadoresPorCelula(Sign_board),
    size(X),    
    vergleich(P, Sign_board, X),
    maplist(label, P), maplist(portray_clause, P),
    halt.