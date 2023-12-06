:-  use_module(library(clpfd)), initialization(main).

%Comparadores entre células
%Acima | Direita | Abaixo | Esquerda
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

%Tamanho do tabuleiro
size(9).

%Regras do Jogo
rules(Board, Sign_board, Size) :-
    %Define o número de linhas de Board
    length(Board, Size),

    %Define o número de colunas de Board
    maplist(same_length(Board), Board),

    %Transforma a matriz em uma lista unidimensional para facilitar a aplicação de restrições a todas as células da matriz de uma vez
    flatten(Board, TodosOsNumeros),
    %ins(Vars, Domain): Vars:Lista de variáveis; Domain:Valores que Vars pode assumir
    TodosOsNumeros ins 1..Size,

    %Regras do vergleich
    solve(Board, Sign_board, 0, 0),

    %Sem repetição de valores por linha
    maplist(all_distinct, Board),

    %Inverte as linhas e colunas
    transpose(Board, Columns),

    %Sem repetição de valores por coluna (agora linha)
    maplist(all_distinct, Columns),

    %A partir das linhas nomeadas, divide em 3 regioes de 3 linhas cada
    %Sem repetição de valores por regiao
    Board = [Linha1, Linha2, Linha3, Linha4, Linha5, Linha6, Linha7, Linha8, Linha9],

    %A partir das linhas nomeadas, divide em 3 regioes de 3 linhas cada
    %Sem repetição de valores por regiao
    regiao(Linha1, Linha2, Linha3),
    regiao(Linha4, Linha5, Linha6),
    regiao(Linha7, Linha8, Linha9).


/*
ACHAR O I-ÉSIMO DA LISTA
O 0-ésimo item da lista é a cabeça da lista, se ela tiver um ou mais itens.
O I-ésimo item da lista é o (I-1)-ésimo item da cauda da lista.
Armazena a resposta em H/X
*/
nthLista(0, [H|_], H).
nthLista(Linha, [_|T], X) :- Linha2 is Linha - 1, nthLista(Linha2, T, X).

/*
ACHAR O I-ÉSIMO DA MATRIZ
Primeiro encontra a lista X em Matriz
Depois encontra o valor Y na lista
Armazena a resposta em Z
*/
nthMatriz(Linha, Coluna, Lista, X) :- nthLista(Linha, Lista, R), nthLista(Coluna, R, X).


/*
COMPARACOES ENTRE CELULAS
Se o comparador for '.' não faz nada
*/
compare_sign(_, _, _, _, '.').
compare_sign(Num1, Board, Linha, Coluna, '>') :- nthMatriz(Linha, Coluna, Board, Num2), Num1 #> Num2.
compare_sign(Num1, Board, Linha, Coluna, '<') :- nthMatriz(Linha, Coluna, Board, Num2), Num1 #< Num2.


%Verifica se o valor é válido para a célula na linha X, coluna Y
is_placement_valid(Num, Linha, Coluna, Board, Comparacoes) :- 
    /*
    Comparadores da célula na linha X, coluna Y
    Busca na matriz de comparadores (Comparadores)
    Salva os comparadores em ComparadoresCelula
    */
    nthMatriz(Linha, Coluna, Comparacoes, ComparadoresPorCelula),

    /*
    Divide os comparadores por direção
    Salva em Comparador[Direção]
    */
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


%Resolvedor
solve(_, _, 9, _).
solve(Board, Sign_board, Linha, Coluna) :- 
    nthMatriz(Linha, Coluna, Board, Num),
    is_placement_valid(Num, Linha, Coluna, Board, Sign_board),
    N is Coluna + 1,
    solve(Board, Sign_board, Linha, N).
%Caso esteja na última coluna
solve(Board, Sign_board, Linha, 9) :- N is Linha + 1, solve(Board, Sign_board, N, 0).

/*
GARANTE QUE VALORES NÃO SE REPITAM POR REGIÃO

[Valor1,Valor2,Valor3|Resto1]: Primeira linha do bloco
[Valor4,Valor5,Valor6|Resto2]: Segunda linha do bloco
[Valor7,Valor8,Valor9|Resto3]: Terceira linha do bloco
Compara os 3 primeiros valores das 3 primeiras linhas,
depois chama novamente com os 6 últimos valores de cada linha
e assim sucessivamente
*/
regiao([], [], []).
regiao([Valor1,Valor2,Valor3|Resto1], [Valor4,Valor5,Valor6|Resto2], [Valor7,Valor8,Valor9|Resto3]) :-
    all_distinct([Valor1,Valor2,Valor3,Valor4,Valor5,Valor6,Valor7,Valor8,Valor9]),
    regiao(Resto1, Resto2, Resto3).

main :- comparadoresPorCelula(Sign_board),
    size(X),    
    %Resolve o Jogo
    rules(Table, Sign_board, X),
    %Rotula todos os elementos de Table
    maplist(label, Table),
    %Formata Table
    maplist(portray_clause, Table),
    halt.
