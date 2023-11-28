:-  use_module(library(clpfd)), initialization(main).


comparadoresPorCelula(C) :-
    C = [
    [['/', '+', '+', '/'], ['/', '+', '-', '-'], ['/', '/', '-', '-'], ['/', '-', '+', '/'], ['/', '+', '+', '+'], ['/', '/', '+', '-'], ['/', '-', '-', '/'], ['/', '+', '+', '+'], ['/', '/', '-', '-']],
    [['-', '-', '-', '/'], ['+', '-', '+', '+'], ['+', '/', '+', '+'], ['-', '-', '-', '/'], ['-', '+', '-', '+'], ['-', '/', '-', '-'], ['+', '+', '+', '/'], ['-', '-', '+', '-'], ['+', '/', '+', '+']],
    [['+', '+', '/', '/'], ['-', '+', '/', '-'], ['-', '/', '/', '-'], ['+', '+', '/', '/'], ['+', '-', '/', '-'], ['+', '/', '/', '+'], ['-', '+', '/', '/'], ['-', '-', '/', '-'], ['-', '/', '/', '+']],
    [['/', '-', '-', '/'], ['/', '-', '-', '+'], ['/', '/', '-', '+'], ['/', '+', '+', '/'], ['/', '-', '+', '-'], ['/', '/', '+', '+'], ['/', '+', '+', '/'], ['/', '+', '-', '-'], ['/', '/', '+', '-']],
    [['+', '-', '+', '/'], ['+', '-', '+', '+'], ['+', '/', '+', '+'], ['-', '-', '+', '/'], ['-', '-', '+', '+'], ['-', '/', '+', '+'], ['-', '-', '-', '/'], ['+', '+', '-', '+'], ['-', '/', '-', '-']],
    [['-', '-', '/', '/'], ['-', '+', '/', '+'], ['-', '/', '/', '-'], ['-', '-', '/', '/'], ['-', '-', '/', '+'], ['-', '/', '/', '+'], ['+', '-', '/', '/'], ['+', '+', '/', '+'], ['+', '/', '/', '-']],
    [['/', '+', '+', '/'], ['/', '-', '+', '-'], ['/', '/', '+', '+'], ['/', '+', '+', '/'], ['/', '-', '-', '-'], ['/', '/', '-', '+'], ['/', '+', '+', '/'], ['/', '+', '+', '-'], ['/', '/', '-', '-']],
    [['-', '+', '-', '/'], ['-', '-', '-', '-'], ['-', '/', '-', '+'], ['-', '-', '+', '/'], ['+', '-', '+', '+'], ['+', '/', '+', '+'], ['-', '+', '+', '/'], ['-', '-', '+', '-'], ['+', '/', '-', '+']],
    [['+', '-', '/', '/'], ['+', '+', '/', '+'], ['+', '/', '/', '-'], ['-', '+', '/', '/'], ['-', '+', '/', '-'], ['-', '/', '/', '-'], ['-', '+', '/', '/'], ['-', '-', '/', '-'], ['+', '/', '/', '+']]
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
%Se o comparador for '/' não faz nada
compara(_, _, _, _, '/').
compara(Num1, Tabuleiro, Linha, Coluna, '+') :- nthMatriz(Linha, Coluna, Tabuleiro, Num2), Num1 #> Num2.
compara(Num1, Tabuleiro, Linha, Coluna, '-') :- nthMatriz(Linha, Coluna, Tabuleiro, Num2), Num1 #< Num2.


%Verifica se o valor é válido para a célula na linha X, coluna Y
define_numero_valido(Num, Linha, Coluna, Tabuleiro, Comparacoes) :- 
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
    compara(Num, Tabuleiro, Top, Coluna, ComparaAcima),
    compara(Num, Tabuleiro, Linha, Right, ComparaDireita),
    compara(Num, Tabuleiro, Bottom, Coluna, ComparaAbaixo),
    compara(Num, Tabuleiro, Linha, Left, ComparaEsquerda).


%Percorre todas posições da matriz e define as comparações referentes a cada posição.
regras(_, _, 9, _).
regras(Tabuleiro, Comparadores, Linha, Coluna) :- 
    nthMatriz(Linha, Coluna, Tabuleiro, Num),
    define_numero_valido(Num, Linha, Coluna, Tabuleiro, Comparadores),
    N is Coluna + 1,
    regras(Tabuleiro, Comparadores, Linha, N).
%Caso esteja na última coluna
regras(Tabuleiro, Comparadores, Linha, 9) :- N is Linha + 1, regras(Tabuleiro, Comparadores, N, 0).


%Regras do Jogo
vergleich(Tabuleiro, Comparadores) :-
    %9 linhas
    length(Tabuleiro, 9),

    %9 colunas
    maplist(same_length(Tabuleiro), Tabuleiro),

    %Concatena todos os números do tabuleiro em NumerosTotal
    append(Tabuleiro, TodosOsNumeros),

    %O tabuleiro tem apenas números entre 1 e 9
    %ins(Vars, Domain): Vars-Lista de variáveis; Domain:Valores que Vars pode assumir
    TodosOsNumeros ins 1..9, % O tabuleiro tem apenas números entre 1 e 9

    %O tabuleiro segue a regra do Tabuleiro vergleich
    regras(Tabuleiro, Comparadores, 0, 0), % O tabuleiro segue a regra do Tabuleiro vergleichs

    %Sem repetição de valores por linha
    maplist(all_distinct, Tabuleiro), % Todas as linhas possuem números entre 1 e 9 sem repetições

    %Inverte as linhas e colunas
    transpose(Tabuleiro, Columns), % Inverte as linhas e colunas

    %Sem repetição de valores por coluna (agora linha)
    maplist(all_distinct, Columns),  % Todas as colunas possuem números entre 1 e 9 sem repetições

    %A partir das linhas nomeadas, divide em 3 regioes de 3 linhas cada
    %Sem repetição de valores por regiao
    Tabuleiro = [Linha1, Linha2, Linha3, Linha4, Linha5, Linha6, Linha7, Linha8, Linha9], % Nomeia todas as linhas da Matriz

    %A partir das linhas nomeadas, divide em 3 regioes de 3 linhas cada
    %Sem repetição de valores por regiao
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


main :- comparadoresPorCelula(Comparadores),
    vergleich(P, Comparadores),
    maplist(label, P), maplist(portray_clause, P),
    halt.