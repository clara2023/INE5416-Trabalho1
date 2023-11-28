:-  use_module(library(clpfd)), initialization(main).

%Acima | Direita | Abaixo | Esquerda
%Menor: -
%Maior: +
%Sem comparador: /
comparadoresPorCelula(Comparadores) :-
    Comparadores = [
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
nthLista(I, [_|T], X) :- I2 is I - 1, nthLista(I2, T, X).


%ACHAR O I-ÉSIMO DA MATRIZ
%Primeiro encontra a lista X em Matriz
%Depois encontra o valor Y na lista
%Armazena a resposta em Z
nthMatriz(X, Y, Matriz, Z) :- nthLista(X, Matriz, W), nthLista(Y, W, Z).


%COMPARACOES ENTRE CELULAS
%Se o comparador for '/' não faz nada
compara(_, _, _, _, '/').
compara(Numero1, Tabuleiro, X, Y, '+') :- nthMatriz(X, Y, Tabuleiro, Numero2), Numero1 > Numero2.
compara(Numero1, Tabuleiro, X, Y, '-') :- nthMatriz(X, Y, Tabuleiro, Numero2), Numero1 < Numero2.


%Verifica se o valor é válido para a célula na linha X, coluna Y
valido(Numero, X, Y, Tabuleiro, Comparadores) :- 
    %Comparadores da célula na linha X, coluna Y
    %Busca na matriz de comparadores (Comparadores)
    %Salva os comparadores em ComparadoresCelula
    nthMatriz(X, Y, Comparadores, ComparadoresCelula),

    %Divide os comparadores por direção
    %Salva em Comparador[Direção]
    nthLista(0, ComparadoresCelula, ComparadorAcima),
    nthLista(1, ComparadoresCelula, ComparadorDireita),
    nthLista(2, ComparadoresCelula, ComparadorAbaixo),
    nthLista(3, ComparadoresCelula, ComparadorEsquerda),

    %Define as comparações por direção
    compara(Numero, Tabuleiro, X-1, Y, ComparadorAcima),
    compara(Numero, Tabuleiro, X, Y+1, ComparadorDireita),
    compara(Numero, Tabuleiro, X+1, Y, ComparadorAbaixo),
    compara(Numero, Tabuleiro, X, Y-1, ComparadorEsquerda).


%Percorre todas posições da matriz e define as comparações referentes a cada posição.
regras(_, _, 9, _).
regras(Tabuleiro, Comparacoes, X, Y) :- 
    nthMatriz(X, Y, Tabuleiro, Numero),
    valido(Numero, X, Y, Tabuleiro, Comparacoes),
    regras(Tabuleiro, Comparacoes, X, Y + 1).
%Caso esteja na última coluna
regras(Tabuleiro, Comparacoes, X, 9) :- regras(Tabuleiro, Comparacoes, X + 1, 0).

%Regras do Jogo
vergleich(Tabuleiro, Comparacoes) :-
    %9 linhas
    length(Tabuleiro, 9),

    %9 colunas
    maplist(same_length(Tabuleiro), Tabuleiro),

    %Concatena todos os números do tabuleiro em NumerosTotal
    append(Tabuleiro, NumerosTotal),

    %O tabuleiro tem apenas números entre 1 e 9
    %ins(Vars, Domain): Vars-Lista de variáveis; Domain:Valores que Vars pode assumir
    NumerosTotal ins 1..9,

    %O tabuleiro segue a regra do Tabuleiro vergleich
    regras(Tabuleiro, Comparacoes, 0, 0),

    %Sem repetição de valores por linha
    maplist(all_distinct, Tabuleiro),

    %Inverte as linhas e colunas
    transpose(Tabuleiro, Columns),

    %Sem repetição de valores por coluna (agora linha)
    maplist(all_distinct, Columns), 

    %Nomeia todas as linhas da Matriz
    Tabuleiro = [Linha1, Linha2, Linha3, Linha4, Linha5, Linha6, Linha7, Linha8, Linha9],


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


main :- comparadoresPorCelula(Comparacoes),
    vergleich(P, Comparacoes),
    maplist(label, P), maplist(portray_clause, P),
    halt.