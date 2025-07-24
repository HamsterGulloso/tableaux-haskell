# tableaux-haskell

## Como executar:

### Pré-requisitos:
* Ambiente unix
* ghc

### Download e compilação:

    git clone https://github.com/HamsterGulloso/tableaux-haskell
    ./build.sh
    ./tableaux-haskell

## Casos Exemplo:

### Exemplo 1:
$p \to (p \to (q \to p))$

    > p p q p -> -> ->

    Formula inserida: p→(p→(q→p))

    [1 ⊤:p→(p→(q→p))]
        [2.1 ⊥:p]
        [3.1 ⊤:p→(q→p)]
            [3.2.1 ⊥:p]
            [3.3.1 ⊤:q→p]
                [3.3.2.1 ⊥:q]
                [3.3.3.1 ⊤:p]
    OK

    [1 ⊥:p→(p→(q→p))] → [2 ⊤:p] → [3 ⊥:p→(q→p)] → [4 ⊤:p] → [5 ⊥:q→p] → [6 ⊤:q] → [7 ⊥:p]
    Contradições: 7 e 4

    A formula é tauntológica

### Exemplo 2:
$(a\to b)\land(b\to a)$

    > a b -> b a -> &

    Formula inserida: (a→b)∧(b→a)

    [1 ⊤:(a→b)∧(b→a)] → [2 ⊤:a→b]
        [3 ⊤:b→a]
            [4.1 ⊥:a] → [4.2.1 ⊥:b]
            [4.1 ⊥:a] → [4.3.1 ⊤:a]
        [3 ⊤:b→a]
            [5.1 ⊤:b] → [5.2.1 ⊥:b]
            [5.1 ⊤:b] → [5.3.1 ⊤:a]
    OK

    [1 ⊥:(a→b)∧(b→a)]
        [2.1 ⊥:a→b] → [2.2 ⊤:a] → [2.3 ⊥:b]
        [3.1 ⊥:b→a] → [3.2 ⊤:b] → [3.3 ⊥:a]
    OK

    A formula é satisfazivel

### Exemplo 3:
$(a\to b) \land a \land \lnot b$

    > a b -> a & b ! &

    Formula inserida: (a→b)∧a∧¬b

    [1 ⊤:(a→b)∧a∧¬b] → [2 ⊤:(a→b)∧a] → [3 ⊤:¬b] → [4 ⊤:a→b]
        [5 ⊤:a] → [6 ⊥:b] → [7.1 ⊥:a]
        [5 ⊤:a] → [6 ⊥:b] → [8.1 ⊤:b]
    Contradições: 8.1 e 6, 7.1 e 5

    [1 ⊥:(a→b)∧a∧¬b]
        [2.1 ⊥:(a→b)∧a]
            [2.2.1 ⊥:a→b] → [2.2.2 ⊤:a] → [2.2.3 ⊥:b]
            [2.3.1 ⊥:a]
        [3.1 ⊥:¬b] → [3.2 ⊤:b]
    OK

    A formula é contraditória

## Melhorias no retorno das entradas
Foram adicionados algumas mensagens de erro para o usuário

### Expressão ruim:
    > a b c ! |

    Expressão ruim
    Expressão formada: b∨¬c
    Tokens não utilizados: [a]

### Expressão vazia:
    >

    Expressão vazia
