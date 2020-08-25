#Introdução
Uma expressão regular é informada ao computador através de uma sequencia de caracteres, um String, que deve ser convertido para uma automata, o módulo de parse é responsável pelo primeiro passo dessa conversão.
Esse módulo recebe uma regex em forma de string e a converte para uma representação intermediaria, uma árvore.

O termo Parse vem da construção de compiladores, onde o parsing é a segunda etapa do processo de compilação, tambem chamada de analize sintática \cite{dragon}.
Na etapa de parse, um stream de *tokens*, um tipo de dado que categoriza um grupo de caracteres, gerado pela análise léxica, é convertido em um representaçã̀o intermediaria, normalmente uma árvore \cite{dragon}.
Sendo assim, o módulo de parse transforma o string de entrada em uma lista de tokens e partir deles gera uma árvore, essa ávore é utilizada em etapas seguintes e convertida em uma automata.

# Design
## Tokens
O processo de parsing tem como caracteristica importante transformar um *stream* de caracteres em um tipo de dado estruturado, algo com significado computacional.
Para tanto, é necessário modelar os elementos das expressões regulares em tipos de dados, ou seja, definir os *tokens* da regex.

Foram definidos 5 tokens para a regex: tokens de simbolos, tokens de operadores e tokens de quantificadores, inicio de grupo e fim de grupo.
Um token de simbolo representa um caractere literal em uma regex, como exemplo a regex "a"; os simbolos validos são as letras minusculas de "a" até "z".
Um token de operador representa um operador, que une partes da regex; as operações validas são concatenação e alternação.
O token de quantifição representa uma operação de quantificação, normalmente idicados por "?", "\*" ou "+"; no momento a unica quantificação aceita é a Kleene Star. 
Os tokens que indicam agrupamento são diferentes pois não possuem nenhum valor associado que varia, o token por si só é o suficiente,

A modelagem do token foi feita usando os tipos de dados algébricos presentes em Haskell.
O trecho de código a baixo contém a definição dos tipos de dado que definem um token.
A palavra-chave `type` define um alias para um tipo, o tipo `Symbol` é equivalente a `Char`.
O tipo `Operator` indica um operador e como foi dito anteriormente pode indicar concatenação (`Concat`) ou alternação (`Alternation`).
`Quantifier` indica operadores de repetição, por fim de simplicidade, somente o operador de Kleene foi implmentado.
Finalmente, o tipo `Token` possui 5 construtores, um construtor para cada tipo de token.

```haskell
type Symbol = Char
data Operator = Concat | Alternation deriving (Show, Eq)
data Quantifier = Kleene deriving (Show, Eq)
data Token = SToken Symbol | QtToken Quantifier | OpToken Operator | GroupBegin | GroupEnd deriving (Show, Eq)
```

Isso conclui o design dos tokens que é usado no módulo de parsing.
A definição de sub-tipos na definição do token é muito util pois permite definir a estrutura da árvore de maneira mas granular e a mesma será definida a seguir.

## Árvore
A definição de uma árvore para estruturar uma regex é util pois, visto que existe uma hierarquia em uma árvore, é possível indicar prioridade e a ordem das operações.
Isso significa que o significado da regex é preservado e devido a quantidade de algoritimos existentes para operar sobre estruturas em árvore, percorrer a árvore para transforma-la em uma automata passa a ser trivial.

Novamente, foi feito o uso de tipos de dados algebricos para definir uma árvore, porém dessa vez o caso de uso é mais interessante.
A definição de uma árvore em uma linguagem que tem suporte a esse tipo de dado é feita de uma maneira recursiva.

Foi definido que uma árvore pode ter 3 formatos: uma folha que armazena um símbolo, um nó que conecta duas árvore e um nó de quantifição que quantifica uma árvore.
O trecho de código abaixo contém a definição da árvore, note que os 3 casos mencionados estão descritos e note também que a definição de `ParseTree` faz uso de `ParseTree`, ou seja, o tipo é definido de maneira recursiva.
O tipo `ParseTree` não faz uso do tipo `Token`, mas sim, dos subtipos `Operator`, `Symbol` e `Quantifier`, isso permite especificar com exatidão o tipo de token.

```
data ParseTree = Node ParseTree Operator ParseTree | QuantifierLeaf ParseTree Quantifier | Leaf Symbol deriving (Show, Eq)
```

Em conclusão, a árvore construída é a saída do módulo de parse, a partir dela será construída a automata equivalente a essa regex.
Usar uma árvore como um formato intermediário é uma prática comum, sendo muito usada na construção de compiladores, segundo \cite{dragon}.
Além disso, a árvore, por ser hierarquica, é uma estrutura capaz de preservar a ordem de operações, respeitando a maneira na qual a regex foi construída.

## Subexpressão
Uma funcionalidade essencial para compor expressões regulares complexas é a possibilidade de criar grupos de buscas.
Esses grupos de buscas são, expressões regulares normais, que por sua vez tambem podem conter sub expressões.

As subexpressões são importantes pois elas permitem quantificar um grupo de caracteres, ao invez de um unico caractere.
Assim, podemos concluir que uma sub-expressão pode ser quantificada, ou não.
Para facilitar a implementação do código, foi definido um tipo `SubExpression`, com dois construtores, para expressões quantificadas e não quantificadas.
Os dois construtores definidos armazenam uma lista de `Token` e o construtor quantificado armazena o operador de quantificação.
O trecho de código abaixo define o tipo de dado `SubExpression`.

```
data SubExpression = SubExp [Token] | QuantifiedSubExp [Token] Quantifier deriving (Show, Eq)
```

A partir dos tipos definidos nessa seção será definido o conjunto de funções que serão utilizadas para conveter o String de entrada na árvore de parse.
Utilizando as particularidades dos tipos de dados algébricos, foi possível definir a estrutura do parser de maneira enxuta, com poucos tipos de dados.
A flexibilidade dos tipos algébricos poupa o trabalho de definir classes abstratas, classes herdeiras e implementar métodos particulares para cada classe definida.
Dependendo da complexida do código, seria necessário definir, também, interfaces para fins de polimorfismo.
A programação orientada a objetos é capaz de definir uma estrutura analoga à que foi definida, mas sua implementação seria muito mais verbosa e complexa.
Embora a estrutura definida seja relativamente simples, a partir dela pode-se ter uma ídeia da diferenças no processo de modelagem em uma linguagem com tipos de dados álgebricos, e seguindo o paradigma de objetos.

# Implementação
Lembrando que, por se tratar do paradigma funcional, a interface publica do módulo será uma funcão que converte um string em uma árvore de parse.
A implementação dessa funcão é feita através de uma cadeia funcões.
A secão a seguir irá explicar a implementacão do módulo de parse de maneira geral.
Serão explicados os passos pelo qual o string passa até virar a árvore final, porém não será explicado o código todo, somente pontos interessantes e importantes.
O código completo está contido no apendice 1 e no github do autor, [http://github.com/lodek/regex-engine].

## Tokenizacão
O primeiro passo da cadeia de transformacões é transformar um String, lista de caracteres, em uma lista de tokens.
Isso pode ser feito de maneira simples, visto que para a linguagem definida, cada caractere tem um mapeamento direto para um token.
O código abaixo recebe um caractere e retorna um token.
Primeiro, o caractere é testado, se o caractere for um caractere de controle, ou seja, operador, agrupamento e quantificacão, um subfuncão é chamada que analisa caso a caso e retorna o token correto.
Caso o teste anterior falhe, verifica-se se o caractere é um simbolo, caso sim é retornado um token de simbolo.
Por ultimo, caso ambos os testes falhem, é retornado um erro.

```
genToken :: Symbol -> Token
genToken c
    | c `elem` controlCharacters = controlToken c
    | c `elem` alphabet = SToken c
    | otherwise = error $ "Symbol " ++ [c] ++ "is not in alphabet."
    where controlCharacters = "()|*"
          alphabet = ['a'..'z']
          controlToken c = case c of '(' -> GroupBegin 
                                     ')' -> GroupEnd
                                     '|' -> OpToken Alternation
                                     '*' -> QtToken Kleene
```

## Validacão
Apos tokenizar os caracteres a regex é validada.
A validacão é feita usando um conjunto de funcões, chamados de predicados, que recebem uma lista de tokens e retorna um valor booleano, verdadeiro caso a expreswsão seja valida, falso caso contrario.
Foram definidos dois predicados, um que valida que os grupos na regex estão balanceados e outro que valida que não exites dois tokens de quantificacão seguidos.
A implementacão desses predicados não é de muito interesse, porém a funcão que aplica os predicados é.
O trecho a seguir implementa a funcão de validacão.

```
validateTokens :: [Token] -> Bool
validateTokens ts = and ( predicates <*> pure ts) -- Applicative functors
   where predicates = [evenGroupPredicate, uniqueQuantifierPredicate]
```

Essa funcão faz uso de uma das funcionalidades mais avançadas do Haskell, *applicatives*.
*Applicative* é uma funcionalidade complexa, com varias implicacões na maneira de escrever código em haskell, explicar a fundo sobre applicative foge do escopo desse texto.
Porém basta saber que no código acima, foi usado o operador `<*>`, o operador dos *applicatives*, para aplicar uma lista de funcões sobre a lista de tokens da regex.
Os *applicatives* são de várias maneiras em Haskell e eles permitem realizar operacões complexas de emaneira enxuta, uma funcionalidade muito boa do Haskell.

A etapa de validacão garante que a regex definida pelo usuário é bem formada.
Isso é fundamental pois certas funções assumem que a regex está estruturada de maneira correta, isso é assumido como uma invariante no design de algumas funcões.


## Normalizacão
Apos validados, os tokens são "normalizados".
O processo de normalizacão é justificado pela notacão da regex, a regex "ab", realmente significa: simbolo "a", concatenacão, simbolo "b"; a concatenacão é uma operacão implicita em regexes, tal como a multiplicacão é implicita na álgebra.
A normalizacão torna esses operadores explicitos, pois eles são usados na construcão da árvore de parse.

Finalmente, a lista de tokens está pronta para ser convertida em um árvore.

## Conversão para árvore


## Subgrupos

## Conclusao

