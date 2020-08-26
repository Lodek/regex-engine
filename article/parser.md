# Introdução
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

```haskell
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

```haskell
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

```haskell
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

```haskell
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
Com os pré-processamentos feitos, a regex em forma de string foi convertida para uma lista de tokens, normalizada e validada.
O passo anterior é importante pois o algoritimo que converte a lista de tokens em uma árvore não implementa programacão preventiva, ele assume que os tokens estão bem estruturados e que condicões anormais tenham sido identificadas em etapas anteriores, caso contrário, existe a possibilidade do programar gerar um erro em tempo de execucão.
Essa secão irá abordar o tópico de transformar uma lista de tokens em uma árvore.
A fim de simplificar a explicacão, primeiramente será explicado como o algoritimo transforma uma expressão regular sem grupos em uma árvore e na próxima secão será adicionado suporte aos sub-grupos.

A transformacão de lista para árvore aconteçe em algumas etapas
1. A funcão `sortAndTreefy` itera sobre a lista de tokens. Se o token é um token de operacão, o mesmo é adicionado a lista de retorno; se o token é um token de símbolo, a funcão cria uma folha para aquele símbolo e a adiciona a lista de retorna; se o token é um token de quantificacão, a funcão cria uma folha quantificada.
2. A funcão `transformEithers` separa a lista de operadores e árvores em um par com uma lista de operadores e outra lista de árvores.
3. A funcão `mergeOps` une as árvores unitárias (folhas), usando os operadores da lista de operadores. A funcão recursivamente: pega as duas primeiros árvores da lista de árvores, o primeiro operador da lista de operador; cria um nó usando o construtor definido no tipo `ParseTree` com as árvores e o operador; continua a recursão usando o nó criado como sendo o primeiro elemento da lista de árvores. Esse processo de repete até que todos os operadores tenham sido utilizados. O retorno dessa funcão é uma lista unitária de árvores.
4. Finalmente, a árvore é removida da lista unitária e retornada.

A funcão mais interessante dessa pipeline é a funcão `sortAndTreefy`, o trecho de código abaixo contém a definicão da mesma.
Essa funcão não é particularmente complexa, ela faz uso de *pattern matching* para identificar a estrutura dos primeiros elementos da lista de tokens para decidir o que fazer.
Essa funcão é um exemplo interessante da diferença de pensamento entre programação imperativa e funcional.
Na programacão imperativa, pensa-se nos passos que serão executados para resolver o problema; na programacão funcional, pensa-se na estrutura e forma dos argumentos de entrada, a programacão funcional permite um pensamento mais alto nível sobre o problema. **(review this)**
Sendo assim, para implementar essa funcão, foi analizado os diferentes padrões que pudessem existir na lista de tokens, e a partir disso, foi definido qual acão a funcão deve executar para cada caso.
Note que, como em muitos casos, a funcão `sortAndTreefy` foi definiada de maneira recurssiva, visto que não é possível mutar os dados e que não existe for loops.

```haskell
sortAndTreefy :: [Token] -> [Either Operator ParseTree]
sortAndTreefy [] = []
sortAndTreefy ((SToken s):(QtToken q):ts) = (Right $ QuantifierLeaf (Leaf s) q):sortAndTreefy ts
sortAndTreefy (t:ts) = case t of (OpToken t) -> (Left t):(sortAndTreefy ts)
                                 (SToken t) -> (Right $ Leaf t):(sortAndTreefy ts)
                                 (GroupEnd) -> []
```

*Implement sortAndTreefy in Java or Python*

A etapa de transformar uma lista de tokens em uma árvore, embora tenha sido mais complexa que as etapas anteriores, ainda possui uma complexidade bem razoavel.
A maior complexidade desse processo é identificar o significado de diferentes combinacões de tokens e definir uma acão apropriada para cada caso.
Novamente, a programacão funcional é capaz de abstrair varios pequenos detalhes de como a computacão será exectuada, se estivermos dispostos a aceitar a mudança de paradigma.
Tendo definido o processo de conversão para um *regex* sem agrupamento, podemos usar o que foi criado para lidar com subgrupos.

## Subexpressão
Na secão anterior, foi introduzido como transformar uma regex sem sub-expressões, em uma árvore, essa secão expande o algoritimo anterior para aceitar sub-expressões.

Uma sub-expressão é literalmente uma regex em uma regex.
Uma regex pode ter uma sub-expressão, que por sua vez pode ter uma sub-expressão e etc.
Note a natureza recursiva de sub-expressões, isso indica que pode ser feito o uso de recursão para construir as sub-árvores e felizmente, linguagens funcionais são ótimas com recursão.

Como em toda recursão, é necessário um *base case* para que ela não seja infinita.
No caso de sub-expressões, o base case seria quando ela fosse uma regex simples, sem grupos.
Visto que, em algum ponto da recursão, teremos uma regex simples, a mesma pode ser convertida para uma arvore, que por sua vez pode ser unida a outra árvore usando os operadores definidos.
Sendo assim, pode-se concluir que, uma sub-expressão é equivalente a uma árvore, e no caso de sub-expressões recursivas, no final de tudo, é possível gerar uma única árvore que seja equivalente a ela.

Para adicionar a funcionalidade de sub-expressões ao código, basta notar que a construcão de uma sub-expressão é equivalente a construcão de uma expressão comum, visto que ambos retornam uma única arvore.
Sendo assim, grande parte do código ja escrito será reutilizado, sendo necessário uma funcão para extrair identificar e extrair uma subexpressão da lista de tokens.
Como uma sub-expressão é delimitada pelos tokens de `GroupBegin` e `GroupEnd`, a funcão de extracão deve retornar todos os tokens entre os tokens de agrupamento.
Para isso, foi escrito a funcão `yankSubExp`, que retorna um valor do tipo `SubExpression` com os tokens que fazem parte daquele grupo.
Essa funcão itera sobre a lista de tokens, iniciando no token de `GroupBegin`, e adiciona tokens a lista de retorno até que exista um numero igual de tokens do tipo `GroupBegin` e `GroupEnd`.
Essa condicão garante que subgrupos sejam retornados também. 

Com essa funcão extra, podemos expandir a funcão `sortAndTreefy` para que, quando a mesma encontre um token de `GroupBegin`, todos os tokens referentes a aquele grupo sejam extraídos, usando esses tokens seja gerada uma nova árvore e essa árvore seja adicionada a lista de retorno.
Essa implementacão, por ser recursiva, funciona para subsub-expressões também, visto que a subsub-expressão será identificada no processo de construcão da árvore da sub-expressão, e o processo irá se repetir.
O trecho de código abaixo contém a funcão `sortAndTreefy` modificada para aceitar grupos.
Note que foram adicionas apenas 2 linhas à funcão original, a equacão referente ao caso do token ser um token de agrupamento.


```haskell
sortAndTreefy :: [Token] -> [Either Operator ParseTree]
sortAndTreefy [] = []
sortAndTreefy ((SToken s):(QtToken q):ts) = (Right $ QuantifierLeaf (Leaf s) q):sortAndTreefy ts
sortAndTreefy ts'@(GroupBegin:ts) = let (sub, tokens) = yankSubExp ts' in
                                        (Right $ buildSubExp sub):(sortAndTreefy tokens)
sortAndTreefy (t:ts) = case t of (OpToken t) -> (Left t):(sortAndTreefy ts)
                                 (SToken t) -> (Right $ Leaf t):(sortAndTreefy ts)
                                 (GroupEnd) -> []
```

A funcão `sortAndTreefy` chama uma funcão `buildSubExp` que é nova, porem essa funcão nada mais é do que um atalho para chamar as funcões definidas anteriormente.
O trecho abaixo contém a definicão dela.

```haskell
buildSubExp :: SubExpression -> ParseTree
buildSubExp (SubExp ts) = (head . uncurry mergeOps . transformEithers . sortAndTreefy) ts
buildSubExp (QuantifiedSubExp ts qt) = QuantifierLeaf (buildSubExp $ SubExp ts) qt
```

Sendo assim, o módulo de parse está completo.

## Conclusao
O módulo de parse tranforma um string em uma árvore.
Para a construcão do módulo de parse primeiramente foi definido os tipos de dados que seriam usados por ele.
A definicão dos tipos de dados foi feito de maneira enxuta e clara, devido a natureza dos tipos de dados álgebricos que existe em Haskell.
A partir dos tipos, foram definidas funcões que realizam um conjunto de transformacões sobre o valor de entrada, um string, até gerar o valor de saída, uma árvore.
Em muitas dessas funcões foi feito uso de recursão, um padrão muito comum na programacão funcional.
Devido a natureza das linguagem funcionais, cada funcão realizava uma unica tarefa, e sendo assim, contém poucas linhas de código.
O resultado final do módulo é uma única funcão que faz uso do operador de composicão para transformar um string em uma árvore.

Nos exemplos contidos nesse texto, é notavél o uso de *pattern matching* como ferramenta fundamental do desenvolvimento, essa ferramenta permite o programador pensar sobre a estrutura do dado, ao invez dos passos que precisam ser feitos.
Isso é somente um exemplo de como a programacão funcional pode ser mais expressiva, possibilitando o programador a pensar de maneira mais abstrata e concisa.
