# Lexical Analyzer
Receives a stream of characters and attemps to give meaning to this stream.
Characters are grouped into tokens.
A token has a category and an attribute, ie it's 2-tuple `(category, attribute)`
The attribute can be a complex structure (Haskells data types seem to be a good fit).

The lexical analyzer is most useful when the tokens are composed of multiple chracters which must be treated as a unit.
In the case of my simple regex notation, I might be able to skip lexical analysis and go straight to parsing and bulding the syntax tree.

