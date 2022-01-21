# RangeQueries

![Documentation Generation](https://github.com/vanyle/lrparser/actions/workflows/github-documentation.yml/badge.svg)
![Tests](https://github.com/vanyle/lrparser/actions/workflows/github-tests.yml/badge.svg)

This nim package implements an SLR parser, an algorithm that allows you to parse string to form trees.
Such a parser allows you to parse programming languages for example. For such an example, see `tests/vlang.nim`

More information about SLR parsers here:
https://en.wikipedia.org/wiki/LR_parser

## Usage

```nim
let input_string = """
        4 * 41 + 3 * (7 + 2)
    """
# The ruleset
let grammar = @[
    ("S", @["E"]),
    ("E", @["E", "+", "T"]),
    ("E", @["T"]),
    ("T", @["T", "*", "F"]),
    ("T", @["F"]),
    ("F", @["(", "E", ")"]),
    ("F", @["LIT"]),
]

# We need to convert the strings to ids so that the parser understands it
let tokenIdTable = makeTokenIdTable(grammar) # returns a Table[string, int]
let int_grammar = convertGrammar(grammar, tokenIdTable)

proc labeler(s: string): int =
    # Function that will detect the types of the tokens
    if s[0] == '*': return tokenIdTable["*"]
    if s[0] == '+': return tokenIdTable["+"]
    if s[0] == '(': return tokenIdTable["("]
    if s[0] == ')': return tokenIdTable[")"]
    return tokenIdTable["LIT"]

# Build the grammar to use it. This might take time so consider doing this at compile time for maximum performance.
let full_grammar = constructGrammar(int_grammar)

# We convert the string into "tokens" (i.e a seq of strings with some metadata)
let tokens = tokenize(input_string, @["*", "+", "(", ")"], @[" ", "\t",
        "\n"], labeler)

# We parse the string
let ast = parse(tokens, full_grammar)

echo ast # We obtain a tree like data structure !
``` 

Even more examples in `tests`.
[Complete documentation available here](https://vanyle.github.io/lrparser/lrparser.html)

## Contributing

Feel free to implement a LALR parser.