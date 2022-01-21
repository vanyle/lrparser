# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import lrparser
import tables
import strutils
import sequtils, parseutils

test "can parse simple grammar":
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

    # We evaluate the tree
    proc eval(t: TokenTreeRef): int =
        if t.content != "":
            let r = parseInt(t.content)
            return r

        if t.children.len == 3:
            if t.id == tokenIdTable["E"]:
                return eval(t.children[0]) + eval(t.children[2])
            if t.id == tokenIdTable["T"]:
                return eval(t.children[0]) * eval(t.children[2])
            if t.id == tokenIdTable["F"]:
                return eval(t.children[1])

        return eval(t.children[0])

    let r = eval(ast)
    #echo input_string.strip," = ",r
    check r == 191


test "can build the parse table at compile time":
    let input_string = "<<<>>><<><>>"
    
    # The ruleset. Generated at compile time !
    const grammar = @[
            ("S", @["E"]),
            ("E", @["E","E"]),
            ("E", @["<", "E", ">"]),
            ("E", @["<",">"]),
        ]

    const tokenIdTable = static makeTokenIdTable(grammar)

    let full_grammar = static:
        let int_grammar = convertGrammar(grammar, tokenIdTable)
        echo "Computing the grammar at compile time ..."
        constructGrammar(int_grammar)

    proc labeler(s: string): int =
        if s[0] == '<': return tokenIdTable["<"]
        return tokenIdTable[">"]


    let tokens = tokenize(input_string, @["<", ">"], @[], labeler)
    let ast = parse(tokens, full_grammar)
    check $ast == "((<,(<,(<,>),>),>),(<,((<,>),(<,>)),>))"

test "other tokenizer can be used":
    let input_string = "open open close close"
    let tokens = input_string.split(" ").mapIt(Token(content: it, id: if it == "open": 2 else: 1))
    
    let grammar = @[
        (4, @[3]), # finish = 3
        (3, @[2,1]), # 3 = open, close
        (3, @[2,3,1]) # 3 = open, 3, close 
    ]
    # no need to convert the grammar to int, it already uses integers !
    let full_grammar = constructGrammar(grammar)
    let ast = parse(tokens, full_grammar)
    check $ast == "(open,(open,close),close)"

