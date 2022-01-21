import lrparser
import tables
import strutils
import strformat

# Example of how the language looks. It's very functional.
#[
    let f = 3;
    let g = 4 * 4 + 2;
    let sum = function(a){
        function(b){
            a+b
        }
    };
    let h = sum(f)(g);
    print(f);
    print(g);
    print(h)
]#

# The ruleset
let grammar = @[
    ("FINAL", @["LE"]),
    # LE is a list of statements
    ("LE", @["S",";","LE"]),
    ("LE", @["S"]),

    # statement
    ("S", @["let","VAR","=","E"]),
    ("S", @["E"]),
    
    # expressions
    ("E", @["E", "+", "T"]),
    ("E", @["E", "-", "T"]),
    ("E", @["T"]),

    ("T", @["T", "*", "F"]),
    ("T", @["F"]),
    # function calls. Only 1 argument, use currying for more.
    ("F", @["(", "E", ")"]),
    ("F", @["FC"]),

    ("FC", @["F","(","E",")"]),
    ("FC", @["FVAL"]),

    ("FVAL",@["FD","FBODY"]),
    ("FBODY",@["{","LE","}"]),
    ("FD", @["function","(","VAR",")"]),

    ("F", @["LIT"]),
    ("F", @["VAR"]),
]

# We need to convert the strings to ids so that the parser understands it
let tokenIdTable* = makeTokenIdTable(grammar) # returns a Table[string, int]
let int_grammar = convertGrammar(grammar, tokenIdTable)

# reverse table:
var reverseTable: Table[int, string]
for k,v in tokenIdTable: reverseTable[v] = k

proc labeler(s: string): int =
    # Function that will detect the types of the tokens
    if s[0] == '*': return tokenIdTable["*"]
    if s[0] == '+': return tokenIdTable["+"]
    if s[0] == '-': return tokenIdTable["-"]
    if s[0] == ';': return tokenIdTable[";"]
    if s[0] == ',': return tokenIdTable[","]
    if s[0] == '=': return tokenIdTable["="]
    if s == "function": return tokenIdTable["function"]
    if s == "let": return tokenIdTable["let"]
    if s[0] == '(': return tokenIdTable["("]
    if s[0] == ')': return tokenIdTable[")"]
    if s[0] == '{': return tokenIdTable["{"]
    if s[0] == '}': return tokenIdTable["}"]

    try:
        discard parseInt(s)
        return tokenIdTable["LIT"]
    except ValueError:
        return tokenIdTable["VAR"]

# Build the grammar to use it. This might take time so consider doing this at compile time for maximum performance.
let full_grammar = constructGrammar(int_grammar)


proc merge[T](a,b: Table[string, T]): Table[string, T] =
    for k,v in a: result[k] = v
    for k,v in b: result[k] = v

# We execute the program and display the output.
# This is a small interpreter implementation.
# The variables are stored as TokenTrees so that we can easily represent functions
# We also introduct the special "if" token:
# if(a > b)(a)(b) # is a or b depending on the first argument

let numType = tokenIdTable["LIT"]

# evaluate and interpret are mutually recursive
proc interpret*(ast: TokenTreeRef, context: Table[string, TokenTreeRef]): (Table[string, TokenTreeRef],TokenTreeRef)

proc evaluate(exp: TokenTreeRef, context: Table[string, TokenTreeRef]): TokenTreeRef =
    ## Evaluate an expression. The id of expression must not be LE / S / FINAL
    ## Return a token that is either FVAL or LIT (the only 2 types of our language)
    if exp.children.len == 0:
        if exp.id == numType:
            return exp
        if exp.id == tokenIdTable["VAR"]:
            let vname = exp.content
            if vname == "if":
                # return the magic if function
                return TokenTreeRef(content: "if", id: tokenIdTable["FVAL"])
            if vname == "print":
                # return the magic print function
                return TokenTreeRef(content: "print", id: tokenIdTable["FVAL"])
            if vname in context:
                return context[vname]
            else:
                echo fmt"Undefined variable {vname} at ({exp.line},{exp.col})"
                return TokenTreeRef(content: "0", id: numType)
    if exp.children.len == 1:
        return evaluate(exp.children[0], context)

    if exp.id == tokenIdTable["E"]:
        let a = evaluate(exp.children[0], context)
        let b = evaluate(exp.children[2], context)
        if a.id != numType or b.id != numType:
            echo fmt"Typing issue: the elements are not integers, cannot add them at ({exp.line},{exp.col})"
            return TokenTreeRef(content: "0", id: numType)
        if exp.children[1].content == "+":
            return TokenTreeRef(content: $(parseInt(a.content) + parseInt(b.content)),id: numType)
        if exp.children[1].content == "-":
            return TokenTreeRef(content: $(parseInt(a.content) - parseInt(b.content)),id: numType)

    if exp.id == tokenIdTable["T"]:
        let a = evaluate(exp.children[0], context)
        let b = evaluate(exp.children[2], context)
        if a.id != numType or b.id != numType:
            echo fmt"Typing issue: the elements are not integers, cannot multiply them at ({exp.line},{exp.col})"
            return TokenTreeRef(content: "0", id: numType)
        return TokenTreeRef(content: $(parseInt(a.content) * parseInt(b.content)),id: numType)

    if exp.id == tokenIdTable["F"]:
        return exp.children[1]

    if exp.id == tokenIdTable["FC"]: # F ( E )
        let function_val = evaluate(exp.children[0], context) # compute the "body" of the function
        
        if function_val.id != tokenIdTable["FVAL"]:
            echo fmt"Typing issue: The value provided is not a function at ({exp.line},{exp.col})"
            return TokenTreeRef(content: "0", id: numType)

        if function_val.content.len != 0:
            # we are dealing with a magic function
            if function_val.content == "print":
                let parameter = evaluate(exp.children[2], context)
                if parameter.id == tokenIdTable["FVAL"]:
                    echo "<function object>"
                elif parameter.id == tokenIdTable["LIT"]:
                    echo parameter.content
                # The fake "print" function always returns 0 
                return TokenTreeRef(content: "0", id: numType)

            if function_val.content == "if":
                return TokenTreeRef(content: "if2", id: tokenIdTable["FVAL"],
                    children: @[exp.children[2]]
                )
            if function_val.content == "if2":
                return TokenTreeRef(content: "if3", id: tokenIdTable["FVAL"],
                    children: @[function_val.children[0],exp.children[2]]
                )
            if function_val.content == "if3":
                let condition = evaluate(function_val.children[0], context)
                let option1 = function_val.children[1]
                let option2 = exp.children[2]
                if condition.id != tokenIdTable["LIT"]:
                    return evaluate(option1, context)

                if parseInt(condition.content) == 0:
                    return evaluate(option2, context)
                else:
                    return evaluate(option1, context)

        let parameter = evaluate(exp.children[2], context)
        # function_val = FD FBODY LAMBDAS = [function ( a )] [{ LE }] []
        let parameter_name = function_val.children[0].children[2].content
        var function_context = context
        function_context[parameter_name] = parameter
        for i in function_val.children[2].children:
            function_context[i.content] = i.children[0]
        # Add the lambda variables to the function context
        # We only care about the result of the function, it's context is discarded.
        let (_, function_result) = interpret(function_val.children[1].children[1], function_context)
        
        return function_result

    if exp.id == tokenIdTable["FVAL"]:
        # add the evaluation context to the expression, this is to capture lambda variables
        var lambda_variables: seq[TokenTreeRef] = @[]
        for k,v in context:
            lambda_variables.add(TokenTreeRef(content: k, children: @[v]))
        var n = exp
        n.children.add(
            TokenTreeRef(children: lambda_variables, id: tokenIdTable["LE"])
        )
        return n

    return TokenTreeRef(content: "0", id: numType)
    


proc interpret*(ast: TokenTreeRef, context: Table[string, TokenTreeRef]): (Table[string, TokenTreeRef],TokenTreeRef) =
    ## Interpret the code given in `ast` with the context `context`.
    ## Returns an updated context with new variables as well as the value of the last evaluation.
    if ast.id == tokenIdTable["FINAL"]:
        return interpret(ast.children[0], context)
    if ast.id == tokenIdTable["LE"]:
        if ast.children.len == 1:

            return interpret(ast.children[0], context)
        else:
            # Discard all values except for the one of the last evaluation.
            # Note that code execution is top to bottom
            let (c, _) = interpret(ast.children[0], context)
            let c2 = merge(context, c)
            return interpret(ast.children[2], c2)
    if ast.id == tokenIdTable["S"]:
        if ast.children.len == 4: # len VAL = E
            var c = context
            c[ast.children[1].content] = evaluate(ast.children[3], context)
            return (c, TokenTreeRef(content: "0", id: numType)) # let statement have no value, 0 is nil.
        else:
            return interpret(ast.children[0], context)

    if ast.id == tokenIdTable["E"]:
        return (context, evaluate(ast, context))
    
    return (context, TokenTreeRef(content: "0", id: numType))


proc interpret*(input_string: string, context: Table[string, TokenTreeRef]): (Table[string, TokenTreeRef],TokenTreeRef) =
    let tokens = tokenize(input_string, @["*", "+","{","}", "-", "(", ")","=",",",";"], @[" ", "\t","\n"], labeler)

    let ast = parse(tokens, full_grammar)
    return interpret(ast, initTable[string,TokenTreeRef]())

import os

when isMainModule:
    # read file and interpret it.
    if paramCount() != 1:
        echo "Usage: vlang [filename.v]"
        quit(1)
    if not fileExists(paramStr(1)):
        echo paramStr(1)," does not exist."
        quit(1)
    let s = readFile(paramStr(1))
    let tokens = tokenize(s, @["*", "+", "(", ")","=",",",";"], @[" ", "\t","\n","\r"], labeler)
    let ast = parse(tokens, full_grammar)
    discard interpret(ast, initTable[string,TokenTreeRef]())