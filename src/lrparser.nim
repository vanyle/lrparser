##[

A LR parser implementation in Nim.

LR parsers (https://en.wikipedia.org/wiki/LR_parser) are
a way to convert format grammars (sets of rules) into a program
that can convert some text into a structured tree structure.

They can be used to parse expressions such as: `x+x*x+x` into `(x+(x*x))+x`.

More complex grammar allow to parse source code to create programming languages.

To parse a string, you need to first split it into tokens as the parser only accepts seqs of Tokens (a token is a wrapper around a string, that provides information about the position of the token for error reporting)

To do this, you can write your own tokenizer (for example, `input.split(" ").mapIt(Token(content: it))` can be a simple tokenizer), or
you can use the `tokenize` function that provides basic tokenizing abilities and outputs tokens instead of strings.

]##
runnableExamples:
    import tables, strutils

    let input_string = """
        4 * 41 + 3 * (7 + 2)
    """
    # The ruleset
    let grammar = @[
        ("E", @["E", "+", "T"]),
        ("E", @["T"]),
        ("T", @["T", "*", "F"]),
        ("T", @["F"]),
        ("F", @["(", "E", ")"]),
        ("F", @["LIT"]),
    ]

    # We need to convert the strings to ids so that the parser understands them
    let tokenIdTable = makeTokenIdTable(grammar) # returns a Table[string, int]
    let int_grammar = convertGrammar(grammar, tokenIdTable)

    proc labeler(s: string): int =
        # Function that will detect the types of the tokens
        if s[0] == '*': return tokenIdTable["*"]
        if s[0] == '+': return tokenIdTable["+"]
        if s[0] == '(': return tokenIdTable["("]
        if s[0] == ')': return tokenIdTable[")"]
        return tokenIdTable["LIT"]

    # Build the grammar to use it.
    # This might take time so consider doing this at compile time for maximum performance.
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
            # id is the type of the token.
            if t.id == tokenIdTable["E"]:
                return eval(t.children[0]) + eval(t.children[2])
            if t.id == tokenIdTable["T"]:
                return eval(t.children[0]) * eval(t.children[2])
            if t.id == tokenIdTable["F"]:
                return eval(t.children[1])

        return eval(t.children[0])

    let r = eval(ast)
    echo input_string.strip, " = ", r



import strutils, strformat, sets, tables, sequtils, algorithm, strformat


type
    TokenTreeRef* = ref Token

    Token* = object
        ## Represents a piece of string. Might be inside an AST.
        ## When in an AST, the children tokens are in `children`.
        ## `(col,line)` are the position of the token in the original string/file.
        ## `charcount` is the position in the string in bytes.
        ## `id` is the type of the token given by the `TokenLabeler`
        children*: seq[TokenTreeRef]
        content*: string
        id*: int
        charcount*: int
        line*: int
        col*: int

    TokenLabeler* = proc(s: string): int
    ## Takes a string and gives it a token type

    Grammar* = object
        ## Internal representation of the set of rules provided
        table: seq[Table[int, (StateTypes, int)]]
        grammar: seq[(int, seq[int])]

    CachableGrammar = object
        # Used for some dynamic programming when computing first and follow.
        grammar: seq[(int, seq[int])]
        nonterminals: HashSet[int]
        terminals: HashSet[int]

        first_cache: Table[int, HashSet[int]]
        follow_cache: Table[int, HashSet[int]]
    StateTypes = enum
        GOTO
        SHIFT
        REDUCE
        ACCEPT


const DOT = -1 # TODO: remove this
const EPSILON = -2 # TODO: remove this
const DOLLAR = -3

proc `$`*(ttr: TokenTreeRef): string =
    if ttr.content != "": return fmt"{ttr.content}"
    else:
        result = "("
        for c in 0..<ttr.children.len:
            if c != 0: result &= ","
            result &= $(ttr.children[c])
        result &= ")"

proc tokenize*(
        s: string,
        tokenBreakersThatAreTokens: seq[string],
        tokenBreakers: seq[string],
        tl: TokenLabeler
    ): seq[Token] =
    ## Split the string `s` along the `tokenBreakers` and returns it as a list of tokens
    var i = 0
    var col = 0
    var line = 0
    var lastTokenIdx = (i: 0, col: 0, line: 0)
    while i < s.len:
        if s[i] == '\n':
            col = 0
            line += 1
        for t in tokenBreakersThatAreTokens:
            if s[i..<s.len].startswith(t):
                if lastTokenIdx.i < i:
                    result.add(
                        Token(
                            col: lastTokenIdx.col,
                            line: lastTokenIdx.line,
                            charcount: lastTokenIdx.i,
                            content: s[lastTokenIdx.i..<i]
                        )
                    )
                result.add(
                    Token(
                        col: col,
                        line: line,
                        charcount: i,
                        content: s[i..<i + t.len]
                    )
                )
                for j in i+1..<i+t.len:
                    if s[j] == '\n':
                        col = 0
                        line += 1
                i += t.len - 1
                lastTokenIdx.i = i+1
                lastTokenIdx.col = col
                lastTokenIdx.line = line

        for t in tokenBreakers:
            if s[i..<s.len].startswith(t):
                # flush token
                if lastTokenIdx.i < i:
                    result.add(
                        Token(
                            col: lastTokenIdx.col,
                            line: lastTokenIdx.line,
                            charcount: lastTokenIdx.i,
                            content: s[lastTokenIdx.i..<i]
                        )
                    )
                for j in i+1..<i+t.len:
                    if s[j] == '\n':
                        col = 0
                        line += 1
                i += t.len - 1
                lastTokenIdx.i = i+1
                lastTokenIdx.col = col
                lastTokenIdx.line = line
        i += 1


    if lastTokenIdx.i < i:
        result.add(
            Token(
                col: lastTokenIdx.col,
                line: lastTokenIdx.line,
                charcount: lastTokenIdx.i,
                content: s[lastTokenIdx.i..i-1]
            )
        )

    # Add ids:
    for i in 0..<result.len:
        result[i].id = tl(result[i].content)


proc attach_dot(production: (int, seq[int])): (int, seq[int]) =
    let (lhsp, rhsp) = production
    return (lhsp, DOT & rhsp)

proc symbol_after_dot(item: (int, seq[int])): int =
    # We assume that item[1] contains a DOT token. An out-of-bounds exception will be thrown otherwise.
    let (_, rhsi) = item
    let dotIdx = rhsi.find(DOT)
    if dotIdx >= rhsi.len-1: return EPSILON
    return rhsi[rhsi.find(DOT)+1]

proc progress_dot(item: (int, seq[int])): (int, seq[int]) =
    var (lhsi, rhsi) = item
    for i in 0..rhsi.len:
        if rhsi[i] == DOT:
            rhsi[i] = rhsi[i+1]
            rhsi[i+1] = DOT
            break
    return (lhsi, rhsi)

proc remove_dot(item: (int, seq[int])): (int, seq[int]) =
    let (lhsi, rhsi) = item
    let didx = rhsi.find(DOT)
    if didx != -1:
        var cp = rhsi
        cp.delete(didx)
        return (lhsi, cp)
    return (lhsi, rhsi)

proc construct_items(grammar: seq[(int, seq[int])]):
    (seq[seq[(int, seq[int])]],
        seq[int],
       Table[int, seq[int]]) =
    ## From a given grammar, generate the corresponding goto rules for the SLR parser.
    ## Also, we return the list of symbols of the grammars

    var nonterminals: HashSet[int]
    for i in grammar:
        nonterminals.incl(i[0])

    var terminals: HashSet[int]
    # set([t for (lhs,rhs) in G for t in rhs if t not in nonterminals])
    for i in grammar:
        for j in i[1]:
            if j notin nonterminals:
                terminals.incl(j)

    proc closure(jIn: seq[(int, seq[int])]): seq[(int, seq[int])] =
        var done = false
        var J = jIn
        while not done:
            done = true
            var i = -1
            while i < J.len-1:
                i += 1
                let B = symbol_after_dot(J[i])
                if B notin nonterminals:
                    continue
                for production in grammar:
                    let (lhsp, _) = production
                    if lhsp != B:
                        continue
                    let new_item = attach_dot(production)
                    if new_item in J:
                        continue
                    J.add(new_item)
                    done = false
        return J

    proc goto(I: seq[(int, seq[int])], X: int): seq[(int, seq[int])] =
        var J: seq[(int, seq[int])] = @[]
        for item in I:
            let B = symbol_after_dot(item)
            if B == X:
                let new_item = progress_dot(item)
                J.add(new_item)
        return J

    var C: seq[seq[(int, seq[int])]] = @[closure(@[attach_dot(grammar[0])])]

    let symbols = nonterminals.union(terminals).toSeq

    var goto_moves: Table[int, seq[int]]
    var done = false
    while not done:
        done = true
        for index in 0..<C.len:
            let I = C[index]
            goto_moves[index] = newSeq[int](symbols.len).mapIt(-1) # fill with -1
            for symbol in symbols:
                let J = closure(goto(I, symbol))
                if len(J) == 0:
                    continue
                if J notin C:
                    C.add(J)
                    done = false
                goto_moves[index][symbols.find(symbol)] = C.find(J)

    return (C, symbols, goto_moves)

proc first(X: int, grammar: var CachableGrammar): HashSet[int] =
    ## Return the character that might be susbituted by X.
    ## If X cannot appear from a substitution, this returns {X}
    ## If multiple characters get substituted by X, only add the first one.

    if X in grammar.first_cache:
        return grammar.first_cache[X]
    var r: HashSet[int]
    var x_prod: seq[(int, seq[int])] # all the rules that can generate X

    for i in grammar.grammar:
        if i[0] == X:
            x_prod.add(i)

    if EPSILON notin grammar.terminals:
        grammar.terminals.incl(EPSILON)

    for production in x_prod:
        let (_, rhsp) = production
        for index in 0..<rhsp.len:
            let symbol = rhsp[index]
            if symbol == X:
                break
            if symbol in grammar.terminals:
                r.incl(symbol)
                break
            let first_y = first(symbol, grammar)
            r = r + (first_y - [EPSILON].toHashSet)
            if EPSILON notin first_y:
                break
            if index == len(rhsp)-1:
                r.incl(EPSILON)
    grammar.first_cache[X] = r
    return grammar.first_cache[X]

proc first_of(beta: seq[int], grammar: var CachableGrammar): HashSet[int] =
    for index in 0..<beta.len:
        let symbol = beta[index]
        if symbol in grammar.terminals:
            result.incl(symbol)
            break
        if symbol == EPSILON:
            result.incl(EPSILON)
            break
        let first_y = first(symbol, grammar)
        result = result + (first_y - [EPSILON].toHashSet)
        if EPSILON notin first_y:
            break
        if index == len(beta)-1:
            result.incl(EPSILON)
    return result

proc follow(X: int, grammar: var CachableGrammar): HashSet[int] =
    ## Return a set of all the symbols that immediatly might follow X, when taking the substitution rules into account.
    if X in grammar.follow_cache:
        return grammar.follow_cache[X]
    let FIRST_SYMBOL = grammar.grammar[0][0]

    if X == FIRST_SYMBOL:
        result.incl(DOLLAR)

    var x_prod: seq[(int, seq[int])] # all the rules that can generate X
    for p in grammar.grammar:
        if X in p[1]:
            x_prod.add(p)

    for production in x_prod:
        let (lhsp, rhsp) = production
        for index in 0..<rhsp.len:
            let symbol = rhsp[index]
            if symbol != X:
                continue
            if (index == len(rhsp) - 1) and (lhsp != symbol):
                result = result + follow(lhsp, grammar)
                continue
            let first_beta = first_of(rhsp[index+1 ..< rhsp.len], grammar)
            result = result + (first_beta - [EPSILON].toHashSet)
            if (EPSILON in first_beta) and (lhsp != symbol):
                result = result + follow(lhsp, grammar)


    grammar.follow_cache[X] = result
    return grammar.follow_cache[X]

proc constructGrammar*(grammar: seq[(int, seq[int])]): Grammar =
    ## Build a parsing table from a given grammar.
    ## Be careful, the size of the grammar is

    var (states, symbols, transitions) = construct_items(grammar)
    symbols.add(DOLLAR)

    var cgrammar: CachableGrammar
    cgrammar.grammar = grammar

    var nonterminals: HashSet[int]
    for i in grammar:
        nonterminals.incl(i[0])

    var terminals: HashSet[int]
    for i in grammar:
        for j in i[1]:
            if j notin nonterminals:
                terminals.incl(j)

    var table = newSeq[Table[int, (StateTypes, int)]](states.len)
    cgrammar.terminals = terminals
    cgrammar.nonterminals = nonterminals


    for i in 0..<states.len:
        for j in 0..<len(symbols)-1:
            let symbol = symbols[j]
            if transitions[i][j] == -1:
                continue
            if symbol in nonterminals:
                table[i][symbol] = (GOTO, transitions[i][j])
            else:
                table[i][symbol] = (SHIFT, transitions[i][j])


    for index in 0..<states.len:
        let I = states[index]
        for item in I:
            let B = symbol_after_dot(item)
            if B != EPSILON:
                continue
            let (lhsi, _) = item
            let prod_index = grammar.find(remove_dot(item))
            if prod_index == 0:
                table[index][DOLLAR] = (ACCEPT, index)
                break
            let follow_left = follow(lhsi, cgrammar)

            for symbol in follow_left:
                if symbol in table[index]:
                    # Proper error reporting would be nice.
                    # echo "Unable to build fully compliant table, there is symbol duplication: ", index," : ",symbol
                    break
                table[index][symbol] = (REDUCE, prod_index)

    result.table = table
    result.grammar = grammar

    return result

proc parseAll*(input_string: seq[Token], g: Grammar, debugTable: Table[int, string]):
            seq[TokenTreeRef] =
    var buf = input_string
    buf.add(Token(id: DOLLAR))
    var state_stack = @[0]
    var symbol_stack: seq[TokenTreeRef] = @[]
    var index = 0

    while true:
        let symbol = buf[index]
        var s = state_stack[state_stack.len - 1]
        if symbol.id notin g.table[s]:
            if symbol.id == DOLLAR:
                # Proper error reporting instead of echo would be nice too.
                echo "Unexpected end of output"
            else:
                echo fmt"Syntax error at symbol {symbol.content} at ({symbol.line+1}, {symbol.col+1})"
            break
        let (action, value) = g.table[s][symbol.id]

        if action == SHIFT:
            if symbol.id in debugTable:
                echo "SHIFT ",debugTable[symbol.id]

            var newsym = new(Token)
            newsym[] = symbol
            symbol_stack.add(newsym)
            state_stack.add(value)
            index += 1

        if action == REDUCE:
            # Build the AST here.
            let (lhsp, rhsp) = g.grammar[value]
            
            if value in debugTable:
                echo "REDUCE ",debugTable[lhsp]," <- ", rhsp.mapIt(debugTable[it]).join(",")

            var children: seq[TokenTreeRef] = @[]
            for i in 0..<rhsp.len:
                discard state_stack.pop()
                children.add(symbol_stack.pop())
            s = state_stack[state_stack.len - 1]
            state_stack.add(g.table[s][lhsp][1])
            var newtok = new(Token)
            newtok.id = lhsp
            newtok.children = children.reversed()
            symbol_stack.add(newtok)

        if action == ACCEPT:
            break

    # By construction, only 1 element remains at the end when there are no errors.
    return symbol_stack

proc parse*(input_string: seq[Token], g: Grammar): TokenTreeRef = 
    ## Perform the parsing of a sequence of tokens. Returns a tree made of tokens, that follows
    ## the set of rules described in the grammar `g` provided.
    ## In case of syntax errors, print the location of the error and return a partially parsed tree.
    ## Use `parseAll` if you need better error management
    var debugTable: Table[int, string]
    parseAll(input_string, g, debugTable)[0]

proc convertGrammar*[T: enum](g: seq[(T, seq[T])]): seq[(int, seq[int])] =
    for el in g:
        var r: (int, seq[int]) = (ord(el[0]), @[])
        for i in el[1]:
            r[1].add(ord(i))
        result.add(r)

proc makeTokenIdTable*[T](g: seq[(T, seq[T])]): Table[T, int] =
    var idCounter = 0
    for el in g:
        if el[0] notin result:
            result[el[0]] = idCounter
            idCounter += 1
        for i in el[1]:
            if i notin result:
                result[i] = idCounter
                idCounter += 1

proc convertGrammar*[T](g: seq[(T, seq[T])], tab: Table[T, int]): seq[(int, seq[int])] =
    for el in g:
        var r: (int, seq[int]) = (tab[el[0]], @[])
        for i in el[1]:
            r[1].add(tab[i])
        result.add(r)

when isMainModule:
    type TokenTypes = enum
        # E, F and T represents wrappers around different priority operations
        TT_LIT
        TT_ADD
        TT_MUL
        TT_S
        TT_E
        TT_F
        TT_T
        TT_LPAR
        TT_RPAR

    let grammar = @[
        (TT_S, @[TT_E]),
        (TT_E, @[TT_E, TT_ADD, TT_T]),
        (TT_E, @[TT_T]),
        (TT_T, @[TT_T, TT_MUL, TT_F]),
        (TT_T, @[TT_F]),
        (TT_F, @[TT_LPAR, TT_E, TT_RPAR]),
        (TT_F, @[TT_LIT]),
    ]
    let int_grammar = convertGrammar(grammar)

    proc labeler(s: string): int =
        if s[0] == '*': return ord(TT_MUL)
        if s[0] == '+': return ord(TT_ADD)
        if s[0] == '(': return ord(TT_LPAR)
        if s[0] == ')': return ord(TT_RPAR)
        return ord(TT_LIT)

    let full_grammar = constructGrammar(int_grammar)

    let to_parse = "apple * apple_price + fixed_fare"
    let tokens = tokenize(to_parse, @["*", "+"], @[" "], labeler)

    echo parse(tokens, full_grammar)
