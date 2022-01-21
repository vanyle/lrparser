
# Let's define a joy-programming language grammar and try to execute it !

import unittest

import lrparser
import tables
import strutils
import langexample


test "We are able to parse and interpret a toy language":
    let input_string = """
        let f = 4 * 5 + 2;
        let g = 2 * f;
        let h = function(a){
            function(b){
                2*b + a
            }
        };
        print(f);
        print(g);
        let i = h(f)(g);
        print(i)
    """

    proc tokToInt(t: TokenTreeRef): int =
        if t.id == tokenIdTable["LIT"]: return parseInt(t.content)
        return 0

    let (context, _) = interpret(input_string, initTable[string,TokenTreeRef]())
    check tokToInt(context["f"]) == 4 * 5 + 2
    check tokToInt(context["g"]) == 2 * tokToInt(context["f"])
    check tokToInt(context["i"]) == tokToInt(context["f"]) + 2 * tokToInt(context["g"])

test "The toy language supports recusivity and conditions":
    let input_string = """
        let print_stuff = function(a){
            print(a);
            if(a)(a+print_stuff(a - 1))(0)
        };
        let b = print_stuff(10)
    """

    proc tokToInt(t: TokenTreeRef): int =
        if t.id == tokenIdTable["LIT"]: return parseInt(t.content)
        return 0

    let (context, _) = interpret(input_string, initTable[string,TokenTreeRef]())
    check tokToInt(context["b"]) == 10 * 11 div 2# 10 + 9 + .. + 1