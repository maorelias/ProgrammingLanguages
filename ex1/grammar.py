"""
This module contains functions for analyzing a grammar, finding
its NULLABLE, FIRST, FOLLOW and SELECT sets, and determining if it is
LL(1).

A grammar is represented as a list of rules of the form (head, body)
where head is the goal non-terminal, and body is a tuple of symbols,
or the empty tuple () for an epsilon rule.

The start symbol is always the head of the first rule in the list.
"""

from symbols import *


grammar_recitation = [
    (S, (ID, ASSIGN, E)),              # S -> id := E
    (S, (IF, LP, E, RP, S, ELSE, S)),  # S -> if (E) S else S
    (E, (T, EP)),                      # E -> T EP
    (T, (ID,)),                        # T -> id
    (T, (LP, E, RP)),                  # T -> (E)
    (EP, ()),                          # EP -> epsilon
    (EP, (PLUS, E)),                   # EP -> + E
]


def calculate_nullable(terminals, nonterminals, grammar):
    """
    Return the set of nullable nonterminals in the given grammar.

    terminals and nonterminals are sets, grammer is a list of rules as
    explained above.

    --- DO NOT MODIFY THIS FUNCTION ---
    """
    nullable = set()
    for head, body in grammar:
        if body == ():
            nullable.add(head)
    changing = True
    while changing:
        changing = False
        for head, body in grammar:
            if set(body) <= nullable and head not in nullable:
                nullable.add(head)
                changing = True
    return nullable


def calculate_first(terminals, nonterminals, grammar, nullable):
    """
    Return a dictionary mapping terminals and nonterminals to their FIRST set
    """
    first = dict()
    for t in terminals:
        first[t] = {t}
    for a in nonterminals:
        first[a] = set()
    changing = True
    while changing:
        changing = False
        for head, body in grammar:
            if len(body) == 0:
                continue
            i = 0
            while True:    
                item = body[i]
                for elem in first[item]:
                    if elem not in first[head]:
                        changing = True
                        first[head].add(elem)
                if elem not in nullable:
                    break
                i = i + 1
    return first


def calculate_follow(terminals, nonterminals, grammar, nullable, first):
    """
    Return a dictionary mapping terminals and nonterminals to their FOLLOW set
    """
    follow = dict()
    for a in nonterminals:
        follow[a] = set()
    start_nonterminal = grammar[0][0]
    follow[start_nonterminal] = {EOF}
    changing = True
    while changing:
      changing = False
      for head, body in grammar:
        if len(body) > 0:
            if body[len(body) - 1] in nonterminals:
              for elem in follow[head]:
                  if not elem in follow[body[len(body) - 1]]:
                      changing = True
                      follow[body[len(body) - 1]].add(elem)
            for i in range(len(body) - 2, -1, -1):
                  if body[i] in terminals:
                      continue
                  for elem in first[body[i + 1]]:
                          if elem not in follow[body[i]]:
                                changing = True
                                follow[body[i]].add(elem)
                          if body[i + 1] in nullable: # If next item is nullable add it's follow.
                                for elem in follow[body[i + 1]]:
                                    if not elem in follow[body[i]]:
                                        changing = True
                                        follow[body[i]].add(elem)
    return follow


def calculate_select(terminals, nonterminals, grammar, nullable, first, follow):
    """
    Return a dictionary mapping rules to their SELECT (a.k.a. PREDICT) set
    """
    select = dict()
    for rule in grammar:
        select[rule] = set()
    for rule in grammar:
        if rule[1] != ():
            select[rule] |= first[rule[1][0]]
        isNullable = True
        for elem in rule[1]:
            if not elem in nullable:
                isNullable = False
        if isNullable:
            select[rule] |= follow[rule[0]]
    return select


def format_rule(r):
    """
    --- DO NOT MODIFY THIS FUNCTION ---
    """
    return "{} -> {}".format(r[0], ' '.join(r[1]))


def find_terminals_and_nonterminals(grammar):
    """
    Find the terminals and nonterminals appearing in the given grammar.

    --- DO NOT MODIFY THIS FUNCTION ---
    """
    symbols = set()
    nonterminals = set()
    for head, body in grammar:
        nonterminals.add(head)
        symbols.update(body)
    terminals = symbols - nonterminals
    return terminals, nonterminals


def analyze_grammar(grammar):
    """
    Use other functions in this module to analyze the grammar and
    check if it is LL(1).

    --- DO NOT MODIFY THIS FUNCTION ---
    """
    print("Analyzing grammar:")
    for r in grammar:
        print("    " + format_rule(r))
    print()

    terminals, nonterminals = find_terminals_and_nonterminals(grammar)
    print("terminals = ", terminals)
    print("nonterminals = ", nonterminals)
    print()

    nullable = calculate_nullable(terminals, nonterminals, grammar)
    print("nullable = ", nullable)
    print()

    first = calculate_first(terminals, nonterminals, grammar, nullable)
    for k in sorted(first.keys()):
        print("first({}) = {}".format(k, first[k]))
    print()

    follow = calculate_follow(terminals, nonterminals, grammar, nullable, first)
    for k in sorted(follow.keys()):
        print("follow({}) = {}".format(k, follow[k]))
    print()

    select = calculate_select(terminals, nonterminals, grammar, nullable, first, follow)
    for k in sorted(select.keys()):
        print("select({}) = {}".format(format_rule(k), select[k]))
    print()

    ll1 = True
    n = len(grammar)
    for i in range(n):
        for j in range(i+1, n):
            r1 = grammar[i]
            r2 = grammar[j]
            if r1[0] == r2[0] and len(select[r1] & select[r2]) > 0:
                ll1 = False
                print("Grammar is not LL(1), as the following rules have intersecting SELECT sets:")
                print("    " + format_rule(r1))
                print("    " + format_rule(r2))
    if ll1:
        print("Grammar is LL(1).")
    print()
    


grammar_json_4a = [
    (obj, (LB, RB)),                          # obj -> {}
    (obj, (LB, members, RB)),                 # obj -> { members }
    (members, (keyvalue,)),                   # members -> keyvalue
    (members, (members, COMMA, members)),     # members -> members , members
    (keyvalue, (STRING, COLON, value)),       # keyvalue -> string : value
    (value, (STRING,)),                       # value -> string
    (value, (INT,)),                          # value -> int
    (value, (obj,))                           # value -> obj
]

grammar_json_4b = [
    (obj, (LB, RB)),                          # obj -> {}
    (obj, (LB, elements, RB)),                # obj -> {elements}
    (elements, (elements, COMMA, members)),   # elements -> elements , members
    (elements, (members,)),                   # elements -> members
    (members, (keyvalue,)),                   # members -> keyvalue
    (keyvalue, (STRING, COLON, value)),       # keyvalue -> string : value
    (value, (STRING,)),                       # value -> string
    (value, (INT,)),                          # value -> int
    (value, (obj,))                           # value -> obj
]

grammar_json_4c = [
    (obj, (LB, after_obj)),                   # obj -> { after_obj
    (after_obj, (RB,)),                       # after_obj -> }
    (after_obj, (elements, RB)),              # after_obj -> elements }
    (elements, (members, after_members)),     # elements -> members after_members
    (after_members, (COMMA, elements)),       # after_members -> , elements
    (after_members, ()),                      # after_members -> epsilon
    (members, (keyvalue,)),                   # members -> keyvalue
    (keyvalue, (STRING, COLON, value)),       # keyvalue -> string : value
    (value, (STRING,)),                       # value -> string
    (value, (INT,)),                          # value -> int
    (value, (obj,))                           # value -> obj
]

grammar_json_6 = [
    (obj, (LB, after_obj)),                   # obj -> { after_obj
    (after_obj, (RB,)),                       # after_obj -> }
    (after_obj, (elements, RB)),              # after_obj -> elements }
    (elements, (members, after_members)),     # elements -> members after_members
    (after_members, (COMMA, elements)),       # after_members -> , elements
    (after_members, ()),                      # after_members -> epsilon
    (members, (keyvalue,)),                   # members -> keyvalue
    (keyvalue, (STRING, COLON, value)),       # keyvalue -> string : value
    (value, (STRING,)),                       # value -> string
    (value, (INT,)),                          # value -> int
    (value, (obj,)),                          # value -> obj
    (value, (LS, after_ls)),                  # value -> [ after_ls
    (after_ls, (RS)),                         # after_ls -> ]
    (after_ls, (arr_elems , RS)),             # after_ls  -> arr_elems ]
    (arr_elems, (arr_elem, after_elem)),      # arr_elems -> arr_elem after_elem
    (after_elem, (COMMA, arr_elems)),         # after_elem -> , arr_elems
    (after_elem, ()),                         # after_elem -> epsilon
    (arr_elem, (value,)),                     # arr_elem -> value
]



def main():
    analyze_grammar(grammar_recitation)
    print

    #
    # --- UNCOMMENT THE FOLLOWING LINES AS YOU PROCEED ---
    #
    analyze_grammar(grammar_json_4a)
    print
    analyze_grammar(grammar_json_4b)
    print
    analyze_grammar(grammar_json_4c)
    print
    analyze_grammar(grammar_json_6)
    print

    #
    # --- ADD MORE TEST CASES HERE ---
    #


if __name__ == '__main__':
    main()
