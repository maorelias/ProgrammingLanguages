"""
Natural Operational Semantics (NOS) of statements, also keeping the derivation tree
"""

from while_ast import *
from expr import *

def nos_tree(S, s):
    """
    Natural Operational Semantics (NOS) of statements

    Returns (s', tree) such that <S, s> --> s' and tree is the derivation tree

    Implements Table 2.1 from the book.

    --- MODIFY THIS FUNCTION QUESTIONS 1, 3 ---
    """

    if type(S) is Skip:
        rule = 'skip'
        premises = ()
        post_state = s

    elif type(S) is Assign:
        rule = 'ass'
        sp = s.copy()
        sp[S.lhs] = eval_arith_expr(S.rhs, s)
        premises = ()
        post_state = sp

    elif type(S) is Comp:
        rule = 'comp'
        sp, t1 = nos_tree(S.S1, s)
        spp, t2 = nos_tree(S.S2, sp)
        premises = (t1, t2)
        post_state = spp

    elif type(S) is If and eval_bool_expr(S.b, s) == tt:
        rule = 'if_tt'
        sp, t = nos_tree(S.S1, s)
        premises = (t, )
        post_state = sp

    elif type(S) is If and eval_bool_expr(S.b, s) == ff:
        rule = 'if_ff'
        sp, t = nos_tree(S.S2, s)
        premises = (t, )
        post_state = sp

    elif type(S) is While and eval_bool_expr(S.b, s) == tt:
        rule = 'while_tt'
        sp, t1 = nos_tree(S.S, s)
        spp, t2 = nos_tree(While(S.b, S.S), sp)
        premises = (t1, t2)
        post_state = spp

    elif type(S) is While and eval_bool_expr(S.b, s) == ff:
        rule = 'while_ff'
        premises = ()
        post_state = s

    elif type(S) is Repeat:
        sp, t1 = nos_tree(S.S, s)
        if(eval_bool_expr(S.b, sp) == tt):
            rule = 'repeat_tt'
            post_state = sp
            premises = (t1,)
        else:
            rule = 'repeat_ff'
            spp, t2 = nos_tree(S,sp)
            post_state = spp
            premises = (t1,t2)
        

    else:
        assert False # Error

    return post_state, ('<{}, {}> --> {}\n[{}]'.format(S, s, post_state, rule), premises)


if __name__ == '__main__':
    from tree_to_dot import view_tree

    prog = Comp(Assign('y', ALit(1)),
                While(Not(Eq(Var('x'), ALit(1))),
                      Comp(Assign('y', Times(Var('y'), Var('x'))),
                           Assign('x', Minus(Var('x'), ALit(1))))))

    s, tree = nos_tree(prog, {'x': 5})
    print s
    print
    print tree
    print
    view_tree(tree)

    euclid_prog = Comp(Assign('a',ALit(84)),
                       Comp(Assign('b',ALit(30)),
                            While(Not(Eq(Var('b'), ALit(0))),
                                  Comp(Assign('t', Var('b')),
                                       Comp(Assign('b', Mod(Var('a'),Var('b'))),
                                            Assign('a', Var('t')))))))

    
    s, tree = nos_tree(euclid_prog, {})
    print s
    print
    print tree
    print
    view_tree(tree)

    repeat_prog = Repeat(Comp(Assign('a',Times(Var('a'),ALit(4))),
                              Assign('b',Minus(Var('b'),ALit(1)))),
                         LE(Var('b'),ALit(0)))

    s, tree = nos_tree(repeat_prog, {'a':6, 'b':2})
    print s
    print
    print tree
    print
    view_tree(tree)


    #
    # --- ADD MORE TESTS HERE ---
    #
