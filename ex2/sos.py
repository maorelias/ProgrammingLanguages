"""
Structural Operational Semantics (SOS) of statements

Implemented according to
http://www.daimi.au.dk/~bra8130/Wiley_book/wiley.pdf (the book).

"""

from while_ast import *
from expr import *

def sos(S, s):
    """
    Structural Operational Semantics (SOS) of statements

    Returns one of the following:
        s' such that <S, s> ==> s'
        (S', s') such that <S, s> ==> <S', s'>

    Implements Table 2.2 from the book.

    --- MODIFY THIS FUNCTION QUESTIONS 1, 3 ---

    """

    if type(S) is Skip:
        return s

    elif type(S) is Assign:
        sp = s.copy()
        sp[S.lhs] = eval_arith_expr(S.rhs, s)
        return sp

    elif type(S) is Comp:
        gamma = sos(S.S1, s)
        if type(gamma) is tuple:
            S1p, sp = gamma
            return (Comp(S1p, S.S2), sp)
        else:
            sp = gamma
            return (S.S2, sp)

    elif type(S) is If and eval_bool_expr(S.b, s) == tt:
        return (S.S1, s)

    elif type(S) is If and eval_bool_expr(S.b, s) == ff:
        return (S.S2, s)

    elif type(S) is While:
        return (If(S.b, Comp(S.S, While(S.b, S.S)), Skip()), s)

    elif type(S) is Repeat:
        return (Comp(S.S, If(S.b, Skip(), S)),s)

    else:
        assert False # Error


def run_sos(S, s):
    """
    Iteratively apply sos until a final state is reached.
    """

    gamma = (S, s)
    while type(gamma) is tuple:
        S, s = gamma
        print '<{}, {}> ==>\n'.format(S, s)
        gamma = sos(S, s)
    print gamma
    return gamma


if __name__ == '__main__':
    prog = Comp(Assign('y', ALit(1)),
                While(Not(Eq(Var('x'), ALit(1))),
                      Comp(Assign('y', Times(Var('y'), Var('x'))),
                           Assign('x', Minus(Var('x'), ALit(1))))))

    run_sos(prog, {'x': 5})

    euclid_prog = Comp(Assign('a',ALit(84)),
                       Comp(Assign('b',ALit(30)),
                            While(Not(Eq(Var('b'), ALit(0))),
                                  Comp(Assign('t', Var('b')),
                                       Comp(Assign('b', Mod(Var('a'),Var('b'))),
                                            Assign('a', Var('t')))))))

    run_sos(euclid_prog, {})

    
    repeat_prog = Repeat(Comp(Assign('a',Times(Var('a'),ALit(4))),
                       Assign('b',Minus(Var('b'),ALit(1)))),
                  LE(Var('b'),ALit(0)))

    run_sos(repeat_prog, {'a': 6, 'b' : 2})
                                            
                            

    #
    # --- ADD MORE TESTS HERE ---
    #
