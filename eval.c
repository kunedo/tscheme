/*
 * Tscheme: A Tiny Scheme Interpreter
 * Copyright (c) 1995-2013 Takuo WATANABE (Tokyo Institute of Technology)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>
#include <stdbool.h>
#include <setjmp.h>

#include "tscheme.h"

#define SUBR_SNAME(s) STR_DATA(SYM_PNAME(SUBR_NAME(s)))

static SCM evaluate_list(SCM exps, SCM env);
static SCM extend_env(SCM alist, SCM vars, SCM vals);
static SCM extend_let_env(SCM alist, SCM let_list);
static SCM extend_let_star_env(SCM alist, SCM let_list);
static SCM extend_letrec_env(SCM alist, SCM let_list);
static SCM get_symcell(SCM alist, SCM sym);
static SCM get_symval(SCM alist, SCM sym);
static SCM set_symval(SCM alist, SCM sym, SCM val, int definep);


SCM evaluate(SCM exp, SCM env) {
    SCM e = exp, r = env;

#ifdef DEBUG
    fprintf (stderr, "evaluate: ");
    scm_write (exp, stderr_value, 0);
    putc ('\n', stderr);
#endif

    goto eval_one;

 eval_begin:
    if (IS_NULL(e))
        return unspecified_value;
    while (!IS_NULL(CDR(e))) {
        evaluate(CAR(e), r);
        e = CDR(e);
    }
    e = CAR(e);

 eval_one:
    switch (TYPE(e)) {
        /* self evaluating forms */
    case T_FIXNUM:
    case T_BOOLEAN:
    case T_CHARACTER:
    case T_NULL:
    case T_STRING:
    case T_EOF_VALUE:
        return e;
        
        /* variables */
    case T_SYMBOL:
        return get_symval(r, e);

        /* other forms */
    case T_PAIR:
        break;
    default:
        error0("invalid expression type.");
    }

    SCM op  = CAR(e), args = CDR(e);
    if (EQ(op, sym_quote))
        return(CAR(args));
    if (EQ(op, sym_begin)) {
        e = args;
        goto eval_begin;
    }
    else if (EQ(op, sym_let)) {
        if (IS_SYMBOL(FIRST(args))) {
            printf("sorry\n");
            return NIL;
        }
        else {
            e = CDR(args);
            r = extend_let_env(r, FIRST(args));
            goto eval_begin;
        }
    }
    else if (EQ(op, sym_let_star)) {
        e = CDR(args);
        r = extend_let_star_env(r, FIRST(args));
        goto eval_begin;
    }
    else if (EQ(op, sym_letrec)) {
        e = CDR(args);
        r = extend_letrec_env(r, FIRST(args));
        goto eval_begin;
    }
    else if (EQ(op, sym_if)) {
        if (EQ(evaluate (FIRST(args), r), boolean_false)) {
            e = CDDR(args);
            goto eval_begin;
        }
        else {
            e = SECOND(args);
            goto eval_one;
        }
    }
    else if (EQ(op, sym_cond)) {
        SCM tmp;
        while (!IS_NULL(args)) {
            if (EQ(CAAR(args), sym_else)) {
                e = CDAR(args);
                goto eval_begin;
            }
            else if (NEQ((tmp = evaluate (CAAR(args), r)), boolean_false)) {
                e = CDAR(args);
                if (IS_NULL(e)) return tmp;
                goto eval_begin;
            }
            args = CDR(args);
            if (!(IS_PAIR(args) || IS_NULL(args)))
                error0("cond: ill-formed expression");
        }
    }
    else if (EQ(op, sym_case)) {
        SCM tmp = evaluate(FIRST(args), r);
        args = CDR(args);
        while (!IS_NULL(args)) {
            if (EQ(CAAR(args), sym_else)) {
                e = CDAR(args);
                goto eval_begin;
            }
            else if (IS_PAIR(CAAR(args))) {
                if (memq(tmp, CAAR(args))) {
                    e = CDAR(args);
                    goto eval_begin;
                }
            }
            else
                error0("case: ill-formed expression");
            args = CDR(args);
            if (!(IS_PAIR(args) || IS_NULL(args)))
                error0("case: ill-formed expression");
        }
    }
    else if (EQ(op, sym_and)) {
        SCM tmp = boolean_true;
        while (!IS_NULL(args)) {
            tmp = evaluate (FIRST(args), r);
            if (EQ(tmp, boolean_false))
                return boolean_false;
            args = CDR(args);
        }
        return tmp;
    }
    else if (EQ(op, sym_or)) {
        while (!IS_NULL(args)) {
            SCM tmp = evaluate (FIRST(args), r);
            if (NEQ(tmp, boolean_false))
                return tmp;
            args = CDR(args);
        }
        return boolean_false;
    }
    else if (EQ(op, sym_lambda)) {
        return mk_closure(CAR(args), CDR(args), r);
    }
    else if (EQ(op, sym_set)) {
        if (IS_SYMBOL(CAR(args)))
            set_symval(r, CAR(args), evaluate(CADR(args), r), 0);
        else
            error0("set!: 1st arg is not a symbol.");
        return unspecified_value;
    }
    else if (EQ(op, sym_define)) {
        SCM first = CAR(args);
        switch (TYPE(first)) {
        case T_SYMBOL:
            set_symval(r, first, evaluate(CADR(args), r), 1);
            return unspecified_value;
        case T_PAIR: {
            SCM closure;
            NEWCELL(closure, T_CLOSURE);
            CLOSURE_ENV(closure) = r;
            CLOSURE_CODE(closure) = CONS(CDR(first), CDR(args));
            set_symval(r, CAR(first), closure, 1);
            return unspecified_value;
        }
        default:
            error0("define: wrong expression");
        }
    }
    op = evaluate(op, r);
    switch (TYPE(op)) {
    case T_FSUBR:
        return ((*(SCM (*)(SCM ,SCM))SUBR_FUN(op))(args, r));

    case T_SUBR0:
        check_nargs(SUBR_SNAME(op), args, 0, 0);
        return ((*SUBR_FUN(op))());

    case T_SUBR1:
        check_nargs(SUBR_SNAME(op), args, 1, 1);
        return ((*(SCM (*)(SCM))SUBR_FUN(op))
                (evaluate(FIRST(args),r)));

    case T_SUBR2:
        check_nargs(SUBR_SNAME(op), args, 2, 2);
        return ((*(SCM (*)(SCM, SCM))SUBR_FUN(op))
                (evaluate(FIRST(args),r),
                 evaluate(SECOND(args),r)));

    case T_SUBR3:
        check_nargs(SUBR_SNAME(op), args, 3, 3);
        return ((*(SCM (*)(SCM, SCM, SCM))SUBR_FUN(op))
                (evaluate(FIRST(args),r),
                 evaluate(SECOND(args),r),
                 evaluate(THIRD(args),r)));

    case T_SUBRN:
        if (!IS_NULL(args)) {
            if (IS_PAIR(args))
                args = evaluate_list(args, r);
            else
                error0 ("invalid expression.");
        }
        return ((*(SCM (*)(SCM))SUBR_FUN(op))(args));

    case T_CLOSURE:
        if (!IS_NULL(args)) {
            if (IS_PAIR(args))
                args = evaluate_list(args, r);
            else
                error0 ("invalid expression.");
        }
        e = CDR(CLOSURE_CODE(op));
        r = extend_env(CLOSURE_ENV(op), CAR(CLOSURE_CODE(op)), args);
        goto eval_begin;
    default:
        error0 ("unknown function type");
        return unspecified_value;
    }
} /* evaluate */

static SCM evaluate_list(SCM exps, SCM env) {
    SCM result, tmp;
    if (!IS_NULL(exps)) {
        result = CONS(evaluate(CAR(exps), env), NIL);
        tmp = result;
        while (true) {
            exps = CDR(exps);
            if (IS_NULL(exps))
                break;
            CDR(tmp) = CONS(evaluate(CAR(exps), env), NIL);
            tmp = CDR(tmp);
        }
    }
    else
        result = NIL;
    return result;
}


/* Environment */

static SCM extend_env(SCM alist, SCM vars, SCM vals) {
    while (!IS_NULL(vars)) {
        if (IS_SYMBOL(vars))
            return CONS(CONS(vars, vals), alist);
        else if (IS_PAIR(vars)) {
            alist = CONS(CONS(CAR(vars), CAR(vals)), alist);
            vars = CDR(vars);
            vals = CDR(vals);
        }
        else
            error0("extend_env: invalid arg.");
    }
    return alist;
}

static SCM extend_let_env(SCM alist, SCM let_list) {
    SCM org_alist = alist;
    while (!IS_NULL(let_list)) {
        SCM first = CAR(let_list);
        alist = CONS(CONS(CAR(first),
                          (IS_NULL(CDR(first)) ? unbound_value :
                           evaluate(CADR(first), org_alist))),
                     alist);
        let_list = CDR(let_list);
    }
    return alist;
}

static SCM extend_let_star_env(SCM alist, SCM let_list) {
    while (!IS_NULL(let_list)) {
        SCM first = CAR(let_list);
        alist = CONS(CONS(CAR(first),
                          (IS_NULL(CDR(first)) ? unbound_value :
                           evaluate(CADR(first), alist))),
                     alist);
        let_list = CDR(let_list);
    }
    return alist;
}

static SCM extend_letrec_env(SCM alist, SCM let_list) {
    SCM tmp = let_list;
    while (!IS_NULL(tmp)) {
        alist = CONS(CONS(CAAR(tmp), unbound_value), alist);
        tmp = CDR(tmp);
    }
    tmp = let_list;
    while (!IS_NULL(tmp)) {
        SCM first = CAR(tmp);
        set_symval(alist, FIRST(first), evaluate(SECOND(first), alist), 0);
        tmp = CDR(tmp);
    }
    return alist;
}

static SCM get_symcell(SCM alist, SCM sym) {
    while (!IS_NULL(alist)) {
        SCM cell = CAR(alist);
        if (EQ(sym, CAR(cell)))
            return cell;
        alist = CDR(alist);
    }
    return NIL;
}

static SCM get_symval(SCM alist, SCM sym) {
    SCM tmp;

    if (EQ((tmp = get_symcell(alist, sym)), NIL)) {
        if (EQ((tmp = SYM_VALUE(sym)), unbound_value))
            error1("ERROR: unbound variable %s.\n",
                   STR_DATA(SYM_PNAME(sym)));
        return SYM_VALUE(sym);
    }
    else {
        if (EQ(CDR(tmp), unbound_value))
            error1("ERROR: unbound variable %s.\n",
                   STR_DATA(SYM_PNAME(sym)));
        return CDR(tmp);
    }
}

static SCM set_symval(SCM alist, SCM sym, SCM val, int definep) {
    SCM tmp;
    if (EQ((tmp = get_symcell(alist, sym)), NIL)) {
        if (EQ((tmp = SYM_VALUE(sym)), unbound_value) && !definep)
            error1("ERROR: unbound variable %s.\n",
                   STR_DATA(SYM_PNAME(sym)));
        return SYM_VALUE(sym) = val;
    }
    else
        return CDR(tmp) = val;
}
