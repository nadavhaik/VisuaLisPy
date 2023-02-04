# Scheme interpreter written in Python
""" program --> Parser --> representation --> Execution --> output """

from abc import ABCMeta, abstractmethod
from enum import Enum
from scheme_interpreter.trace import trace_line_sexp, trace_line_exp, trace_line_exp_tag
from scheme_parser.parser import sexp_parse, sexps_parse, exprs_parse, exprs_tag_parse
from scheme_types.types import ScmSymbol


class ParsingMode(Enum):
    READER = 0
    TAG_PARSER = 1
    SEMANTIC_ANALYZER = 2


class Env(dict):
    # Env is a subclass of dict.
    # An environment is a dictionary of {'var': val} pairs with an outer Env.
    def __init__(self, parms=(), args=(), outer=None):
        super(Env, self).__init__()
        self.update(zip(parms, args))
        self.outer = outer

    def find(self, var):
        # Find the innermost Env where var appears.
        # Env.find finds the right environment according to lexical scoping rules.

        # return self if var in self else self.outer.find(var)
        if var in self:
            return self
        else:
            if self.outer is None:
                raise ValueError("Couldn't find: %r" % var)
            return self.outer.find(var)


SCM_NIL = ScmSymbol('()')


def make_scm_list(*args):
    res = SCM_NIL
    for x in reversed(args):
        res = x, res
    return res


def car(scm_list):
    return scm_list[0]


def cdr(scm_list):
    return scm_list[1]


def is_null(x):
    return x == SCM_NIL


def is_scm_list(x):
    if not isinstance(x, tuple):
        return x == SCM_NIL

    return is_scm_list(cdr(x))


def scm_list_to_python_list(scm_list):
    if is_null(scm_list):
        return []

    return [car(scm_list)] + scm_list_to_python_list(cdr(scm_list))


def python_list_to_scheme_list(python_list):
    return make_scm_list(*python_list)


def append_to_scm_list(list1, list2):
    return python_list_to_scheme_list(scm_list_to_python_list(list1) + scm_list_to_python_list(list2))


def add_globals(env):
    # Add Scheme standard procedures to an environment. This is only called once at the start of the program.
    import operator as op

    env.update({
        # art file allows for *args
        '+': op.add,
        '-': op.sub,
        '*': op.mul,
        '/': op.truediv,
        'not': op.not_,
        '>': op.gt,
        '<': op.lt,
        '>=': op.ge,
        '<=': op.le,
        '=': op.eq,
        'equal?': op.eq,  # true if variable values are equal
        'eq?': op.is_,  # true if variables point to the same object in memory
        'length': len,

        # Lambda forms (lambda expressions) have the same syntactic position as
        # expressions.  They are a shorthand to create anonymous functions; the
        # expression ``lambda arguments: expression`` yields a function object.

        'cons': lambda x, y: (x, y),
        'car': car,
        'cdr': cdr,
        'append': append_to_scm_list,  # this works for Scheme, I guess?
        # Well, Python concatenates strings this way...
        'list': make_scm_list,
        # list() -> new empty list
        # list(iterable) -> new list initialized from iterable's items
        # syntactic sugar. l = [] is the same as l = list()
        'list?': is_scm_list,
        'null?': is_null,
        'symbol?': lambda x: isinstance(x, ScmSymbol)
    })
    return env


class SExpression:
    def __init__(self, env):
        self.env = env

    __metaclass__ = ABCMeta

    @abstractmethod
    def eval(self):
        pass


"""          Symbol, Env classes           """

import json

global_env = add_globals(Env())

"""          eval           """


# Evaluate an expression x in an environment env.
# def eval_exp(x, env=global_env):
#     print"""
#
#
#     """
#
#     # variable reference
#     # if x is a str
#     if isa(x, Symbol):
#         print '%r is a Symbol ' % x,
#         print 'that evaluates to %r' % env.find(x)[x]
#         # .find is a method defined in the Env class
#         # env.find locates the proper environment
#         # the [x] returns the actual procedure/symbol definition
#         return env.find(x)[x]
#
#     # constant literal
#     elif not isa(x, list):
#         # print "%r is not a list; %r is of type %r" % (x, x, type(x))
#         return x
#
#     # (quote exp)
#     elif x[0] == 'quote':
#         print 'quote'
#         (_, exp) = x  # _ is a character we use as a variable.
#         return exp
#
#     # conditional (if test conseq alt)
#     elif x[0] == 'if':
#         (_, test, conseq, alt) = scm_list_to_python_list(x)
#         print 'if %r then %r else %r' % (test, conseq, alt)
#
#         if_statement = eval_exp(test, env)
#         if_eval = True if eval_exp(test, env) else False
#
#         print 'the if evaluates to %r, which is %r' % (if_statement, if_eval)
#         return eval_exp((conseq if eval_exp(test, env) else alt), env)
#         # value_when_true if condition else value_when_false
#
#     # assignment (set! var exp)
#     elif x[0] == 'set!':
#         (_, var, exp) = x
#         env.find(var)[var] = eval_exp(exp, env)  # recursively eval the expression
#
#     # (define var exp)
#     elif x[0] == 'define':
#         print x
#         (_, var, exp) = x
#         print 'define %r as %r' % (var, exp)
#         env[var] = eval_exp(exp, env)  # adds var to the global environment dictionary
#
#     # procedure (lambda (var*) exp)
#     elif x[0] == 'lambda':
#         print '%r is a lambda procedure' % x
#         (_, vars, body) = x
#         print 'vars', vars
#         print 'body', body
#
#         return LambdaExpression(env, vars, body).eval()
#
#
#     elif x[0] == 'let':
#         print '%r is a let expression' % x
#         (_, bindings, body) = x
#         print 'bindings', bindings
#         print 'body', body
#
#         return LetExpression(env, bindings, body).eval()
#
#
#
#     # sequencing (begin exp*)
#     elif x[0] == 'begin':
#         val = None
#         for exp in x[1:]:
#             val = eval_exp(exp, env)
#         return val  # val will keep being reassigned, so we only return the last val?
#
#     else:
#         print 'procedure call!'
#         return eval_procedure_call(x, env)
#
#
# # alias
#
# """          parse, read, user interaction           """
#
#
# # Parsing is traditionally separated into two parts: lexical analysis,
# # in which the input character string is broken up into a sequence of tokens,
# # and syntactic analysis, in which the tokens are assembled into an internal representation.
# # The Lispy tokens are parentheses, symbols (such as set! or x), and numbers (such as 2).


def format_json(user_input, parsing_mode: ParsingMode = ParsingMode.READER):
    # prepare tracing structures
    # global expression_trace
    global_env_for_json_conversion = {}

    # list of lines of code
    user_input_clean = user_input.strip()
    user_input_lines = user_input_clean.split('\r\n')
    # do I care about unicode? It still works...but print statements are kind of ugly.

    # remove white space from each line
    for i in range(len(user_input_lines)):
        user_input_lines[i] = user_input_lines[i].strip()

    # # evaluate each line of code
    # for line in user_input_lines:
    #     val = eval_exp(parse(line))

    # for every entry in the global_env dictionary, convert it for JSON
    for key, _ in global_env.items():
        global_env_for_json_conversion[key] = str(global_env[key])

    # prepare JSON output object
    json_output = {
        # code is a list of user_input_lines as strings
        "code": user_input_lines,
        # trace is a list of dictionaries
        "trace": []
    }
    if parsing_mode == ParsingMode.READER:
        expressions = sexps_parse(user_input)
        expression_trace = [trace_line_sexp(sexp) for sexp in expressions]
    elif parsing_mode == ParsingMode.TAG_PARSER:
        expressions = exprs_parse(user_input)
        expression_trace = [trace_line_exp(exp) for exp in expressions]
    elif parsing_mode == ParsingMode.SEMANTIC_ANALYZER:
        expressions = exprs_tag_parse(user_input)
        expression_trace = [trace_line_exp_tag(exp) for exp in expressions]
    else:
        raise ValueError(f"Unknown parsing mode: {parsing_mode}")

    json_output["trace"].append(dict(global_env=global_env_for_json_conversion))
    json_output["trace"].append(dict(expression_trace=expression_trace))
    json_object = json.dumps(json_output, indent=5)

    # print to_string(val)
    # print json_object
    return json_object


def main():
    """In case we need this for something"""
    pass


if __name__ == "__main__":
    pass
    # uncomment repl() for troubleshooting in the terminal
    # repl()

    # s = '(define area (lambda (r) (* 3.141592653 (* r r))))'
    # print format_json(s)
