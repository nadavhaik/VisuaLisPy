# Scheme interpreter written in Python
""" program --> Parser --> representation --> Execution --> output """

from abc import ABCMeta, abstractmethod
from typing import TypeVar, Generic, Callable, Optional
from dataclasses import dataclass
import functools
import operator

from scheme_interpreter.trace import trace_line_sexp
from scheme_parser.parser import sexp_parse, sexps_parse
from scheme_types.types import ScmSymbol


# def raise_(e):
#     raise e
#
# def fold_left(func, xs, acc):
#   return functools.reduce(func, xs, acc)
#
# def fold_right(func, xs, acc):
#     return fold_left(func, acc, reversed(xs))
#
# T = TypeVar("T")
#
# isa = isinstance  # isinstance(9, int) --> True, isinstance(9, str) --> False
# @dataclass
# class ParsingResult(Generic[T]):
#     index_from: int
#     index_to: int
#     found: T
#
#     def __eq__(self, other):
#         if type(self) != type(other):
#             return False
#         return self.index_from == other.index_from and self.index_to == other.index_to and self.found == other.found
#
# Parser = Callable[[str, int], ParsingResult[T]]
#
# class NotYetImplementedError(Exception):
#     pass
#
# class NoMatchError(Exception):
#     pass
#
# def const(pred: Callable[[str], bool]) -> Parser[str]:
#     def parser(string: str, index: int):
#         if index < len(string) and pred(string[index]):
#             return ParsingResult(index, index+1, string[index])
#         raise NoMatchError()
#
#     return parser
#
# T1 = TypeVar("T1")
# T2 = TypeVar("T2")
#
# def caten(p1: Parser[T1], p2: Parser[T2]) -> Parser[tuple[T1, T2]]:
#     def parser(string: str, index_from: int):
#         res1 = p1(string, index_from)
#         res2 = p2(string, res1.index_to)
#
#         return ParsingResult(index_from, res2.index_to, (res1.found, res2.found))
#
#     return parser
#
# def caten_many(*parsers: Parser[T]) -> Parser[list[T]]:
#     def caten_parser(string: str, index_from: int):
#         results: list[T] = []
#         next_index_from = index_from
#         for p in parsers:
#             next_res = p(string, next_index_from)
#             next_index_from = next_res.index_to
#             results.append(next_res)
#
#         return ParsingResult(index_from, next_index_from, results)
#
#     return caten_parser
#
# def caten_list(parsers: list[Parser[T]]) -> Parser[list[T]]:
#     return caten_many(*parsers)
#
# def pack(p: Parser[T1], f: Callable[[T1], T2]) -> Parser[T2]:
#     def parser(string: str, index_from: int):
#         res = p(string, index_from)
#         return ParsingResult(res.index_from, res.index_to, f(res.found))
#
#     return parser
#
# def nt_epsilon(_: str, index_from: int):
#     return ParsingResult(index_from, index_from, [])
#
# nt_any = const(lambda _: True)
#
#
# def caten_lists(parsers: list[Parser[T]]):
#     return fold_right(lambda nt1, nt2: pack(caten(nt1, nt2), lambda e, es: [e] + es), parsers, nt_epsilon)
#
# def disj(*parsers: Parser[T]) -> Parser[T]:
#     def new_parser(string: str, index_from: int):
#         for p in parsers:
#             try:
#                 return p(string, index_from)
#             except NoMatchError:
#                 pass
#         raise NoMatchError()
#
#     return new_parser
#
# def nt_none(string: str, index_from: int):
#     raise NoMatchError()
#
# def disj_list(parsers: list[Parser[T]]):
#     return disj(*parsers)
#
# def delayed(thunk: Callable[[], Parser[T]]) -> Parser[T1]:
#     return lambda string, index_from: thunk()(string, index_from)
#
# def nt_end_of_input(string: str, index_from: int):
#     if index_from < len(string):
#         raise NoMatchError()
#     return ParsingResult(index_from, index_from, [])
#
# def star(p: Parser[T]) -> Parser[list[T]]:
#     def star_parser(string: str, index_from: int):
#         next_index_from = index_from
#         results = []
#         while True:
#             try:
#                 next_res = p(string, next_index_from)
#                 if next_res.index_to == next_index_from:
#                     break
#                 results.append(next_res)
#                 next_index_from = next_res.index_to
#             except NoMatchError:
#                 break
#
#         return ParsingResult(index_from, next_index_from, results)
#
#     return star_parser
#
# def plus(p: Parser[T]) -> Parser[list[T]]:
#     return pack(caten(p, star(p)), lambda first, rest: [first] + rest)
#
# def power(p: Parser[T], n: int) -> Parser[list[T]]:
#     res = nt_epsilon
#     for _ in range(n):
#         res = pack(caten(p, res), lambda first, rest: [first] + rest)
#
#     return res
#
# def nonify(p: Parser[T]) -> Parser[None]:
#     return pack(p, lambda _: None)
#
# def maybe(p: Parser[T]) -> Parser[Optional[T]]:
#     def maybe_parser(string: str, index_from: int):
#         try:
#             return p(string, index_from)
#         except NoMatchError:
#             return ParsingResult(index_from, index_from, None)
#
#     return maybe_parser
#
# def diff(p1: Parser[T1], p2: Parser[T2]) -> Parser[T1]:
#     def diff_parser(string: str, index: int):
#         res1 = p1(string, index)
#         res2 = maybe(p2)(string, index)
#         if res2.found is not None:
#             raise NoMatchError()
#
#         return res1
#
#     return diff_parser
#
# def followed_by(p1: Parser[T1], p2: Parser[T2]) -> Parser[T1]:
#     return pack(caten(p1, p2), lambda res: res[0])
#
# def not_followed_by(p1: Parser[T1], p2: Parser[T2]) -> Parser[T1]:
#     def not_followed_by_parser(string: str, index: int):
#         res1 = p1(string, index)
#         res2 = maybe(p2)(string, res1.index_to)
#
#         if res2.found is not None:
#             raise NoMatchError()
#
#         return res1
#
#     return not_followed_by_parser
#
# def make_char(equator: Callable[[str, str], bool], ch1: str):
#     return const(lambda ch2: equator(ch1, ch2))
#
# def char(c):
#     return make_char(lambda c1, c2: c1 == c2, c)
#
# def char_insensitive(c):
#     return make_char(lambda c1, c2: c1.lower() == c2.lower(), c)
#
# def make_word(word: str) -> Parser[str]:
#     def word_parser(string: str, index_from: int):
#         index_to = index_from + len(word)
#         if string[index_from:index_to] == word:
#             return ParsingResult(index_from, index_to, word)
#         raise NoMatchError()
#
#     return word_parser
#
# def make_insensitive_word(word: str):
#     def word_parser(string: str, index_from: int):
#         index_to = index_from + len(word)
#         if string[index_from:index_to].lower() == word.lower():
#             return ParsingResult(index_from, index_to, string[index_from:index_to])
#         raise NoMatchError()
#
#     return word_parser
#
# def non_escaped(c: str) -> Parser[str]:
#     def non_escaped_parser(string: str, index: int):
#         if index < 2 or string[index - 2] != "#" or string[index-1] != '\\':
#             return make_word(c)(string, index)
#         raise NoMatchError()
#     return non_escaped_parser
#
# def complement(p: Parser[T]):
#     return diff(nt_any, p)
#
# ScmNumber = int | float
#
#
#
# nt_whitespace = const(lambda c: ascii(c) <= ascii(' '))
# nt_end_of_line_or_file = nonify(disj(char('\n'), nt_end_of_input))
#
#
# def nt_sexp(string, index_from) -> ParsingResult[SExp]:
#     pass
# def nt_line_comment(string: str, index_from: int) -> ParsingResult[None]:
#     nt1 = char(';')
#     nt2 = diff(nt_any, nt_end_of_line_or_file)
#     nt2 = star(nt2)
#     nt1 = caten(nt1, nt2)
#     nt1 = caten(nt1, nt_end_of_line_or_file)
#     nt1 = nonify(nt1)
#
#     return nt1(string, index_from)
#
# nt_sexp_comment_symbol = nonify(make_word("#;"))
#
# nt_sexpr_comment = nonify(caten(nt_sexp_comment_symbol, nt_sexp))
# nt_left_curly_bracket = non_escaped("{")
# nt_right_curly_bracket = non_escaped("}")
# nt_legit_paired_comment_chars = diff(complement(nt_left_curly_bracket), nt_right_curly_bracket)
#
# nt_symbol: Parser[SExp] = raise_(NotYetImplementedError())
#
# def nt_paired_comment(string: str, index: int) -> ParsingResult[None]:
#     return nonify(caten_many(nt_left_curly_bracket, star(disj(nt_symbol, nt_paired_comment,
#                     nt_legit_paired_comment_chars)), nt_right_curly_bracket))(string, index)
#
# nt_comment = disj(nt_line_comment, nt_paired_comment, nt_sexpr_comment)


class Symbol(str):
    pass



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
        'symbol?': lambda x: isinstance(x, Symbol)
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



def tokenize(s):
    # convert a string into a list of tokens
    # add white space in between parentheses and split on white space
    return s.replace('(', ' ( ').replace(')', ' ) ').split()



def atom(token):
    # numbers become numbers
    try:
        return int(token)
    except ValueError:
        try:
            return float(token)
        # every other token is a symbol
        except ValueError:
            return Symbol(token)




def repl():
    # prompt-read-eval-print loop
    while True:
        user_input = input('lis.py > ')
        # able to push enter infinitely
        if user_input:
            format_json(user_input)


def format_json(user_input):
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


    expressions = sexps_parse(user_input)
    expression_trace = [trace_line_sexp(sexp) for sexp in expressions]


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
