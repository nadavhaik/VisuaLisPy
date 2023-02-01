from dataclasses import asdict

from scheme_types.types import ScmNil, ProperList, ScmVoid, ScmSymbol, ScmBoolean, ScmChar, ScmString, ScmNumber, \
    ScmFloat, ScmVector, SExp, ScmPair
from typing import TypeVar

T = TypeVar("T")
T1 = TypeVar("T1")
T2 = TypeVar("T2")


class NotSupportedError(Exception):
    pass


def car(exp: tuple[T1, T2]):
    return exp[0]


def cdr(exp: tuple[T1, T2]):
    return exp[1]


def is_proper_list(exp):
    if exp == ScmNil():
        return True
    if not isinstance(exp, tuple) or len(exp) != 2:
        return False

    return is_proper_list(cdr(exp))


def proper_list_to_python_list(proper_list: ProperList[T]) -> list[T]:
    if proper_list == ScmNil():
        return []

    return [car(proper_list)] + proper_list_to_python_list(cdr(proper_list))


# SExp = ScmVoid | ScmNil | ScmBoolean | ScmChar | ScmString | ScmSymbol | ScmNumber | ScmVector | ScmPair

def trace_symbol(symbol: ScmSymbol):
    return f"'{str(symbol)}"


def trace_void(_: ScmVoid):
    return "#<void>"


def trace_nil(_: ScmNil):
    return trace_symbol(ScmSymbol("()"))


def trace_bool(b: ScmBoolean):
    if b:
        return "#t"
    else:
        return "#f"


def trace_char(c: ScmChar):
    match c:
        case ScmChar('\n'):
            return "#\\newline"
        case ScmChar('\r'):
            return "#\\return"
        case ScmChar('\012'):
            return "#\\page"
        case ScmChar('\t'):
            return "#\\tab"
        case ScmChar(' '):
            return '#\\space'
        case default:
            return f'#\\{default}'


def trace_string(string: ScmString):
    return f'"{str(string)}"'


def trace_number(num: ScmNumber):
    if isinstance(num, ScmFloat):
        return num

    match asdict(num):
        case {"numerator": 0, "denominator": _}:
            return 0
        case {"numerator": numerator, "denominator": 1}:
            return numerator
        case {"numerator": numerator, "denominator": -1}:
            return -numerator
        case {"numerator": numerator, "denominator": denominator}:
            return f"{numerator}/{denominator}"
        case _:
            raise ValueError(f"Could not match number {num}")


def trace_vector(vec: ScmVector):
    return ["ScmVec"] + list(reversed([trace_sexp(sexp) for sexp in vec]))


def trace_pair(pair: ScmPair):
    # if is_proper_list(pair):
    #     return ["Proper List"] + trace_vector(proper_list_to_python_list(pair))
    # else:
    return ["ScmPair", trace_sexp(cdr(pair)), trace_sexp(car(pair))]


def trace_sexp(sexp: SExp):
    if isinstance(sexp, ScmVoid):
        return trace_void(sexp)
    if isinstance(sexp, ScmNil):
        return trace_nil(sexp)
    if isinstance(sexp, ScmBoolean):
        return trace_bool(sexp)
    if isinstance(sexp, ScmChar):
        return trace_char(sexp)
    if isinstance(sexp, ScmString):
        return trace_string(sexp)
    if isinstance(sexp, ScmSymbol):
        return trace_symbol(sexp)
    if isinstance(sexp, ScmNumber):
        return trace_number(sexp)
    if isinstance(sexp, list):
        return trace_vector(sexp)
    if isinstance(sexp, tuple):
        return trace_pair(sexp)

    raise ValueError(f"Could not trace: {sexp}")


def trace_line_vec(vec: ScmVector):
    if not isinstance(vec[0], ScmSymbol):
        raise NotSupportedError(f"Cannot trace vector {vec}")
    return {str(vec[0]): trace_sexp(vec)}


def trace_line_pair(p: ScmPair):
    if not isinstance(car(p), ScmSymbol):
        return {"ScmPair": trace_sexp(p)}
    else:
        return {str(car(p)): trace_sexp(p)}


def trace_line_sexp(sexp: SExp):
    if isinstance(sexp, list):
        return trace_line_vec(sexp)
    if isinstance(sexp, tuple):
        return trace_line_pair(sexp)

    raise NotSupportedError(f"Cannot trace line exp {sexp}")
