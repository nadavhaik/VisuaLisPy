from dataclasses import asdict

from scheme_types.types import *
from typing import TypeVar

T = TypeVar("T")
T1 = TypeVar("T1")
T2 = TypeVar("T2")


class NotSupportedError(Exception):
    pass


NodeType = str | list["NodeType"]


def make_node(parent: str, *children: NodeType) -> NodeType:
    return [parent] + list(reversed(children))


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
    return make_node("ScmVec", *[trace_sexp(sexp) for sexp in vec])


def trace_pair(pair: ScmPair):
    return make_node("ScmPair", trace_sexp(car(pair)), trace_sexp(cdr(pair)))


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


def trace_line_sexp(sexp: SExp):
    return {"ROOT": trace_sexp(sexp)}


def trace_var(var: ScmVar):
    return f"var {var.name}"


def trace_lambda_simple(_: LambdaSimple):
    return "Simple"


def trace_lambda_opt(opt: LambdaOpt):
    return f"Opt {opt.opt}"


def trace_lambda_kind(lambda_kind: LambdaKind):
    if isinstance(lambda_kind, LambdaSimple):
        return trace_lambda_simple(lambda_kind)
    if isinstance(lambda_kind, LambdaOpt):
        return trace_lambda_opt(lambda_kind)
    raise ValueError(f"Could not trace lambda kind: {lambda_kind}")


def trace_scm_const(const: ScmConst):
    return make_node("ScmConst", trace_sexp(const.sexpr))


def trace_var_get(var_get: ScmVarGet):
    return make_node("ScmVarGet", make_node(trace_var(var_get.var)))


def trace_scm_if(scm_if: ScmIf):
    return make_node("ScmIf", trace_exp(scm_if.test), trace_exp(scm_if.dit), trace_exp(scm_if.dif))


def trace_seq(seq: ScmSeq):
    return make_node("ScmSeq", *[trace_exp(exp) for exp in seq.exprs])


def trace_or(scm_or: ScmOr):
    return make_node("ScmOr", *[trace_exp(exp) for exp in scm_or.exprs])


def trace_var_set(var_set: ScmVarSet):
    return make_node("ScmVarSet", trace_var(var_set.var), trace_exp(var_set.val))


def trace_var_def(var_def: ScmVarDef):
    return make_node("ScmVarDef", trace_var(var_def.var), trace_exp(var_def.val))


def trace_lambda(scm_lambda: ScmLambda):
    return make_node("ScmLambda", trace_strings(scm_lambda.params), trace_lambda_kind(scm_lambda.kind),
                     trace_exp(scm_lambda.body))


def trace_applic(applic: ScmApplic):
    return make_node("ScmApplic", trace_exp(applic.applicative), trace_exprs(applic.params))


def trace_exprs(exprs: list[Expr]) -> NodeType:
    return make_node("OcamlList", *[trace_exp(exp) for exp in exprs])


def trace_strings(strings: list[str]) -> NodeType:
    return make_node("OcamlList", *[f'"{string}"' for string in strings])


def trace_exp(exp: Expr) -> NodeType:
    if isinstance(exp, ScmConst):
        return trace_scm_const(exp)
    if isinstance(exp, ScmVarGet):
        return trace_var_get(exp)
    if isinstance(exp, ScmIf):
        return trace_scm_if(exp)
    if isinstance(exp, ScmSeq):
        return trace_seq(exp)
    if isinstance(exp, ScmOr):
        return trace_or(exp)
    if isinstance(exp, ScmVarSet):
        return trace_var_set(exp)
    if isinstance(exp, ScmVarDef):
        return trace_var_def(exp)
    if isinstance(exp, ScmLambda):
        return trace_lambda(exp)
    if isinstance(exp, ScmApplic):
        return trace_applic(exp)

    raise ValueError(f"Could not trace exp: {exp}")


def trace_line_exp(exp: Expr):
    return {"ROOT": trace_exp(exp)}


def trace_enum(enum: Enum):
    return str(enum.name)


def trace_lexical_address(address: LexicalAddress):
    if isinstance(address, Free):
        return "Free"
    elif isinstance(address, Param):
        return f"Param({address.minor})"
    elif isinstance(address, Bound):
        return f"Bound({address.major},{address.minor})"


def trace_var_tag(var_tag: ScmVarTag):
    return make_node("var'", var_tag.name, trace_lexical_address(var_tag.lexical_address))


def trace_scm_const_tag(const: ScmConstTag):
    return make_node("ScmConst'", trace_sexp(const.sexpr))


def trace_var_get_tag(var_get_tag: ScmVarGetTag):
    return make_node("ScmVarGet'", trace_var_tag(var_get_tag.var))


def trace_scm_if_tag(scm_if: ScmIfTag):
    return make_node("ScmIf'", trace_expr_tag(scm_if.test), trace_expr_tag(scm_if.dit), trace_expr_tag(scm_if.dif))


def trace_seq_tag(seq: ScmSeqTag):
    return make_node("ScmSeq'", *[trace_expr_tag(exp) for exp in seq.exprs])


def trace_or_tag(scm_or: ScmOrTag):
    return make_node("ScmOr'", *[trace_expr_tag(exp) for exp in scm_or.exprs])


def trace_var_set_tag(var_set: ScmVarSetTag):
    return make_node("ScmVarSet'", trace_var_tag(var_set.var), trace_expr_tag(var_set.val))


def trace_var_def_tag(var_def: ScmVarDefTag):
    return make_node("ScmVarDef'", trace_var_tag(var_def.var), trace_expr_tag(var_def.val))


def trace_scm_box_tag(box_tag: ScmBoxTag):
    return make_node("ScmBox'", trace_var_tag(box_tag.var))


def trace_scm_box_get_tag(box_get_tag: ScmBoxGetTag):
    return make_node("ScmBoxGet'", trace_var_tag(box_get_tag.var))


def trace_scm_box_set_tag(box_set_tag: ScmBoxSetTag):
    return make_node("ScmBoxSet'", trace_var_tag(box_set_tag.var), trace_expr_tag(box_set_tag.val))


def trace_lambda_tag(scm_lambda: ScmLambdaTag):
    return make_node("ScmLambda'", trace_strings(scm_lambda.params), trace_lambda_kind(scm_lambda.kind),
                     trace_expr_tag(scm_lambda.body))


def trace_applic_tag(applic: ScmApplicTag):
    return make_node("ScmApplic'", trace_expr_tag(applic.applicative), trace_exprs_tag(applic.params),
                     trace_enum(applic.kind))


def trace_exprs_tag(exprs: list[ExprTag]) -> NodeType:
    return make_node("OcamlList", *[trace_expr_tag(exp) for exp in exprs])


def trace_expr_tag(expr_tag: ExprTag) -> NodeType:
    if isinstance(expr_tag, ScmConstTag):
        return trace_scm_const_tag(expr_tag)
    if isinstance(expr_tag, ScmVarGetTag):
        return trace_var_get_tag(expr_tag)
    if isinstance(expr_tag, ScmIfTag):
        return trace_scm_if_tag(expr_tag)
    if isinstance(expr_tag, ScmSeqTag):
        return trace_seq_tag(expr_tag)
    if isinstance(expr_tag, ScmOrTag):
        return trace_or_tag(expr_tag)
    if isinstance(expr_tag, ScmVarSetTag):
        return trace_var_set_tag(expr_tag)
    if isinstance(expr_tag, ScmVarDefTag):
        return trace_var_def_tag(expr_tag)
    if isinstance(expr_tag, ScmBoxTag):
        return trace_scm_box_tag(expr_tag)
    if isinstance(expr_tag, ScmBoxGetTag):
        return trace_scm_box_get_tag(expr_tag)
    if isinstance(expr_tag, ScmBoxSetTag):
        return trace_scm_box_set_tag(expr_tag)
    if isinstance(expr_tag, ScmLambdaTag):
        return trace_lambda_tag(expr_tag)
    if isinstance(expr_tag, ScmApplicTag):
        return trace_applic_tag(expr_tag)
    raise f"Unrecognized Expr': {expr_tag}"


def trace_line_exp_tag(exp_tag: ExprTag):
    return {"ROOT": trace_expr_tag(exp_tag)}
