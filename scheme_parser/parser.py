from scheme_parser.ocaml_bridge import OcamlBridge
from scheme_types.types import *


def constructor_name(ocaml_val):
    return ocaml_val._constructor_name


def raw_parse_sexp(string: str):
    return OcamlBridge().sexp_parser(string, 0)


def raw_parse_sexps(string: str):
    return OcamlBridge().sexprs_parser(string, 0)


def raw_parse_expr(string: str):
    return OcamlBridge().exp_parser(string, 0)


def raw_parse_exprs(string: str):
    return OcamlBridge().exprs_parser(string, 0)


def raw_parser_expr_tag(string: str):
    return OcamlBridge().exp_tag_parser(string, 0)


def raw_parser_exprs_tag(string: str):
    return OcamlBridge().exps_tag_parser(string, 0)


def convert_number(ocaml_scm_number) -> ScmNumber:
    match constructor_name(ocaml_scm_number):
        case "ScmRational":
            return ScmRational(ocaml_scm_number.f0[0], ocaml_scm_number.f0[1])
        case "ScmReal":
            float_val: float = ocaml_scm_number.f0
            assert isinstance(float_val, float), f"Not a float: {float_val}"
            return float_val
        case [_]:
            raise ValueError(f"Could not recognize number: {ocaml_scm_number}")


def convert_boolean(ocaml_boolean) -> ScmBoolean:
    bool_val: bool = ocaml_boolean.f0
    assert isinstance(bool_val, ScmBoolean), f"Not a bool: {bool_val}"
    return bool_val


def convert_char(ocaml_char) -> ScmChar:
    char_val: str = ocaml_char.f0
    assert isinstance(char_val, str), f"Not a string: {ocaml_char}"
    return ScmChar(char_val)


def convert_string(ocaml_string) -> ScmString:
    str_val: str = ocaml_string.f0
    assert isinstance(str_val, str), f"Not a string: {ocaml_string}"
    return ScmString(str_val)


def convert_symbol(ocaml_symbol) -> ScmSymbol:
    symbol_val: str = ocaml_symbol.f0
    assert isinstance(symbol_val, str), f"Not a string: {ocaml_symbol}"
    return ScmSymbol(symbol_val)


def convert_vector(ocaml_vector) -> ScmVector:
    sexprs = ocaml_vector.f0
    return [convert_sexp(sexpr) for sexpr in sexprs]


def convert_pair(ocaml_pair) -> ScmPair:
    car = ocaml_pair.f0[0]
    cdr = ocaml_pair.f0[1]

    return convert_sexp(car), convert_sexp(cdr)


def convert_sexp(ocaml_sexp) -> SExp:
    match constructor_name(ocaml_sexp):
        case 'ScmVoid':
            return ScmVoid()
        case 'ScmNil':
            return ScmNil()
        case 'ScmBoolean':
            return convert_boolean(ocaml_sexp)
        case 'ScmNumber':
            return convert_number(ocaml_sexp.f0)
        case 'ScmChar':
            return convert_char(ocaml_sexp)
        case 'ScmString':
            return convert_string(ocaml_sexp)
        case 'ScmSymbol':
            return convert_symbol(ocaml_sexp)
        case 'ScmVector':
            return convert_vector(ocaml_sexp)
        case 'ScmPair':
            return convert_pair(ocaml_sexp)
        case [_]:
            raise ValueError(f"Could not recognize sexp {ocaml_sexp}")


# ScmVarGet | ScmIf | ScmSeq | ScmOr | ScmVarSet | ScmVarDef | ScmLambda | ScmApplic

def convert_scm_var(ocaml_scm_var) -> ScmVar:
    return ScmVar(ocaml_scm_var.f0)


def convert_lambda_opt(ocaml_lambda_opt) -> LambdaOpt:
    return LambdaOpt(ocaml_lambda_opt.f0)


def convert_lambda_kind(ocaml_lambda_kind) -> LambdaKind:
    match constructor_name(ocaml_lambda_kind):
        case 'Simple':
            return LambdaSimple()
        case 'Opt':
            return convert_lambda_opt(ocaml_lambda_kind)
        case _:
            raise f"Unrecognized lambda kind: {ocaml_lambda_kind}"


def convert_scm_const(ocaml_scm_const) -> ScmConst:
    return ScmConst(convert_sexp(ocaml_scm_const.f0))


def convert_var_get(ocaml_var_get) -> ScmVarGet:
    return ScmVarGet(convert_scm_var(ocaml_var_get.f0))


def convert_scm_if(ocaml_scm_if) -> ScmIf:
    return ScmIf(convert_expr(ocaml_scm_if.f0), convert_expr(ocaml_scm_if.f1), convert_expr(ocaml_scm_if.f2))


def convert_scm_seq(ocaml_scm_seq) -> ScmSeq:
    exprs = ocaml_scm_seq.f0
    return ScmSeq([convert_expr(exp) for exp in exprs])


def convert_scm_or(ocaml_scm_or) -> ScmOr:
    exprs = ocaml_scm_or.f0
    return ScmOr([convert_expr(exp) for exp in exprs])


def convert_scm_var_set(ocaml_var_set) -> ScmVarSet:
    return ScmVarSet(convert_scm_var(ocaml_var_set.f0), convert_expr(ocaml_var_set.f1))


def convert_scm_var_def(ocaml_var_def) -> ScmVarDef:
    return ScmVarDef(convert_scm_var(ocaml_var_def.f0), convert_expr(ocaml_var_def.f1))


def convert_scm_lambda(ocaml_scm_lambda) -> ScmLambda:
    return ScmLambda(list(ocaml_scm_lambda.f0), convert_lambda_kind(ocaml_scm_lambda.f1),
                     convert_expr(ocaml_scm_lambda.f2))


def convert_scm_applic(ocaml_scm_applic) -> ScmApplic:
    return ScmApplic(convert_expr(ocaml_scm_applic.f0), [convert_expr(param) for param in ocaml_scm_applic.f1])


def convert_expr(ocaml_exp) -> Expr:
    match constructor_name(ocaml_exp):
        case "ScmConst":
            return convert_scm_const(ocaml_exp)
        case "ScmVarGet":
            return convert_var_get(ocaml_exp)
        case "ScmIf":
            return convert_scm_if(ocaml_exp)
        case "ScmSeq":
            return convert_scm_seq(ocaml_exp)
        case "ScmOr":
            return convert_scm_or(ocaml_exp)
        case "ScmVarSet":
            return convert_scm_var_set(ocaml_exp)
        case "ScmVarDef":
            return convert_scm_var_def(ocaml_exp)
        case "ScmLambda":
            return convert_scm_lambda(ocaml_exp)
        case "ScmApplic":
            return convert_scm_applic(ocaml_exp)
        case _:
            raise ValueError(f"Could not recognize expr {ocaml_exp}")


def convert_app_kind(ocaml_app_kind) -> AppKind:
    return AppKind[constructor_name(ocaml_app_kind)]


def convert_lexical_address(ocaml_lexical_address) -> LexicalAddress:
    match constructor_name(ocaml_lexical_address):
        case 'Free':
            return Free()
        case 'Param':
            return Param(ocaml_lexical_address.f0)
        case 'Bound':
            return Bound(ocaml_lexical_address.f0[0], ocaml_lexical_address.f0[1])
        case _:
            raise f"Unrecognized address: {ocaml_lexical_address}"


# type var' = Var' of string * lexical_address;;
def convert_scm_var_tag(ocaml_scm_var) -> ScmVarTag:
    return ScmVarTag(ocaml_scm_var.f0, convert_lexical_address(ocaml_scm_var.f1))


convert_scm_const_tag = convert_scm_const


def convert_var_get_tag(ocaml_var_get_tag) -> ScmVarGetTag:
    return ScmVarGetTag(convert_scm_var_tag(ocaml_var_get_tag.f0))


def convert_scm_if_tag(ocaml_scm_if_tag) -> ScmIfTag:
    #   | ScmIf' of expr' * expr' * expr'
    return ScmIfTag(convert_expr_tag(ocaml_scm_if_tag.f0), convert_expr_tag(ocaml_scm_if_tag.f1),
                    convert_expr_tag(ocaml_scm_if_tag.f2))


def convert_scm_seq_tag(ocaml_scm_seq_tag) -> ScmSeqTag:
    #   | ScmSeq' of expr' list
    exprs = ocaml_scm_seq_tag.f0
    return ScmSeqTag([convert_expr_tag(exp) for exp in exprs])


def convert_scm_or_tag(ocaml_scm_or_tag) -> ScmOrTag:
    #   | ScmOr' of expr' list
    exprs = ocaml_scm_or_tag.f0
    return ScmOrTag([convert_expr_tag(exp) for exp in exprs])


def convert_scm_var_set_tag(ocaml_var_set_tag) -> ScmVarSetTag:
    #   | ScmVarSet' of var' * expr'
    return ScmVarSetTag(convert_scm_var_tag(ocaml_var_set_tag.f0), convert_expr_tag(ocaml_var_set_tag.f1))


def convert_scm_var_def_tag(ocaml_var_def_tag) -> ScmVarDefTag:
    #   | ScmVarDef' of var' * expr'
    return ScmVarDefTag(convert_scm_var_tag(ocaml_var_def_tag.f0), convert_expr_tag(ocaml_var_def_tag.f1))


def convert_scm_box_tag(ocaml_scm_box_tag) -> ScmBoxTag:
    #   | ScmBox' of var'
    return ScmBoxTag(convert_scm_var_tag(ocaml_scm_box_tag.f0))


def convert_scm_box_get_tag(ocaml_scm_box_get_tag) -> ScmBoxGetTag:
    #   | ScmBoxGet' of var'
    return ScmBoxGetTag(convert_scm_var_tag(ocaml_scm_box_get_tag.f0))


def convert_scm_box_set_tag(ocaml_scm_box_set_tag) -> ScmBoxSetTag:
    #   | ScmBoxSet' of var' * expr'
    return ScmBoxSetTag(ocaml_scm_box_set_tag.f0, ocaml_scm_box_set_tag.f1)


def convert_scm_lambda_tag(ocaml_scm_lambda_tag) -> ScmLambdaTag:
    #   | ScmLambda' of string list * lambda_kind * expr'
    return ScmLambdaTag(list(ocaml_scm_lambda_tag.f0), convert_lambda_kind(ocaml_scm_lambda_tag.f1),
                        convert_expr_tag(ocaml_scm_lambda_tag.f2))


def convert_scm_applic_tag(ocaml_scm_applic) -> ScmApplicTag:
    #   | ScmApplic' of expr' * expr' list * app_kind;;
    return ScmApplicTag(convert_expr_tag(ocaml_scm_applic.f0),
                        [convert_expr_tag(param) for param in ocaml_scm_applic.f1],
                        convert_app_kind(ocaml_scm_applic.f2))


def convert_expr_tag(ocaml_exp) -> ExprTag:
    match constructor_name(ocaml_exp):
        case "ScmConst'":
            return convert_scm_const_tag(ocaml_exp)
        case "ScmVarGet'":
            return convert_var_get_tag(ocaml_exp)
        case "ScmIf'":
            return convert_scm_if_tag(ocaml_exp)
        case "ScmSeq'":
            return convert_scm_seq_tag(ocaml_exp)
        case "ScmOr'":
            return convert_scm_or_tag(ocaml_exp)
        case "ScmVarSet'":
            return convert_scm_var_set_tag(ocaml_exp)
        case "ScmVarDef'":
            return convert_scm_var_def_tag(ocaml_exp)
        case "ScmBox'":
            return convert_scm_box_tag(ocaml_exp)
        case "ScmBoxGet'":
            return convert_scm_box_get_tag(ocaml_exp)
        case "ScmBoxSet'":
            return convert_scm_box_set_tag(ocaml_exp)
        case "ScmLambda'":
            return convert_scm_lambda_tag(ocaml_exp)
        case "ScmApplic'":
            return convert_scm_applic_tag(ocaml_exp)
        case _:
            raise ValueError(
                f"Could not recognize expr {ocaml_exp} with constructor name: {constructor_name(ocaml_exp)}")


def sexp_parse(string: str) -> SExp:
    ocaml_result = raw_parse_sexp(string)
    return convert_sexp(ocaml_result.found)


def sexps_parse(string: str) -> list[SExp]:
    results = raw_parse_sexps(string)
    return [convert_sexp(res) for res in results.found]


def expr_parse(string: str) -> Expr:
    ocaml_result = raw_parse_expr(string)
    return convert_expr(ocaml_result.found)


def exprs_parse(string: str) -> list[Expr]:
    results = raw_parse_exprs(string)
    return [convert_expr(res) for res in results.found]


def expr_tag_parse(string: str) -> ExprTag:
    result = raw_parser_expr_tag(string)
    return convert_expr_tag(result.found)


def exprs_tag_parse(string: str) -> list[ExprTag]:
    results = raw_parser_exprs_tag(string)
    return [convert_expr_tag(res) for res in results.found]

