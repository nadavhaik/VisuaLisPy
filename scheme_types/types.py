from dataclasses import dataclass
from typing import TypeVar, Generic, Callable
from enum import Enum

from scheme_types.char import Char

T = TypeVar("T")


@dataclass(eq=True)
class ParsingResult(Generic[T]):
    index_from: int
    index_to: int
    found: T


Parser = Callable[[str, int], ParsingResult[T]]


@dataclass(eq=True)
class ScmVoid:
    def __repr__(self):
        return "#<void>"


@dataclass
class ScmNil:
    def __repr__(self):
        return "'()"

    def __eq__(self, other):
        return type(self) == type(other)


ProperList = tuple[T, "ProperList"] | ScmNil

ScmBoolean = bool


@dataclass
class ScmRational:
    numerator: int
    denominator: int


ScmFloat = float

ScmNumber = ScmRational | ScmFloat
ScmChar = Char


class ScmString(str):
    def __init__(self, string):
        str.__init__(string)

    def __eq__(self, other):
        if type(self) != type(other):
            return False
        return super().__eq__(other)

    def __repr__(self):
        return f'"{super().__str__()}"'


class ScmSymbol(str):
    def __init__(self, string):
        str.__init__(string)

    def __eq__(self, other):
        if type(self) != type(other):
            return False
        return super().__eq__(other)

    def __repr__(self):
        return f"'{super().__str__()}"


ScmVector = list["SExp"]
ScmPair = tuple["SExp", "SExp"]

SExp = ScmVoid | ScmNil | ScmBoolean | ScmChar | ScmString | ScmSymbol | ScmNumber | ScmVector | ScmPair


@dataclass(eq=True)
class ScmVar:
    name: str


@dataclass(eq=True)
class LambdaSimple:
    pass


@dataclass(eq=True)
class LambdaOpt:
    opt: str


LambdaKind = LambdaSimple | LambdaOpt


@dataclass(eq=True)
class ScmConst:
    sexpr: SExp


@dataclass(eq=True)
class ScmVarGet:
    var: ScmVar


@dataclass(eq=True)
class ScmIf:
    test: "Expr"
    dit: "Expr"
    dif: "Expr"


@dataclass(eq=True)
class ScmSeq:
    exprs: list["Expr"]


@dataclass(eq=True)
class ScmOr:
    exprs: list["Expr"]


@dataclass(eq=True)
class ScmVarSet:
    var: ScmVar
    val: "Expr"


@dataclass(eq=True)
class ScmVarDef:
    var: ScmVar
    val: "Expr"


@dataclass(eq=True)
class ScmLambda:
    params: list[str]
    kind: LambdaKind
    body: "Expr"


@dataclass(eq=True)
class ScmApplic:
    applicative: "Expr"
    params: list["Expr"]


Expr = ScmConst | ScmVarGet | ScmIf | ScmSeq | ScmOr | ScmVarSet | ScmVarDef | ScmLambda | ScmApplic


class AppKind(Enum):
    Tail_Call = 0
    Non_Tail_Call = 1


@dataclass(eq=True)
class Free:
    pass


@dataclass(eq=True)
class Param:
    minor: int


@dataclass(eq=True)
class Bound:
    major: int
    minor: int


LexicalAddress = Free | Param | Bound


# let string_of_lexical_address: lexical_address -> string = function
#     | Free -> "Free"
#     | Param(x) -> Printf.sprintf "Param(%d)" x
#     | Bound(x, y) -> Printf.sprintf "Bound(%d,%d)" x y;;
#
# type var' = Var' of string * lexical_address;;
@dataclass(eq=True)
class ScmVarTag:
    name: str
    lexical_address: LexicalAddress


ScmConstTag = ScmConst


@dataclass(eq=True)
class ScmVarGetTag:
    var: ScmVarTag


@dataclass(eq=True)
class ScmIfTag:
    test: "ExprTag"
    dit: "ExprTag"
    dif: "ExprTag"


@dataclass(eq=True)
class ScmSeqTag:
    exprs: list["ExprTag"]


@dataclass(eq=True)
class ScmOrTag:
    exprs: list["ExprTag"]


@dataclass(eq=True)
class ScmVarSetTag:
    var: ScmVarTag
    val: "ExprTag"


@dataclass(eq=True)
class ScmVarDefTag:
    var: ScmVarTag
    val: "ExprTag"


@dataclass(eq=True)
class ScmBoxTag:
    var: ScmVarTag


@dataclass(eq=True)
class ScmBoxGetTag:
    var: ScmVarTag


@dataclass(eq=True)
class ScmBoxSetTag:
    var: ScmVarTag
    val: "ExprTag"


@dataclass(eq=True)
class ScmLambdaTag:
    params: list[str]
    kind: LambdaKind
    body: "ExprTag"


@dataclass(eq=True)
class ScmApplicTag:
    applicative: "ExprTag"
    params: list["ExprTag"]
    kind: AppKind


ExprTag = ScmConstTag | ScmVarGetTag | ScmIfTag | ScmSeqTag | ScmOrTag | ScmVarSetTag | \
          ScmVarDefTag | ScmBoxTag | ScmBoxGetTag | ScmBoxSetTag | ScmLambdaTag | ScmApplicTag
