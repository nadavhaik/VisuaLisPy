from pathlib import Path
import ocaml

def _singleton(class_):
    class class_w(class_):
        _instance = None
        def __new__(class_, *args, **kwargs):
            if class_w._instance is None:
                class_w._instance = super(class_w,
                                    class_).__new__(class_,
                                                    *args,
                                                    **kwargs)
                class_w._instance._sealed = False
            return class_w._instance
        def __init__(self, *args, **kwargs):
            if self._sealed:
                return
            super(class_w, self).__init__(*args, **kwargs)
            self._sealed = True
    class_w.__name__ = class_.__name__
    return class_w


OCAML_PARSER_FILE = "./scheme_parser/parser.ml"

@_singleton
class OcamlBridge:
    def __init__(self):

        parser_code = Path(OCAML_PARSER_FILE).read_text()
        self.module = ocaml.compile(parser_code)
    
    def sexp_parser(self, string: str, index_from: int):
        return self.module.sexp_parser(string, index_from)

    def tag_parse(self, sexp):
        return self.module.tag_parser(sexp)

    def exp_parser(self, string: str, index_from: int):
        return self.module.exp_parser(string, index_from)

    def sexprs_parser(self, string: str, index_from: int):
        return self.module.sexps_parser(string, index_from)

    def exprs_parser(self, string: str, index_from: int):
        return self.module.exps_parser(string, index_from)
