class InvalidOperationException(Exception):
    pass


class Char(str):
    def __init__(self, string):
        str.__init__(string)
        if len(self) != 1:
            raise ValueError(f"{string} is not a char!")

    def __repr__(self):
        return f"'{super().__str__()}'"

    def __eq__(self, other):
        if type(self) != type(other):
            return False
        return super().__eq__(other)

    def __iadd__(self, *args, **kwargs):
        raise InvalidOperationException("the '+=' operator is undefined for chars")

    def __imul__(self, *args, **kwargs):
        raise InvalidOperationException("the '*=' operator is undefined for chars")
