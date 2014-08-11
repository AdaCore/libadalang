COMPILED = True

if COMPILED:
    # noinspection PyUnresolvedReferences
    from parsers.compiled_native import *
    _ = Discard
else:
    # noinspection PyUnresolvedReferences
    from parsers.classic import *

