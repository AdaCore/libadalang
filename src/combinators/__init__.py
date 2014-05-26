COMPILED = True

if COMPILED:
    print "USING COMPILED COMBINATORS"
    # noinspection PyUnresolvedReferences
    from combinators.compiled_native import *
    _ = Discard
else:
    print "USING OLD COMBINATORS"
    # noinspection PyUnresolvedReferences
    from combinators.classic import *

