"""
Test that env shedding works correctly when resolving from the body of a
generic instantiation.
"""

import libadalang as lal


def type_from_subp_body(call):
    return str(
        call.p_referenced_decl().p_body_part().f_subp_spec
        .f_subp_returns.p_designated_type_decl
    )


c = lal.AnalysisContext('utf-8')
u = c.get_from_file("main.adb")

ps = u.root.find(lambda n: n.text == 'Foo').parent
print(type_from_subp_body(ps))

ps = u.root.find(lambda n: n.text == 'Bar').parent
print(type_from_subp_body(ps))

ps = u.root.find(lambda n: n.text == 'Baz').parent
print(type_from_subp_body(ps))
