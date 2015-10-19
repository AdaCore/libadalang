# The following sources do not contain the same string literals so that
# testcases can check the the reparsing actually worked.

src_buffer_iso_8859_1 = """with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
begin
   Put_Line("H\xe9llo w\xf6rld!");
end Test;
"""


src_buffer_utf_8 = """with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
begin
   Put_Line("H\xc3\xa8llo w\xc3\xb5rld!");
end Test;
"""


def get_string_literal(unit):
    """
    Assming UNIT is one of the above source that is parsed successfuly, return
    the text associated to the string literal in the Put_Line call.
    """
    node = unit.root
    subp = node.f_bodies[0].f_item
    call = subp.f_statements.f_statements[0]
    str_lit = call.f_suffix.f_params[0].f_expr
    return str_lit.f_tok.text
