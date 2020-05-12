# The following sources do not contain the same string literals so that
# testcases can check the the reparsing actually worked.

src_buffer_iso_8859_1 = b"""with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
begin
   Put_Line("H\xe9llo w\xf6rld!");
end Test;
"""


src_buffer_utf_8 = b"""with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
begin
   Put_Line("H\xc3\xa8llo w\xc3\xb5rld!");
end Test;
"""


def get_string_literal(unit):
    """
    Assuming UNIT is one of the above source that is parsed successfuly, return
    the text associated to the string literal in the Put_Line call.
    """
    node = unit.root
    subp = node.f_body.f_item
    call = subp.f_stmts.f_stmts[0]
    str_lit = call.f_call.f_suffix[0].f_r_expr
    return str_lit.text
