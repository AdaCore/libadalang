procedure Test is
begin
   null;
end;
pragma Annotate (Xcov, Exempt_On,
                 "Invalid" & " argument"
                 --% concat_op=node.parent.parent.parent
                 --% concat_op.p_is_static_expr()
                 );
