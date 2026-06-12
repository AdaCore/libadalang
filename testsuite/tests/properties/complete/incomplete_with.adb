--  Completion in a with clause can only propose units already loaded in
--  the analysis context: without the "with Pkg_1.Child;" clause below
--  (which loads Pkg_1.Child, and Pkg_1.Child2 transitively), completing
--  "with Pkg_1." would return an empty result.

with Pkg_1.Child;
with Pkg_1.
--% list(node.p_complete)

procedure Incomplete_With is
begin
   null;
end Incomplete_With;
