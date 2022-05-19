separate (Pkg)
package body Nested_1 is
   procedure Proc_5 is separate;
   --% node.p_syntactic_fully_qualified_name

   procedure Proc_6 is
   begin
      Proc_5;
   end Proc_6;
end Nested_1;
