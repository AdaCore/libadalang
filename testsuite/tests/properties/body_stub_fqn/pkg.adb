package body Pkg is
   procedure Proc_1 is separate;
   --% node.p_syntactic_fully_qualified_name

   task type Task_1 is
      entry E;
   end Task_1;
   task body Task_1 is separate;
   --% node.p_syntactic_fully_qualified_name

   protected type P_Body_1 is
      procedure Proc_4;
   end P_Body_1;
   protected body P_Body_1 is separate;
   --% node.p_syntactic_fully_qualified_name

   package body Nested_1 is separate;
   --% node.p_syntactic_fully_qualified_name
end Pkg;
