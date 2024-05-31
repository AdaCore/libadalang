procedure Test is
   task My_Task;
   --% node.find(lal.DefiningName).p_basic_decl

   task body My_Task is
   begin
      null;
   end My_Task;
   --% node.find(lal.DefiningName).p_basic_decl

   task type My_Task_Type;
   --% node.find(lal.DefiningName).p_basic_decl

   task body My_Task_Type is
   begin
      null;
   end My_Task_Type;
   --% node.find(lal.DefiningName).p_basic_decl

   generic
   package Pkg is
   end Pkg;
   --% node.find(lal.DefiningName).p_basic_decl

   generic
   procedure Foo;
   --% node.find(lal.DefiningName).p_basic_decl

   procedure Foo is null;
   --% node.find(lal.DefiningName).p_basic_decl
begin
   null;
end Test;
