procedure Test is
   function Foo (X : Integer) return Integer
      with Annotate => (My_Tool, My_Command_1),
           Annotate => (My_Tool, My_Command_2);
   --% node.p_get_annotations()

   function Foo (X : Integer) return Integer is
   begin
      return X;
   end Foo;

   pragma Annotate (My_Tool, My_Command_3, Entity => Foo);
   pragma Annotate (My_Tool, My_Command_4, Entity => Foo);

   package Pkg is
      type Base is tagged null record
         with Annotate => (My_Tool, My_Commmand_1);
      --% node.p_get_annotations()

      type Der is new Base with private
         with Annotate => (My_Tool, My_Command_3);
      --% node.p_get_annotations()

      pragma Annotate (My_Tool, My_Command_2, Entity => Base);
   private
      type Der is new Base with null record;

      pragma Annotate (My_Tool, My_Command_4, Entity => Der);
   end Pkg;
begin
   null;
end Test;

