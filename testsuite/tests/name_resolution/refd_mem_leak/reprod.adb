with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;

procedure Reprod is
   Ctx : constant Analysis_Context := Create_Context;

   Unit1, Unit2, Unit3 : Analysis_Unit;

begin
   Unit1 := Ctx.Get_From_Buffer
     (Filename => "great_children.ads",
      Buffer   =>
         "with Children; use Children;"
         & ASCII.LF & "package Great_Children is"
         & ASCII.LF & "   type Great_Child is new Child with null Record;"
         & ASCII.LF & "   procedure Primitive (Self : Great_Child);"
         & ASCII.LF & "end Great_Children;");
   Unit1.Populate_Lexical_Env;

   Unit2 := Ctx.Get_From_Buffer
     (Filename => "children.ads",
      Buffer   =>
         "with Parents; use Parents;"
         & ASCII.LF & "package Children is"
         & ASCII.LF & "   type Child is new Parent with null Record;"
         & ASCII.LF & "   procedure Primitive (Self : Child);"
         & ASCII.LF & "end Children;");
   Unit2.Populate_Lexical_Env;

   Unit3 := Ctx.Get_From_Buffer
     (Filename => "great_children.adb",
      Buffer   =>
         "with Ada.Text_IO;"
         & ASCII.LF & "package body Great_Children is"
         & ASCII.LF & "   procedure Primitive (Self : Great_Child) is"
         & ASCII.LF & "   begin"
         & ASCII.LF & "      Ada.Text_IO.Put_Line (""Great child"");"
         & ASCII.LF & "   end Primitive;"
         & ASCII.LF & "end Great_Children;");
   Unit3.Populate_Lexical_Env;

   Put_Line ("Done.");
end Reprod;
