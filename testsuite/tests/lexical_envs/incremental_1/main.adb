with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Analysis;  use Libadalang.Analysis;

procedure Main is

   Main_Buffer : constant String :=
     "with Foo;" & ASCII.LF
     & "procedure Main is" & ASCII.LF
     & "begin" & ASCII.LF
     & "   Foo.Bar;" & ASCII.LF
     & "end Main;" & ASCII.LF;

   Foo_Spec_Buffer : constant String :=
     "package Foo is" & ASCII.LF
     & "   procedure Bar;" & ASCII.LF
     & "end Foo;" & ASCII.LF;

   Foo_Body_Buffer : constant String :=
     "package body Foo is" & ASCII.LF
     & "" & ASCII.LF
     & "   procedure Bar is" & ASCII.LF
     & "   begin" & ASCII.LF
     & "      null;" & ASCII.LF
     & "   end Bar;" & ASCII.LF
     & "" & ASCII.LF
     & "end Foo;" & ASCII.LF;

   Ctx      : Analysis_Context;
   Main     : Analysis_Unit;
   Foo_Spec : Analysis_Unit;
   Foo_Body : Analysis_Unit;

   procedure Print_Label (Label : String);
   procedure Check;

   -----------------
   -- Print_Label --
   -----------------

   procedure Print_Label (Label : String) is
   begin
      Put_Line ("# " & Label);
   end Print_Label;

   -----------
   -- Check --
   -----------

   procedure Check is
      N : constant Dotted_Name := Main.Root.Lookup ((4, 7)).As_Dotted_Name;
   begin
      Put_Line (N.Image & " refers to " & N.P_Referenced_Defining_Name.Image);
      New_Line;
   end Check;
begin
   if Ada.Command_Line.Argument_Count > 0 then
      GNATCOLL.Traces.Parse_Config
        ("LIBADALANG.MAIN_TRACE=yes" & ASCII.LF
         & "LANGKIT.LEXICAL_ENV=yes" & ASCII.LF
         & "LANGKIT.LEXICAL_ENV.RECURSIVE=yes" & ASCII.LF
         & "LANGKIT.LEXICAL_ENV.CACHES=yes" & ASCII.LF);
   end if;

   Print_Label ("Reference");
   Ctx := Create_Context;
   Main := Ctx.Get_From_Buffer ("main.adb", Buffer => Main_Buffer);
   Foo_Spec := Ctx.Get_From_Buffer ("foo.ads", Buffer => Foo_Spec_Buffer);
   Check;

   Ctx := Create_Context;
   Print_Label ("Parse main.adb");
   Main := Ctx.Get_From_Buffer ("main.adb", Buffer => Main_Buffer);
   Print_Label ("First check");
   Check;
   Print_Label ("Parse foo.ads");
   Foo_Spec := Ctx.Get_From_Buffer ("foo.ads", Buffer => Foo_Spec_Buffer);
   Print_Label ("Parse foo.adb");
   Foo_Body := Ctx.Get_From_Buffer ("foo.adb", Buffer => Foo_Body_Buffer);
   Print_Label ("Second check");
   Check;

   Put_Line ("main.adb: Done.");
end Main;
