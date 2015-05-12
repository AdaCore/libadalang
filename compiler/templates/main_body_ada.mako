## vim: filetype=makoada

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with ${_self.ada_api_settings.lib_name}.Parsers;
use ${_self.ada_api_settings.lib_name}.Parsers;

package body ${_self.ada_api_settings.lib_name} is

   function Create (Context   : Analysis_Context;
                    File_Name : String) return Analysis_Unit;

   procedure Destroy (Unit : Analysis_Unit);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Analysis_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Analysis_Unit);

   ------------
   -- Create --
   ------------

   function Create return Analysis_Context is
   begin
      return new Analysis_Context_Type'
        (Units_Map => <>,
         Symbols   => Allocate);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Context   : Analysis_Context;
                    File_Name : String) return Analysis_Unit
   is
      Result : Analysis_Unit := new Analysis_Unit_Type'
        (Context     => Context,
         Ref_Count   => 1,
         AST_Root    => null,
         File_Name   => To_Unbounded_String (File_Name),
         TDH         => <>,
         Diagnostics => <>);

   begin
      Initialize (Result.TDH, Context.Symbols);
      declare
         Parser : Parser_Type;
      begin
         Parser := Create_From_File (File_Name, Result.TDH'Access);
         Result.AST_Root := Parse (Parser);
         Result.Diagnostics := Parser.Diagnostics;
         Clean_All_Memos;
      exception
         when Name_Error =>
            Dec_Ref (Result);
            raise;
      end;
      return Result;
   end Create;

   ----------------------
   -- Create_From_File --
   ----------------------

   function Create_From_File
     (Context   : Analysis_Context;
      File_Name : String) return Analysis_Unit
   is
      Result : constant Analysis_Unit := Create (Context, File_Name);
   begin
      Context.Units_Map.Insert (To_Unbounded_String (File_Name), Result);
      return Result;
   end Create_From_File;

   ------------
   -- Remove --
   ------------

   procedure Remove (Context   : Analysis_Context;
                     File_Name : String)
   is
      use Units_Maps;

      Cur : Cursor := Context.Units_Map.Find (To_Unbounded_String (File_Name));
   begin
      if Cur /= No_Element then

         --  We remove the corresponding analysis unit from this context but
         --  users could keep references on it, so make sure it can live
         --  independently.

         declare
            Unit : constant Analysis_Unit := Element (Cur);
         begin
            Unit.Context := null;
            Dec_Ref (Unit);
         end;

         Context.Units_Map.Delete (Cur);
      end if;
   end Remove;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Analysis_Context) is
   begin
      for Unit of Context.Units_Map loop
         Unit.Context := null;
         Dec_Ref (Unit);
      end loop;
      Free (Context.Symbols);
      Free (Context);
   end Destroy;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Unit : Analysis_Unit) is
   begin
      Unit.Ref_Count := Unit.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Unit : Analysis_Unit) is
   begin
      Unit.Ref_Count := Unit.Ref_Count - 1;
      if Unit.Ref_Count = 0 then
         Destroy (Unit);
      end if;
   end Dec_Ref;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Unit : Analysis_Unit) is
      Unit_Var : Analysis_Unit := Unit;
   begin
      Dec_Ref (Unit.AST_Root);
      Free (Unit.TDH);
      Free (Unit_Var);
   end Destroy;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Analysis_Unit) is
   begin
      if Unit.AST_Root = null then
         Put_Line ("<empty analysis unit>");
      else
         Unit.AST_Root.Print;
      end if;
   end Print;

   % for chunk in _self.primitives_bodies:
   ${chunk}
   % endfor

end ${_self.ada_api_settings.lib_name};
