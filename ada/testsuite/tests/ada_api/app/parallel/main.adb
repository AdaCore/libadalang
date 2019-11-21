with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Libadalang.Helpers;  use Libadalang.Helpers;

procedure Main is

   procedure Generate_Sources;
   --  Generate sources for the project to load

   function Stripped_Image (N : Natural) return String;

   package Decl_Count_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Natural);

   type Decl_Count_Map_Array is array (Job_ID range <>) of Decl_Count_Maps.Map;
   type Decl_Count_Map_Array_Access is access Decl_Count_Map_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Decl_Count_Map_Array, Decl_Count_Map_Array_Access);

   Maps : Decl_Count_Map_Array_Access;

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array);
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   procedure App_Tear_Down
     (Context : App_Context; Jobs : App_Job_Context_Array);

   package App is new Libadalang.Helpers.App
     ("Count object declarations in source files",
      Enable_Parallelism => True,
      App_Setup          => App_Setup,
      Process_Unit       => Process_Unit,
      App_Tear_Down      => App_Tear_Down);

   --------------------
   -- Stripped_Image --
   --------------------

   function Stripped_Image (N : Natural) return String is
      Result : constant String := N'Image;
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Stripped_Image;

   ----------------------
   -- Generate_Sources --
   ----------------------

   procedure Generate_Sources is
   begin
      for I in 1 .. 50 loop
         declare
            F : File_Type;
            N : constant String := Stripped_Image (I);
         begin
            Create (F, Name => "src/p" & N & ".ads");
            Put_Line (F, "package P" & N & " is");
            for J in 1 .. I / 2 loop
               Put_Line (F, "  B" & Stripped_Image (J) & " : Boolean;");
            end loop;
            Put_Line (F, "end P" & N & ";");
            Close (F);
         end;
      end loop;
   end Generate_Sources;

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array) is
      pragma Unreferenced (Context);
   begin
      Maps := new Decl_Count_Map_Array (Jobs'Range);
   end App_Setup;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is

      function Visit (Node : Ada_Node'Class) return Visit_Status;

      Map   : Decl_Count_Maps.Map renames Maps.all (Context.ID);
      File  : constant String := Unit.Get_Filename;
      Count : Natural := 0;

      -----------
      -- Visit --
      -----------

      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Ada_Object_Decl then
            Count := Count + 1;
         end if;
         return Into;
      end Visit;
   begin
      Unit.Root.Traverse (Visit'Access);
      Map.Insert (File, Count);
   end Process_Unit;

   -------------------
   -- App_Tear_Down --
   -------------------

   procedure App_Tear_Down
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is
      use Decl_Count_Maps;
      Merged_Map : Map;
   begin
      --  Merge all job-specific maps into Merged_Map

      for M of Maps.all loop
         for Pos in M.Iterate loop
            Merged_Map.Insert (Key (Pos), Element (Pos));
         end loop;
      end loop;
      Free (Maps);

      --  Report statistics

      for Pos in Merged_Map.Iterate loop
         declare
            File  : constant String := +Create (+Key (Pos)).Base_Name;
            Count : constant Natural := Element (Pos);
         begin
            Put_Line (File & ":" & Count'Image);
         end;
      end loop;
   end App_Tear_Down;
begin
   Generate_Sources;
   App.Run;
end Main;
