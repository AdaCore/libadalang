--  Check that Libadalang.Project_Provider.Source_Files works as expected for
--  representative inputs.

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   function "+" (S : String) return Unbounded_String
   renames To_Unbounded_String;

   Env  : Project_Environment_Access;
   Tree : Project_Tree;

   type Project_Name_Array is array (Positive range <>) of Unbounded_String;

   procedure Check (Projects : Project_Name_Array);
   --  Print the output of Libadalang.Project.Source_Files for the given list
   --  of projects and for all source files modes.

   function Starts_With (S, Prefix : String) return Boolean
   is (S'Length >= Prefix'Length
       and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);

   function Is_Runtime_Source (Simple_Name : String) return Boolean
   is (Starts_With (Simple_Name, "a-")
       or else Starts_With (Simple_Name, "i-")
       or else Starts_With (Simple_Name, "g-")
       or else Starts_With (Simple_Name, "s-")
       or else Simple_Name in
         "ada.ads"
         | "calendar.ads"
         | "directio.ads"
         | "gnat.ads"
         | "interfac.ads"
         | "ioexcept.ads"
         | "machcode.ads"
         | "memtrack.adb"
         | "sequenio.ads"
         | "system.ads"
         | "text_io.ads"
         | "unchconv.ads"
         | "unchdeal.ads");
   --  Return whether ``Simple_Name`` designates a source from the runtime

   -----------
   -- Check --
   -----------

   procedure Check (Projects : Project_Name_Array) is
      Prjs : Project_Array (Projects'Range);
   begin
      Put ("## [");
      for I in Projects'Range loop
         declare
            Name : constant String := To_String (Projects (I));
         begin
            if I > Projects'First then
               Put (", ");
            end if;
            Put (Name);
            Prjs (I) := Tree.Project_From_Name (Name);
         end;
      end loop;
      Put_Line ("]");
      New_Line;

      for Mode in Source_Files_Mode loop
         Put_Line ("For " & Mode'Image);

         --  In order to preserve test output stability, do not list runtime
         --  sources, but just track whether we have seen some runtime units.

         declare
            Has_Runtime : Boolean := False;
         begin
            for F of Source_Files (Tree, Mode, Prjs) loop
               declare
                  Simple_Name : constant String :=
                    Ada.Directories.Simple_Name (To_String (F));
               begin
                  if Is_Runtime_Source (Simple_Name) then
                     Has_Runtime := True;
                  else
                     Put_Line ("  " & Simple_Name);
                  end if;
               end;
            end loop;
            if Has_Runtime then
               Put_Line ("  ... plus runtime sources");
            end if;
         end;
         New_Line;
      end loop;

      New_Line;
   end Check;

begin
   Initialize (Env);
   Tree.Load (Create (+"root.gpr"), Env);

   --  Simple case: just pass the root project, both implicitly and explicitly

   Check ((1 .. 0 => <>));
   Check ((1 => +"root"));

   --  Pass the two direct subprojects

   Check ((+"sub1", +"sub2"));

   --  Pass only one of them

   Check ((1 => +"sub2"));

   --  Pass the common subproject only

   Check ((1 => +"common"));

   --  Pass one subproject and the (redundant) common subproject

   Check ((+"sub2", +"common"));

   --  Only pass a "root" externally built project

   Check ((1 => +"installed"));

   --  Only pass a "leaf" externally built project

   Check ((1 => +"installed_dep"));

   Tree.Unload;
   Free (Env);

   Put_Line ("Done.");
end Main;
