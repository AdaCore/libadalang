--  Check that Libadalang.Project_Provider.Source_Files works as expected for
--  representative inputs.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View.Set;

with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   function "+" (S : String) return Unbounded_String
   renames To_Unbounded_String;

   Env  : Project_Environment_Access;
   Tree : Project_Tree_Access;

   GPR2_Tree : GPR2.Project.Tree.Object;
   Log       : GPR2.Log.Object;

   type Project_Name_Array is array (Positive range <>) of Unbounded_String;

   procedure Load (Filename : String);
   --  Load the GPR file at Filename into Env/Tree and GPR2_Tree

   procedure Unload;
   --  Free all GPR resources

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

   ----------
   -- Load --
   ----------

   procedure Load (Filename : String) is
      Options : GPR2.Options.Object;
   begin
      Initialize (Env);
      Tree := new Project_Tree;
      Tree.Load (Create (+Filename), Env);

      Options.Add_Switch (GPR2.Options.P, Filename);
      if not GPR2_Tree.Load (Options, With_Runtime => True)
         or else not Update_Sources (GPR2_Tree)
      then
         raise Program_Error;
      end if;
   end Load;

   ------------
   -- Unload --
   ------------

   procedure Unload is
   begin
      Tree.Unload;
      Free (Tree);
      Free (Env);
   end Unload;

   -----------
   -- Check --
   -----------

   procedure Check (Projects : Project_Name_Array) is
      GPR1_Prjs : Project_Array (Projects'Range);
      GPR2_Prjs : GPR2.Project.View.Set.Object;
   begin
      Put ("## [");
      for I in Projects'Range loop
         declare
            Name  : constant String := To_Lower (To_String (Projects (I)));
            Found : Boolean;
         begin
            if I > Projects'First then
               Put (", ");
            end if;
            Put (Name);
            GPR1_Prjs (I) := Tree.Project_From_Name (Name);

            Found := False;
            for V of GPR2_Tree loop
               if To_Lower (String (V.Name)) = Name then
                  Found := True;
                  GPR2_Prjs.Include (V);
               end if;
            end loop;
            if not Found then
               raise Program_Error with "no such project: " & Name;
            end if;
         end;
      end loop;
      Put_Line ("]");
      New_Line;

      for Mode in Source_Files_Mode loop
         Put_Line ("For " & Mode'Image);

         --  In order to preserve test output stability, do not list runtime
         --  sources, but just track whether we have seen some runtime units.

         declare
            use type Filename_Vectors.Vector;

            Has_Runtime  : Boolean := False;
            GPR1_Sources : Filename_Vectors.Vector :=
              Source_Files (Tree.all, Mode, GPR1_Prjs);
            GPR2_Sources : constant Filename_Vectors.Vector :=
              Source_Files (GPR2_Tree, Mode, GPR2_Prjs);
         begin
            --  Remove the "memtrack.adb" runtime unit from GPR1 sources: it is
            --  not supposed to be there, but we have no hope to get this fixed
            --  at this point.

            for I in reverse 1 .. GPR1_Sources.Last_Index loop
               declare
                  Simple_Name : constant String :=
                    Ada.Directories.Simple_Name (To_String (GPR1_Sources (I)));
               begin
                  if Simple_Name = "memtrack.adb" then
                     GPR1_Sources.Delete (I);
                  end if;
               end;
            end loop;

            for F of GPR1_Sources loop
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

            if GPR1_Sources /= GPR2_Sources then
               Put_Line ("With GPR1:");
               for F of GPR1_Sources loop
                  Put_Line ("  " & To_String (F));
               end loop;
               Put_Line ("With GPR2:");
               for F of GPR2_Sources loop
                  Put_Line ("  " & To_String (F));
               end loop;
               raise Program_Error with "got different sources with GPR2";
            end if;
         end;
         New_Line;
      end loop;

      New_Line;
   end Check;

begin
   Load ("root.gpr");

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

   Unload;

   Put_Line ("===== Now testing with aggregate project =====");
   New_Line;

   Load ("agg.gpr");

   --  Simple case: just pass the root project, both implicitly and explicitly

   Check ((1 .. 0 => <>));
   Check ((1 => +"agg"));

   --  Pass the two direct subprojects

   Check ((+"sub1", +"sub2"));

   --  Pass only one of them

   Check ((1 => +"sub2"));

   --  Pass the common subproject only

   Check ((1 => +"common"));

   --  Pass one subproject and the (redundant) common subproject

   Check ((+"sub2", +"common"));

   Unload;

   Put_Line ("Done.");
end Main;
