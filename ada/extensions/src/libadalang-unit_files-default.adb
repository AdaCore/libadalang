with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

with Interfaces; use Interfaces;

with GNATCOLL.Projects;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Unit_Files.Projects;

package body Libadalang.Unit_Files.Default is

   function File_From_Unit
     (Name : String;
      Kind : Unit_Kind)
      return String
   is (+GNATCOLL.Projects.File_From_Unit
         (GNATCOLL.Projects.No_Project,
          Name,
          Libadalang.Unit_Files.Projects.Convert (Kind),
          "ada"));

   --------------
   -- Get_File --
   --------------

   overriding function Get_File
     (Provider : Default_Unit_File_Provider_Type;
      Context  : Analysis_Context;
      Node     : Ada_Node;
      Kind     : Unit_Kind)
      return String
   is
      pragma Unreferenced (Provider, Context);
   begin
      if Node.all in Name_Type'Class then
         declare
            Str_Name  : constant String :=
               Unit_String_Name (Libadalang.Analysis.Name (Node));
         begin
            return File_From_Unit (Str_Name, Kind);
         end;
      end if;

      raise Property_Error with "invalid AST node for unit name";
   end Get_File;

   --------------
   -- Get_File --
   --------------

   overriding function Get_File
     (Provider : Default_Unit_File_Provider_Type;
      Context  : Analysis_Context;
      Name     : Text_Type;
      Kind     : Unit_Kind)
      return String
   is
      pragma Unreferenced (Provider, Context);
   begin
      return File_From_Unit (Unit_String_Name (Name), Kind);
   end Get_File;

   --------------------
   -- Unit_Text_Name --
   --------------------

   function Unit_Text_Name (N : Name) return Text_Type is
   begin
      if N.all in Identifier_Type'Class then
         return Text (Identifier (N).F_Tok);

      elsif N.all in Dotted_Name_Type'Class then
         declare
            DN : constant Dotted_Name := Dotted_Name (N);
         begin
            if DN.F_Prefix.all in Name_Type'Class
               and then DN.F_Suffix.all in Identifier_Type'Class
            then
               return (Unit_Text_Name (Name (DN.F_Prefix))
                       & "."
                       & Unit_Text_Name (Name (DN.F_Suffix)));
            end if;
         end;
      end if;

      raise Property_Error with "invalid AST node for unit name";
   end Unit_Text_Name;

   Dot        : constant := Character'Pos ('.');
   Underscore : constant := Character'Pos ('_');
   Zero       : constant := Character'Pos ('0');
   Nine       : constant := Character'Pos ('9');
   Lower_A    : constant := Character'Pos ('a');
   Upper_A    : constant := Character'Pos ('A');
   Lower_Z    : constant := Character'Pos ('z');
   Upper_Z    : constant := Character'Pos ('Z');

   ----------------------
   -- Unit_String_Name --
   ----------------------

   function Unit_String_Name (Name : Text_Type) return String is
      Result      : String (Name'Range);
   begin
      --  Make Name lower case and replace dots with dashes. Only allow ASCII.

      for I in Name'Range loop
         declare
            C  : constant Wide_Wide_Character := Name (I);
            CN : constant Unsigned_32 := Wide_Wide_Character'Pos (C);
         begin
            if CN in Dot
                   | Underscore
                   | Zero .. Nine
                   | Upper_A .. Upper_Z
                   | Lower_A .. Lower_Z
            then
               Result (I) := Ada.Strings.Maps.Value
                 (Ada.Strings.Maps.Constants.Lower_Case_Map,
                  Character'Val (CN));
            else
               raise Property_Error with
                 ("unhandled unit name: character "
                  & Image (T => (1 => C), With_Quotes => True)
                  & " not supported");
            end if;
         end;
      end loop;

      return Result;
   end Unit_String_Name;

   --------------------
   -- Spec_File_Name --
   --------------------

   function Spec_File_Name (Name : String) return String is
     (File_From_Unit (Name, Unit_Specification));

   --------------------
   -- Body_File_Name --
   --------------------

   function Body_File_Name (Name : String) return String is
     (File_From_Unit (Name, Unit_Body));

end Libadalang.Unit_Files.Default;
