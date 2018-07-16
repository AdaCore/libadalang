with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

with Interfaces; use Interfaces;

with GNATCOLL.Projects;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Unit_Files.Projects;

package body Libadalang.Unit_Files.Default is

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Default_Unit_Provider_Type;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String
   is
      pragma Unreferenced (Provider);
   begin
      return File_From_Unit (Unit_String_Name (Name), Kind);
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Default_Unit_Provider_Type;
      Context     : LP.Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LP.Analysis_Unit
   is
      pragma Unreferenced (Provider);
   begin
      return LP.Get_From_File
        (Context, File_From_Unit (Unit_String_Name (Name), Kind), Charset,
         Reparse);
   end Get_Unit;

   --------------------
   -- Unit_Text_Name --
   --------------------

   function Unit_Text_Name (N : Bare_Name) return Text_Type is
   begin
      if N.all in Bare_Identifier_Type'Class then
         return N.Text;

      elsif N.all in Bare_Dotted_Name_Type'Class then
         declare
            DN : constant Bare_Dotted_Name := Bare_Dotted_Name (N);
         begin
            if DN.F_Prefix.all in Bare_Name_Type'Class
               and then DN.F_Suffix.all in Bare_Identifier_Type'Class
            then
               return (Unit_Text_Name (Bare_Name (DN.F_Prefix))
                       & "."
                       & Unit_Text_Name (Bare_Name (DN.F_Suffix)));
            end if;
         end;
      end if;

      raise Property_Error with "invalid AST node for unit name";
   end Unit_Text_Name;

   Dot        : constant := Character'Pos ('.');
   Dash       : constant := Character'Pos ('-');
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
      Result : String (Name'Range);
   begin

      --  Make Name lower case. Only allow ASCII.

      for I in Name'Range loop
         declare
            C  : constant Wide_Wide_Character := Name (I);
            CN : constant Unsigned_32 := Wide_Wide_Character'Pos (C);
         begin
            case CN is
               when Dot
                    | Underscore
                    | Dash
                    | Zero .. Nine
                    | Upper_A .. Upper_Z
                    | Lower_A .. Lower_Z =>
                  Result (I) := Ada.Strings.Maps.Value
                    (Ada.Strings.Maps.Constants.Lower_Case_Map,
                     Character'Val (CN));
               when others =>
                  raise Property_Error with
                    ("unhandled unit name: character "
                     & Image (T => (1 => C), With_Quotes => True)
                     & " not supported");
            end case;
         end;
      end loop;

      return Result;
   end Unit_String_Name;

   --------------------
   -- File_From_Unit --
   --------------------

   function File_From_Unit
     (Name : String;
      Kind : Unit_Kind)
      return String
   is
   begin
      GPR_Lock.Seize;

      declare
         Ret : String := +GNATCOLL.Projects.File_From_Unit
           (GNATCOLL.Projects.No_Project,
            Name,
            Libadalang.Unit_Files.Projects.Convert (Kind),
            "ada");
      begin
         GPR_Lock.Release;
         return Ret;
      end;
   exception
      when others =>
         GPR_Lock.Release;
         raise;
   end File_From_Unit;

end Libadalang.Unit_Files.Default;
