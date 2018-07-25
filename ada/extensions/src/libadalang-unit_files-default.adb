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
      Context     : LP.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LP.Analysis_Unit'Class
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

   function Unit_Text_Name (N : LP.Name) return Text_Type is
   begin
      if N.Kind = Ada_Identifier then
         return N.Text;

      elsif N.Kind = Ada_Dotted_Name then
         declare
            DN : constant LP.Dotted_Name := N.As_Dotted_Name;
         begin
            if DN.F_Prefix.Kind in Ada_Name
               and then DN.F_Suffix.Kind = Ada_Identifier
            then
               return (Unit_Text_Name (DN.F_Prefix.As_Name)
                       & "."
                       & Unit_Text_Name (DN.F_Suffix.As_Name));
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

   function File_From_Unit (Name : String; Kind : Unit_Kind) return String is
      Dummy : Scoped_Lock (GPR_Lock'Access);
   begin
      return +GNATCOLL.Projects.File_From_Unit
        (GNATCOLL.Projects.No_Project,
         Name,
         Libadalang.Unit_Files.Projects.Convert (Kind),
         "ada");
   end File_From_Unit;

end Libadalang.Unit_Files.Default;
