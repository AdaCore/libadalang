with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

with Interfaces; use Interfaces;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.AST.Types; use Libadalang.AST.Types;

package body Libadalang.Unit_Files.Default is

   --------------
   -- Get_File --
   --------------

   overriding function Get_File
     (Provider : Default_Unit_File_Provider_Type;
      Node     : Ada_Node)
      return String
   is
      pragma Unreferenced (Provider);
   begin
      if Node.all in Name_Type'Class then
         return Get_Unit_Name (Name (Node)) & ".ads";
      end if;

      raise Property_Error with "invalid AST node for unit name";
   end Get_File;

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name (N : Name) return Text_Type is
   begin
      if N.all in Identifier_Type'Class then
         return Data (Identifier (N).F_Tok).Text.all;

      elsif N.all in Dotted_Name_Type'Class then
         declare
            DN : constant Dotted_Name := Dotted_Name (N);
         begin
            if DN.F_Prefix.all in Name_Type'Class
               and then DN.F_Suffix.all in Identifier_Type'Class
            then
               return (Get_Unit_Name (Name (DN.F_Prefix))
                       & "."
                       & Get_Unit_Name (Name (DN.F_Suffix)));
            end if;
         end;
      end if;

      raise Property_Error with "invalid AST node for unit name";
   end Get_Unit_Name;

   Underscore : constant := Character'Pos ('_');
   Zero       : constant := Character'Pos ('0');
   Nine       : constant := Character'Pos ('9');
   Lower_A    : constant := Character'Pos ('a');
   Upper_A    : constant := Character'Pos ('A');
   Lower_Z    : constant := Character'Pos ('z');
   Upper_Z    : constant := Character'Pos ('Z');

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name (N : Name) return String is
      Text_Result : constant Text_Type := Get_Unit_Name (N);
      Result      : String (1 .. Text_Result'Length);
      I           : Positive := 1;
   begin
      --  Make Name lower case and replace dots with dashes. Only allow ASCII.

      for C of Text_Type'(Get_Unit_Name (N)) loop
         declare
            CN : constant Unsigned_32 := Wide_Wide_Character'Pos (C);
         begin
            if C = '.' then
               Result (I) := '-';
            elsif CN in Underscore
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
         I := I + 1;
      end loop;

      return Result;
   end Get_Unit_Name;

end Libadalang.Unit_Files.Default;
