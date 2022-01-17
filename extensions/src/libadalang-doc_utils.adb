------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Vectors;

with Libadalang.Common; use Libadalang.Common;

package body Libadalang.Doc_Utils is

   use XStrings;

   function Extract_Doc_From
     (Token            : Token_Reference;
      Backwards        : Boolean;
      Skip_White_Lines : Integer := -1) return Doc_Type;
   --  Extract documentation from comments starting at ``Token``. If
   --  ``Backwards`` is ``True``, then search for documentation backwards.
   --  Skip up to ``Skip_White_Lines`` white lines separating the first doc
   --  comment from ``Token``. If ``Skip_White_Lines`` is -1, skip any number
   --  of white lines.
   --
   --  Will raise a ``Property_Error`` if the doc is incorrectly formatted.

   ----------------------
   -- Extract_Doc_From --
   ----------------------

   function Extract_Doc_From
     (Token            : Token_Reference;
      Backwards        : Boolean;
      Skip_White_Lines : Integer := -1) return Doc_Type
   is
      Tok : Token_Reference := Token;
      T   : XStrings.XString;
      LF  : constant Wide_Wide_Character := To_Wide_Wide_Character (ASCII.LF);

      procedure Next_Token;
      --  Set Tok to the token after it (if Backwards is False) or to the token
      --  before it (if Backwards it True).

      procedure Next_Token is
      begin
         if Backwards then
            Tok := Previous (Tok);
         else
            Tok := Next (Tok);
         end if;
      end Next_Token;

      Ret : Doc_Type;
      K   : Token_Kind;

      package XString_Vectors is new Ada.Containers.Vectors
        (Positive, XString);

      Doc_Vec : XString_Vectors.Vector;
   begin
      Next_Token;

      --  There is no next token: exit
      if Tok = No_Token then
         return Ret;
      end if;

      if Skip_White_Lines /= 0 and then Kind (Data (Tok)) = Ada_Whitespace then
         if Skip_White_Lines = -1 then
            --  If told to skip all white lines, go ahead

            Next_Token;

         elsif Skip_White_Lines > 0 then
            --  If told to skip a certain number of white lines, verify that
            --  the next token indeed contains said number of white lines.

            T := To_XString (Common.Text (Tok));

            if T.Count (LF) = Skip_White_Lines then
               Next_Token;
            end if;
         end if;
      end if;

      --  No comment in the direction expected? There is no doc!
      if Tok = No_Token or else Kind (Data (Tok)) /= Ada_Comment then
         return Ret;
      end if;

     --  Process as many comments as possible from our starting point,
     --  until we find an empty line or anything else than a comment or
     --  a whitespace.
      while Tok /= No_Token  loop
         K := Kind (Data (Tok));
         case K is
            when Ada_Whitespace =>
               T := To_XString (Common.Text (Tok));
               exit when T.Count (LF) > 1;

            when Ada_Comment =>
               T := To_XString (Common.Text (Tok));

               --  Strip potential CR at the end of the line
               if not T.Is_Empty and then T.Get (T.Length) = Chars.CR then
                  T := T.Slice (1, T.Length - 1);
               end if;

               --  Strip the "--" from the comment
               T := T.Slice (3, T.Length);

               --  If this is an annotation then
               if T.Starts_With ("%") then
                  declare
                     --  Try to split on the ":"
                     X : constant XString_Array := T.Split (":");
                     K : constant Wide_Wide_String :=
                       X (1).Slice (2, X (1).Length).Trim.To_String;
                     --             ^ Strip % prefix
                     V : constant Wide_Wide_String :=
                       (if X'Length < 2
                        then raise Property_Error
                          with "Incorrectly formatted docstring"
                        else X (2).Trim.To_String);
                  begin
                     Ret.Annotations.Include (K, V);
                  end;
               else
                  Doc_Vec.Append (T);
               end if;
            when others => exit;
         end case;

         Next_Token;
      end loop;

      --  Reverse the Doc vector if lines were searched backwards
      if Backwards then
         Doc_Vec.Reverse_Elements;
      end if;

      --  Transform the doc vector into a string
      declare
         Last_Index : constant Natural := Doc_Vec.Last_Index;

         Offset : Positive := Positive'Last;
         --  Offset for the leftmost first non whitespace char in all the
         --  docstring.

      begin
         for I in Doc_Vec.First_Index .. Last_Index loop
            declare
               L       : XString renames Doc_Vec (I);
               Trimmed : XString renames L.Trim;
            begin
               if Trimmed.Length > 0 then
                  Offset := Positive'Min
                    (Offset, L.Length - Trimmed.Length + 1);
               end if;
            end;
         end loop;

         for I in Doc_Vec.First_Index .. Last_Index loop
            declare
               L : XString renames Doc_Vec (I);
            begin

               --  Check that every character we're going to strip is a white
               --  space; else, raise an error.
               if not L.Is_Empty
                  and then (Offset >= L.Length
                            or else (for some C
                                     of L.Slice (1, Offset - 1)
                                     => not Is_Space (C)))
               then
                  raise Property_Error with "Incorrectly formatted docstring";
               end if;

               Ret.Doc.Append (L.Slice (Offset, L.Length));
               if I /= Last_Index then
                  Ret.Doc.Append (LF);
               end if;
            end;
         end loop;
      end;
      return Ret;
   end Extract_Doc_From;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation (Decl : Basic_Decl) return Doc_Type is
      Doc : Doc_Type;
   begin
      if Decl.Kind = Ada_Generic_Package_Internal then
         return Get_Documentation (Decl.Parent.As_Basic_Decl);
      elsif Decl.Kind in Ada_Base_Package_Decl | Ada_Generic_Package_Decl then
         --  Documentation for packages is assumed to appear before the
         --  "package" keyword.
         Doc := Extract_Doc_From
           (Decl.Token_Start, Backwards => True, Skip_White_Lines => -1);

         --  If not found and the package is a library unit, search before the
         --  prelude.
         if Doc.Doc = Null_XString and then Decl.P_Is_Compilation_Unit_Root
         then
            Doc := Extract_Doc_From
              (Decl.Unit.Root.Token_Start,
               Backwards        => True,
               Skip_White_Lines => -1);
         end if;

         return Doc;

      else
         --  Documentation for all other entities is assumed to appear after
         --  the node representing the entity.
         return Extract_Doc_From
           (Decl.Token_End, Backwards => False, Skip_White_Lines => 1);
      end if;
   end Get_Documentation;

end Libadalang.Doc_Utils;
