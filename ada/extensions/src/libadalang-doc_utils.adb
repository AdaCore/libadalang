with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Vectors;

with Libadalang.Common; use Libadalang.Common;

package body Libadalang.Doc_Utils is

   use XStrings;

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
      LF  : Wide_Wide_Character := To_Wide_Wide_Character (ASCII.LF);

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
      if Skip_White_Lines /= 0 and then Kind (Data (Tok)) = Ada_Whitespace then
         if Skip_White_Lines = -1 then

            --  If told to skip all white lines, go ahead

            Next_Token;

         elsif Skip_White_Lines > 0 then

            --  If told to skip a certain number of white lines, verify that
            --  the next token indeed contains said number of white lines.

            T := To_XString (Text (Tok));

            if T.Count (LF) = Skip_White_Lines then
               Next_Token;
            end if;
         end if;
      end if;

      --  No comment in the direction expected? There is no doc!

      if Kind (Data (Tok)) /= Ada_Comment then
         return Ret;
      end if;

     --  Process as many comments as possible from our starting point,
     --  until we find an empty line or anything else than a comment or
     --  a whitespace.

      while Tok /= No_Token  loop
         K := Kind (Data (Tok));
         case K is
            when Ada_Whitespace =>
               T := To_XString (Text (Tok));
               exit when T.Count (LF) > 1;

            when Ada_Comment =>
               T := To_XString (Text (Tok));
               T := T.Slice (3, T.Length);

               --  If this is an annotation then
               if T.Starts_With ("%")  then
                  declare
                     --  Try to split on the ":"
                     X : XString_Array := T.Split (":");
                     K : Wide_Wide_String :=
                       X (1).Slice (3, X (1).Length).To_String;
                     --      ^ Strip % prefix
                     V : Wide_Wide_String := X (2).Trim.To_String;
                  begin
                     Ret.Annotations.Include (K, V);
                  exception
                     when Constraint_Error =>
                        raise Property_Error
                          with "Improper format for annotation";
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
         Last_Index : Natural := Doc_Vec.Last_Index;
      begin
         for I in Doc_Vec.First_Index .. Doc_Vec.Last_Index loop
            declare
               L : XString renames Doc_Vec (I);
            begin
               Ret.Doc.Append (L.Slice (3, L.Length));
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

   function Get_Documentation
     (Decl : Basic_Decl) return Doc_Type
   is
   begin
      if Decl.Kind in Ada_Base_Package_Decl
        and then Decl.Kind /= Ada_Generic_Package_Decl
      then
         return Extract_Doc_From
           (Decl.Token_Start, Backwards => True, Skip_White_Lines => -1);
      else
         return Extract_Doc_From
           (Decl.Token_End, Backwards => False, Skip_White_Lines => 1);
      end if;
   end Get_Documentation;

end Libadalang.Doc_Utils;
