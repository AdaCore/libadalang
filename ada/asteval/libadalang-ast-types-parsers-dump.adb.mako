## vim: ft=makoada

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

package body Libadalang.AST.Types.Parsers.Dump is

   ## Declarations for all Image functions

   % for cls in ctx.sorted_types(ctx.struct_types):
      function Image (S : ${cls.name()}) return String;
   % endfor

   % for cls in ctx.sorted_types(ctx.array_types):
      function Image (A : ${cls.name()}) return String;
   % endfor

   ## And now their implementations

   % for cls in ctx.sorted_types(ctx.struct_types):

      -----------
      -- Image --
      -----------

      function Image (S : ${cls.name()}) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "(");
         % for i, f in enumerate(cls.get_fields(include_inherited=True)):
            % if i > 0:
               Append (Result, ", ");
            % endif
            Append (Result, "${f.name} => ");
            Append (Result, Image (S.${f.name}));
         % endfor
         Append (Result, ")");
         return To_String (Result);
      end Image;

   % endfor

   % for cls in ctx.sorted_types(ctx.array_types):

      -----------
      -- Image --
      -----------

      function Image (A : ${cls.name()}) return String is
         Result : Unbounded_String;
      begin
         if A = null then
            return "<null>";
         end if;

         Append (Result, "(");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, I'Img & " => ");
            Append (Result, Image (A.Items (I)));
         end loop;
         Append (Result, ")");
         return To_String (Result);
      end Image;

   % endfor

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (V : Eval_Result) is
      use type AST_Envs.Lexical_Env;
   begin
      case V.Kind is
      when Boolean_Value =>
         Put_Line (Image (V.Bool));
      when Integer_Value =>
         Put_Line (Integer'Image (V.Int));

      % for cls in eval_types:
         when ${enum_for_type(cls)} =>
            Put_line (Image (V.${field_for_type(cls)}));
      % endfor

      when Ada_Node_Value =>
         if V.Node = null then
            Put_Line ("<null AST node>");
         else
            V.Node.Print;
         end if;
      when Token_Value =>
         Put_Line (Image (V.Tok));
      when Lexical_Env_Value =>
         if V.Lexical_Env = null then
            Put_Line ("<null lexical environment>");
         else
            Dump_One_Lexical_Env (V.Lexical_Env);
         end if;

      when Find_Builtin_Value =>
         Put_Line
           (".Find builtin method, bound to " & Short_Image (V.Find_Root));

      when Symbol_Value =>
         Ada.Wide_Wide_Text_IO.Put_Line (V.Symbol.all);

      when Error_Value =>
         raise Program_Error;
      end case;
   end Put_Line;

end Libadalang.AST.Types.Parsers.Dump;
