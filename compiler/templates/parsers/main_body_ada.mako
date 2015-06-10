## vim: filetype=makoada

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Packrat;

with Libadalang.Lexer; use Libadalang.Lexer;

package body ${_self.ada_api_settings.lib_name}.Parsers is

   --  Prepare packrat instantiations: one per enum type and onefor each kind
   --  of node (including lists).

   % for enum_type in _self.enum_declarations:
      package ${enum_type.type.name()}_Memos is new Langkit_Support.Packrat
        (${enum_type.type.name()});
      use ${enum_type.type.name()}_Memos;
   % endfor

   % for cls in _self.astnode_types:
      package ${cls.name()}_Memos is new Langkit_Support.Packrat
        (${cls.name()}, Dec_Ref);
      use ${cls.name()}_Memos;

      % if cls in _self.list_types:
         package List_${cls.name()}_Memos is new Langkit_Support.Packrat
           (List_${cls.name()}, Dec_Ref);
         use List_${cls.name()}_Memos;
      % endif
   % endfor

   % for parser in _self.generated_parsers:
   ${parser.spec}
   % endfor

   function Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True) return Boolean;
   --  Helper for the user parsing function, to be called after a low-level
   --  parsing function. Check_Complete has the same semantics as in Parse. If
   --  the parsing failed (Parser.Current_Pos = -1), append corresponding
   --  diagnostics to Parser.Diagnostics, do nothing instead. Return whether
   --  the parsing failed in the first place.

   ----------------------
   -- Create_From_File --
   ----------------------

   function Create_From_File (Filename : String;
                              TDH      : Token_Data_Handler_Access)
                              return Parser_type
   is
   begin
      Lex_From_Filename (Filename, "", TDH.all);
      return (TDH => TDH, others => <>);
   end Create_From_File;

   ------------------------
   -- Create_From_Buffer --
   ------------------------

   function Create_From_Buffer (Buffer : String;
                                TDH    : Token_Data_Handler_Access)
                                return Parser_type
   is
   begin
      Lex_From_Buffer (Buffer, TDH.all);
      return (TDH => TDH, others => <>);
   end Create_From_Buffer;

   ---------------------------
   -- Process_Parsing_Error --
   ---------------------------

   function Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True) return Boolean
   is
   begin
      if Parser.Current_Pos = -1 then
         declare
            Last_Token : Token renames
               Get_Token (Parser.TDH.all, Parser.Last_Fail.Pos);
            D : constant Diagnostic :=
              (Sloc_Range => Last_Token.Sloc_Range,
               Message    => To_Unbounded_String
                 ("Expected """
                  & Token_Text (Parser.Last_Fail.Expected_Token_Id)
                  & """, got """
                  & Token_Text (Parser.Last_Fail.Found_Token_Id)
                  & """"));
         begin
            Parser.Diagnostics.Append (D);
            return True;
         end;
      elsif Check_Complete
        and then (Parser.Current_Pos
                  /= Token_Vectors.Last_Index (Parser.TDH.Tokens))
      then
         declare
            First_Garbage_Token : Token renames
              Get_Token (Parser.TDH.all, Parser.Current_Pos);
            D : constant Diagnostic :=
              (Sloc_Range => First_Garbage_Token.Sloc_Range,
               Message    => To_Unbounded_String
                 ("End of input expected, got """
                  & Token_Text (First_Garbage_Token.Id)
                  & """"));
         begin
            Parser.Diagnostics.Append (D);
         end;
      end if;
      return False;
   end Process_Parsing_Error;

   -----------
   -- Parse --
   -----------

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True) return AST_Node
   is
   begin
      return AST_Node
        (Parse_${_self.rules_to_fn_names[_self.main_rule_name]._name}
           (Parser, Check_Complete));
   end Parse;

   ## Generate user wrappers for all parsing rules
   % for rule_name, parser in sorted(_self.rules_to_fn_names.items()):
      function Parse_${parser._name}
        (Parser         : in out Parser_Type;
         Check_Complete : Boolean := True)
         return ${decl_type(parser.get_type())}
      is
         Result : ${decl_type(parser.get_type())} :=
            ${parser.gen_fn_name} (Parser, 0);
      begin
         if not Process_Parsing_Error (Parser, Check_Complete) then
            % if is_ast_node(parser.get_type()):
               Inc_Ref (Result);
            % else:
               null;
            % endif
         end if;
         Clean_All_Memos;
         return Result;
      end Parse_${parser._name};
   % endfor

   % for parser in _self.generated_parsers:
   ${parser.body}
   % endfor

   ---------------------
   -- Clean_All_Memos --
   ---------------------

   procedure Clean_All_Memos is
   begin
      % for fn in _self.fns:
         Clear (${fn}_Memo);
      % endfor
   end Clean_All_Memos;

end ${_self.ada_api_settings.lib_name}.Parsers;
