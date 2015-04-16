## vim: filetype=makoada

${_self.gen_fn_name}_Memo : ${decl_type(_self.get_type())}_Memos.Memo_Type;

function ${_self.gen_fn_name} (Parser : in out Parser_Type;
                               Pos    : Integer)
                               return ${decl_type(_self.get_type())}
is
   % for name, typ in parser_context.var_defs:
      ${name} : ${decl_type(typ)}
         ${":=" + typ.nullexpr() if typ.nullexpr() else ""};
   % endfor

   % if _self.is_left_recursive():
      Mem_Pos : Integer := Pos;
      Mem_Res : ${decl_type(_self.get_type())} := ${_self.get_type().nullexpr()};
   % endif

   M       : ${decl_type(_self.get_type())}_Memos.Memo_Entry :=
      Get (${_self.gen_fn_name}_Memo, Pos);

begin

   if M.State = Success then
      Current_Pos := M.Final_Pos;
      ${parser_context.res_var_name} := M.Instance;
      return ${parser_context.res_var_name};
   elsif M.State = Failure then
      Current_Pos := -1;
      return ${parser_context.res_var_name};
   end if;

   % if _self.is_left_recursive():
       Set (${_self.gen_fn_name}_Memo,
            False,
            ${parser_context.res_var_name},
            Pos,
            Mem_Pos);

       <<Try_Again>>
   % endif

   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   ${parser_context.code}

   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

   % if _self.is_left_recursive():
      if ${parser_context.pos_var_name} > Mem_Pos then
         Mem_Pos := ${parser_context.pos_var_name};
         % if _self.needs_refcount():
            if Mem_Res /= null then
               Dec_Ref(Mem_Res);
            end if;
         % endif
         Mem_Res := ${parser_context.res_var_name};
         % if _self.needs_refcount():
            Inc_Ref (Mem_Res);
         % endif
         Set (${_self.gen_fn_name}_Memo,
              ${parser_context.pos_var_name} /= -1,
              ${parser_context.res_var_name},
              Pos,
              ${parser_context.pos_var_name});
         % if _self.needs_refcount():
            Inc_Ref (${parser_context.res_var_name});
         % endif
         goto Try_Again;

      elsif Mem_Pos > Pos then
         % if _self.needs_refcount():
            if ${parser_context.res_var_name} /= null then

               --  The following is an inelegant way to free this node

               Inc_Ref (${parser_context.res_var_name});
               Dec_Ref (${parser_context.res_var_name});
            end if;
         % endif

         % if _self.needs_refcount():
            Dec_Ref (Mem_Res);
         % endif
         ${parser_context.res_var_name} := Mem_Res;
         ${parser_context.pos_var_name} := Mem_Pos;
         goto No_Memo;
      end if;
   % endif

   Set (${_self.gen_fn_name}_Memo,
        ${parser_context.pos_var_name} /= -1,
        ${parser_context.res_var_name},
        Pos,
        ${parser_context.pos_var_name});
   % if _self.needs_refcount():
       if ${parser_context.res_var_name} /= null then
          Inc_Ref (${parser_context.res_var_name});
       end if;
   % endif

   % if _self.is_left_recursive():
       <<No_Memo>>
   % endif

   Current_Pos := ${parser_context.pos_var_name};

   return ${parser_context.res_var_name};
end ${_self.gen_fn_name};
