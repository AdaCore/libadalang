## vim: filetype=makoada

--  Start list_code

## If we accept empty lists, then we never want to return -1 as a position
## TODO: This is weird because:
## 1. With those semantics, list(empty_valid=true) is equivalent to Opt(list),
##    so it might be better to compile it as such.
## 2. An empty list result should probably result in an empty list, not in a
##    null result.
% if _self.empty_valid:
    ${pos} := ${pos_name};
% else:
    ${pos} := -1;
% endif

${res} := ${_self.get_type().nullexpr()};
${cpos} := ${pos_name};

loop
   ## Parse one list element
   ${parser_context.code}

   ## Stop as soon as we cannot parse list elements anymore
   exit when ${parser_context.pos_var_name} = -1;

   ${pos} := ${parser_context.pos_var_name};
   ${cpos} := ${parser_context.pos_var_name};

   ## The revtree option allows to parse a list as a tree of node so that
   ## for example the following rule, given a simple revtree class having
   ## two fields::
   ##
   ##    List(id, sep=".", revtree=Simple) -> A.B.C.D
   ##
   ## will produce the tree::
   ##
   ##    Simple(
   ##       left  = Simple(
   ##          left  = Simple(
   ##             left  = A
   ##             right = B
   ##          )
   ##          right = C
   ##       )
   ##       right = D
   ##    )
   ##
   ## Algorithmically, the process is simple: Everytime you parse a new
   ## result, you create a new node, and store the previous result as the
   ## left field and the new result as the right field. You then store this
   ## node as the previous result.

   % if _self.revtree_class:

      ## If the current result is null, this is the first result. Store it.
      if ${res} = ${_self.get_type().nullexpr()} then
         ${res} := ${_self.get_type().name()} (${parser_context.res_var_name});

      ## Else, fold the current and previous results into a new node
      else
         declare
            <% tree_class = _self.revtree_class.name() %>

            ## Create the node which will contain current and previous results
            New_Res : ${tree_class} := ${tree_class}
              (${tree_class}_Alloc.Alloc (Parser.Mem_Pool));
         begin
            <%
            field_0, field_1 = list(
               _self.revtree_class.get_fields(include_inherited=False)
            )
            %>
            ## Set left children of node to the previously accumulated result
            New_Res.${field_0.name} := ${res};

            ## Set right children of node to just parsed result
            New_Res.${field_1.name} :=
               ${_self.get_type().name()} (${parser_context.res_var_name});

            ## Set the parent of both children to the created node
            ${res}.Parent := AST_Node (New_Res);
            ${parser_context.res_var_name}.Parent := AST_Node (New_Res);

            ## Store node as previously accumulated result
            ${res} := ${_self.get_type().name()} (New_Res);
         end;

         ## Set token data for result
         ${res}.Token_Data := Parser.TDH;
         ${res}.Token_Start := ${pos_name};
         ${res}.Token_End := (if ${cpos} = ${pos_name}
                              then ${pos_name}
                              else ${cpos} - 1);
      end if;

   ## This corresponds to the regular case in which a list is parsed and
   ## stored in a vector of nodes, in a flat fashion.
   % else:

      ## Related to TODO above, we create the list lazily only when the first
      ## result has been parsed.
      <% parser_type = decl_type(_self.parser.get_type()) %>

      if ${res} = ${_self.get_type().nullexpr()} then
         ${res} := List_${parser_type}
           (List_${parser_type}_Alloc.Alloc (Parser.Mem_Pool));

         ${res}.Vec :=
           Lists_${parser_type}.Node_Vectors.Create (Parser.Mem_Pool);
      end if;

      ## Append the parsed result to the list
      Lists_${decl_type(_self.parser.get_type())}.Node_Vectors.Append
        (${res}.Vec, ${parser_context.res_var_name});

      ## If we are parsing nodes, then set the parent of parsed node to the
      ## list, and increment its ref count.
      % if is_ast_node (_self.parser.get_type()):
         if ${parser_context.res_var_name} /= null then
            ${parser_context.res_var_name}.Parent := AST_Node (${res});
         end if;
      % endif
   % endif

   ## Parse the separator, if there is one. The separator is always discarded.
   % if _self.sep:
      ${sep_context.code}
      if ${sep_context.pos_var_name} /= -1 then
          ${cpos} := ${sep_context.pos_var_name};
      else
         ## If we didn't successfully parse a separator, exit
         exit;
      end if;
   % endif

end loop;

## If we managed to parse a list, compute and set the sloc range for this AST
## node.
if ${res} /= null then
   ${res}.Token_Data := Parser.TDH;
   ${res}.Token_Start := ${pos_name};
   ${res}.Token_End := (if ${cpos} = ${pos_name}
                        then ${pos_name}
                        else ${cpos} - 1);
end if;


--  End list_code
