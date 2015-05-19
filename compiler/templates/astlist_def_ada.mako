## vim: filetype=makoada

% if not private_part:

   type List_${decl_type(element_type)}_Type is new AST_Node_Type with private;
   type List_${decl_type(element_type)} is
      access all List_${decl_type(element_type)}_Type'Class;

   procedure Inc_Ref (Node : in out List_${decl_type(element_type)});
   pragma Inline (Inc_Ref);

   procedure Dec_Ref (Node : in out List_${decl_type(element_type)});
   pragma Inline (Dec_Ref);

% else:

   package Lists_${decl_type(element_type)} is new Liblang_Support.AST.List
     (Node_Type   => ${decl_type(element_type)}_Type,
      Node_Access => ${decl_type(element_type)});

   type List_${decl_type(element_type)}_Type is
      new Lists_${decl_type(element_type)}.List_Type with null record;

% endif
