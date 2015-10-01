## vim: filetype=makoada

<% type = decl_type(element_type) %>

% if not private_part:

   type List_${type}_Type is new AST_Node_Type with private;
   type List_${type} is
      access all List_${type}_Type'Class;

% else:

   package Lists_${type} is new Langkit_Support.AST.List
     (Node_Type   => ${type}_Type,
      Node_Access => ${type});

   type List_${type}_Type is
      new Lists_${type}.List_Type with null record;

   type List_${type}_Access is access all List_${type}_Type;

   package List_${type}_Alloc is
     new Tagged_Alloc (List_${type}_Type, List_${type}_Access);

% endif
