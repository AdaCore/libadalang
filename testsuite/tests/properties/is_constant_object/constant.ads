generic
   type T is private;
   X : in out T;
   --% node.p_is_constant_object
   Y : in T;
   --% node.p_is_constant_object
procedure P
  (N : Integer;
   M : in Integer;
   O : in out Integer;
   P : out Integer);
   --% [x.p_is_constant_object for x in node.f_subp_decl.f_subp_spec.p_params]
