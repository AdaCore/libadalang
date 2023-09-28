procedure Test is
   package Pkg is
      type T is tagged private;

      type T_Access is access all T'Class;
      --% typ = node.f_type_def.f_subtype_indication.p_designated_type_decl
      --% typ.p_next_part
   private
      type T is tagged null record;
   end Pkg;
begin
end Test;
