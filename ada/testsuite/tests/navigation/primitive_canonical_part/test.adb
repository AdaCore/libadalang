procedure Test is
   package P is
      type T is tagged null record;

      procedure Proc (X : T);

      procedure Proc_2 (X : T);
   private
      procedure Proc (X : T) is null;
      --% node.p_canonical_part

      procedure Proc_2 (X : T) is null;
      --% node.p_canonical_part
   end P;

   package Q is
      type T is new P.T with null record;

      overriding procedure Proc_2 (X : T);
   private
      overriding procedure Proc_2 (X : T) is null;
      --% node.p_canonical_part
   end Q;

   X : Q.T;
begin
   Q.Proc (X);
   --% node.f_call.f_name.p_referenced_decl().p_canonical_part

   Q.Proc_2 (X);
   --% node.f_call.f_name.p_referenced_decl().p_canonical_part
end Test;
