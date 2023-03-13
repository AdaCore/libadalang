procedure Kind is
   package P is
      type T is tagged null record;
      type T2 is new T with record
         A, B : Integer;
      end record;
   end P;

   V : P.T'Class := P.T2'(1, 2);
   V2 : P.T'Class := V;
   V3 : P.T'Class := V;

   type A is array (Natural range <>) of Integer;
   E : A (1 .. 4) := (1, 2, 3, 4);
   G : A (1 .. 2);

   Q : Integer;

   procedure P1 (A : Integer) is
   begin
      null;
   end P1;

   function F1 (A : Integer) return Boolean is
   begin
      P1 (A);
      --% node.f_call.p_kind
      return True;
   end F1;

   Y : Boolean;
   N : Natural;
begin
   P.T2 (V3).A := P.T2 (V2).A;
   --% node.f_dest.f_prefix.p_kind
   --% node.f_expr.f_prefix.p_kind

   Q := Integer (2.5);
   --% node.f_expr.p_kind

   Q := E(1);
   --% node.f_expr.p_kind

   G := E(1 .. 2);
   --% node.f_expr.p_kind

   Y := F1 (1);
   --% node.f_expr.p_kind

   N := Natural (Q);
   --% node.f_expr.p_kind
end Kind;
