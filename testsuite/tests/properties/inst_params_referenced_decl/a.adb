procedure A is
   Y : Integer := 42;

   generic
      X : in Integer := Y;
      Y : in out Integer;
   procedure P;

   procedure P is
   begin
      null;
   end P;

   procedure Inst is new P (Y => Y);
   --% p = node.p_inst_params
   --% p[0].actual.p_referenced_decl()
   --% p[1].actual.p_referenced_decl()

   generic
      X : Integer;
      Y : Integer := X;
   procedure G;

   procedure G is
   begin
      null;
   end G;

   procedure IG is new G (X => 42);
   --% p = node.p_inst_params
   --% p[0].actual.p_referenced_decl()
   --% p[1].actual.p_referenced_decl()
begin
   null;
end A;
