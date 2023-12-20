procedure Test is
   package Pkg is
      type T is synchronized interface;
      procedure Pent (Self : T; Y : Integer) is abstract
         with Synchronization => By_Entry;
   end Pkg;
   type U is access all Pkg.T'Class;

   protected Pro is
      entry Ent (X : Integer);
   private
      O : U;
   end Pro;

   protected body Pro is
      entry Ent (X : Integer) when True is
      begin
         requeue O.Pent;
         pragma Test_Statement;
      end Ent;
   end Pro;

   task type T is
      entry Ent (X : Integer);
      entry Fnt (X : Integer);
   end T;

   task body T is
      procedure Pent (Y : Integer) renames Ent;
   begin
      accept Fnt (X : Integer) do
         requeue Pent;
         pragma Test_Statement;
      end Fnt;
   end T;

begin
   null;
end Test;
