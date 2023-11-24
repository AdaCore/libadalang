procedure Derived is
   task type T is
   end T;

   task body T is
   begin
      null;
   end T;

   subtype ST is T;
   type NT is new T;

   procedure Kill (X : ST) is
   begin
      abort X;
      pragma Test_Statement;
   end Kill;

   procedure Kill (X : NT) is
   begin
      abort X;
      pragma Test_Statement;
   end Kill;

begin
   null;
end Derived;
