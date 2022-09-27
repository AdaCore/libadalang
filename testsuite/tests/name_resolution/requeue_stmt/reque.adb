procedure Reque is
   task type T (B : Boolean) is
      entry E2 (I : Integer; B : Boolean);
   private
      entry E1 (I : Integer; B : Boolean);
   end T;

   task body T is
   begin
      select
         accept E2 (I : Integer; B : Boolean) do
            requeue E1;
            pragma Test_Statement;
         end E2;
      or
         accept E1 (I : Integer; B : Boolean) do
            null;
         end E1;
      end select;
   end T;
begin
   null;
end Reque;
