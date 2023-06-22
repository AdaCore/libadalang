package body Test4 is
   pragma SPARK_Mode (On);

   procedure P is null;

   procedure Q is
      pragma SPARK_Mode;

      procedure K;
      pragma SPARK_Mode (Off);

      procedure K is
         pragma SPARK_Mode (Off);
      begin
         null;
      end K;

   begin
      declare
         procedure QQ is
         begin
            null;
         end QQ;
      begin
         declare
            procedure QQQ is null;
         begin
            null;
         end;
      end;
   end Q;

   package body Inn is
      procedure P is
         package M is
            procedure K;
         end M;

         package body M is
            procedure K is null;
         end M;
      begin
         null;
      end P;
   end Inn;
begin
   pragma SPARK_Mode (On);

   declare
      procedure P;

      procedure P is null;
   begin
       P;
   end;

end Test4;
