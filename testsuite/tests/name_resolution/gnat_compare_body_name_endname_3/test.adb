procedure Test is
   package P is
      type T;
      type U is limited private;

      task type T is
      end T; -- refers to T line 6
   private
      task type U is
      end U; -- refers to U line 4
   end P;

   package body P is
      task body T is -- refers to T line 6
      begin
         null;
      end T; -- refers to T line 6

      task body U is -- refers to U line 4
      begin
         null;
      end U; -- refers to U line 4
   end P;
   use P;

   My_T : T;  -- refers to T line 3
begin
   null;
end;
