procedure Test is
   type Month is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
   subtype Warm_Month is Month range Jun .. Sep;

   type Employee is (Jan, Jen, Jim, Tim, Tom, Mary, Randy, Ike);

   Selector : Integer;
begin
   case (if Selector < 1 then Jan
         elsif Selector in 1..8 then Warm_Month'(Jun)
         else Dec) is
      when Jan => null;
      when Oct => null;
      when Dec => null;
      when others => null;
   end case;
   pragma Test_Statement;

   Selector := Integer (if True then 1 else Selector);
   pragma Test_Statement;
end Test;
