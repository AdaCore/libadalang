procedure Test is
   procedure P is null;

   type Month is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
   type Employee is (Jan, Jen, Jim, Tim, Tom, Mary, Randy, Ike);

   Selector : Natural := 1;
begin
   case (case Selector is
            when 0      => Jan,
            when 1..8   => Oct,
            when others => Dec) is
      when Jan    => P;
      when others => P;
   end case;
   pragma Test_Statement;

   case (case Selector is
            when 0      => Jan,
            when 1..8   => Jen,
            when others => Jim) is
      when Jan    => P;
      when others => P;
   end case;
   pragma Test_Statement;
end Test;
