package body Test is

   procedure Event_Occurred (Event : in out Event_Record) is null;

   function Count1 (Event : in Event_Record) return Integer is (1);
   function Count2 (Event : in Event_Record) return Integer is (1);

end Test;
