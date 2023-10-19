package Test is

   type Event_Record is tagged private;

   procedure Event_Occurred (Event : in out Event_Record)
      with Post => Event.Count1 = Event'Old.Count2 + 1;
   pragma Test_Block;

   function Count1 (Event : in Event_Record) return Integer;
   function Count2 (Event : in Event_Record) return Integer;

private

   type Event_Record is tagged record
      Id                : Integer := 0;
      No_Of_Occurrences : Integer := 0;
   end record;

end Test;
