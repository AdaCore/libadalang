procedure Test is
   task type T is
      entry Start;
   end T;

   task body T is
      type Rec;
      --% node.p_next_part
      type Rec_Access is access Rec;
      type Rec is record
         X : Rec_Access;
      end record;
      --% node.p_previous_part()
   begin
      accept Start;
   end T;
begin
   null;
end Test;
