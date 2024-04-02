procedure Family_Index is
   subtype Index_T is Positive range 1 .. 2;

   protected T is
      entry E (I : Integer);
      entry F (Index_T) (I : Integer);
   end T;

   protected body T is
      entry E (I : Integer) when True is
      begin
         requeue F (I);
         --% node.f_call_name.p_kind
      end E;

      entry F (for I in Index_T) (I : Integer) when False is
      begin
         null;
      end F;
   end T;

begin
   T.F (1) (3);
   --% node.f_call.p_kind
   --% node.f_call.f_name.p_kind
end Family_Index;
