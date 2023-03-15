procedure Test is
   package Queue_Element_G is
      type Queue_Obj_T is tagged private;

      generic
         type Obj_T is new Queue_Obj_T with private;
      package Inner_G is
         procedure Reinit (Element : in out Obj_T);
      end Inner_G;

   private
      type Queue_Obj_T is tagged record
         Npl : Natural := 0;
      end record;
   end Queue_Element_G;

   package body Queue_Element_G is
      package body Inner_G is

         procedure Reinit (Element : in out Obj_T) is
         begin
            Element.Npl := 0;
            pragma Test_Statement;
         end Reinit;

      end Inner_G;
   end Queue_Element_G;
begin
   null;
end Test;
