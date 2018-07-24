package Base is
   generic
      type T is private;
   package Gen_List is
      type List is private;
      procedure Append (Self : in out List; Item : T);
   private
      type List is record
         Item : T;
      end record;
   end Gen_List;
end Base;
