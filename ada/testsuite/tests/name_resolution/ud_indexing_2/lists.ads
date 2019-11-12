package Lists is
   type List is tagged null record
      with Variable_Indexing => Get;

   type Ref_Type (E : access Integer) is null record
      with Implicit_Dereference => E;

   function Get (Self : List; I : Integer) return Ref_Type is (E => null);
end Lists;
