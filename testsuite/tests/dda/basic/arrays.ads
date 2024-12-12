package Arrays is
   type Bool_Array is array (Positive range <>) of Boolean;
   subtype Bool_Couple is Bool_Array (1 .. 2);
   Obj : array (Boolean) of Character;
   type New_Bool_Couple is new Bool_Couple;
end Arrays;
