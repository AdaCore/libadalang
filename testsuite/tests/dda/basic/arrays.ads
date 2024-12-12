package Arrays is
   type Bool_Array is array (Positive range <>) of Boolean;
   subtype Bool_Couple is Bool_Array (1 .. 2);
end Arrays;
