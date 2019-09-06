package Root.P is
private
   type T;

   procedure F (X : access T);

   type T is null record;
end Root.P;

