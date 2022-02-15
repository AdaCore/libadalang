procedure Foo is
begin
---------------
-- Undefined --
---------------

#if Undef_Sym < 1 then
   null;
#end if;

--------------------------
-- Compare with literal --
--------------------------

#if Int_Sym > 0 then
   null;
#end if;

#if Int_Sym > 20 then
   null;
#end if;

#if Int_Sym >= 0 then
   null;
#end if;

#if Int_Sym >= 10 then
   null;
#end if;

#if Int_Sym < 5 then
   null;
#end if;

#if Int_Sym < 15 then
   null;
#end if;

#if Int_Sym <= 5 then
   null;
#end if;

#if Int_Sym <= 10 then
   null;
#end if;

-------------------------------
-- Compare with other symbol --
-------------------------------

#if Int_Sym < Other_Int_Sym then
   null;
#end if;

#if Int_Sym > Other_Int_Sym then
   null;
#end if;

-------------------------
-- Compare with string --
-------------------------

#if Int_Sym < "bar" then
   null;
#end if;

#if Int_Sym < Str_Sym then
   null;
#end if;

end Foo;
