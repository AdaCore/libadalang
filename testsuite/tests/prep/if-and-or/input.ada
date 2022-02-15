procedure Foo is
begin

------------
-- Simple --
------------

#if F and F then
   null;
#end if;

#if F and T then
   null;
#end if;

#if T and F then
   null;
#end if;

#if T and T then
   null;
#end if;

#if F or F then
   null;
#end if;

#if F or T then
   null;
#end if;

#if T or F then
   null;
#end if;

#if T or T then
   null;
#end if;

------------------
-- Shortcircuit --
------------------

#if F and then No_Such_Symbol then
   null;
#end if;

#if T and then No_Such_Symbol then
   null;
#end if;

#if F or else No_Such_Symbol then
   null;
#end if;

#if T or else No_Such_Symbol then
   null;
#end if;

-------------------
-- Valid chained --
-------------------

#if T and T and T then
   null;
#end if;

#if T or T or T then
   null;
#end if;

#if F or T or else No_Such_Symbol then
   null;
#end if;

#if T and F and then No_Such_Symbol then
   null;
#end if;

---------------------
-- Invalid chained --
---------------------

#if T and T or T then
   null;
#end if;

#if not T and T then
   null;
#end if;

#if T or not T then
   null;
#end if;

end Foo;
