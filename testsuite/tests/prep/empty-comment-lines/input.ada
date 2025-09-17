#if X'Defined then
--  Block 1
#end if;

#if not X'Defined then
--  Block 2
#end if;

   #if Y'Defined
   --  Indented block 1
   # end if;

   #if not Y'Defined
   --  Indented block 2
   # end if;
