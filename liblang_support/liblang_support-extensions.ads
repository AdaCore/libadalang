package Liblang_Support.Extensions is

   type Extension_ID is new Natural;

   function Register_Extension (Name : String) return Extension_ID;

end Liblang_Support.Extensions;
