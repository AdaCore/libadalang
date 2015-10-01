package Langkit_Support.Extensions is

   type Extension_ID is new Natural;

   function Register_Extension (Name : String) return Extension_ID;

   function Has_Extensions return Boolean;

end Langkit_Support.Extensions;
