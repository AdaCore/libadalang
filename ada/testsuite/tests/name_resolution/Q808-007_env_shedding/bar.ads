generic
   type Counter is private;
package Bar is

   Z : Counter;

   generic
      Init : Counter;
   package Gen is
      Count : Counter := Counter (Init);
   end;

end Bar;
