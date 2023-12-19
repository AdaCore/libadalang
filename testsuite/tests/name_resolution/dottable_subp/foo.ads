package Foo is
   type Container is tagged null record;

   type T is new Container with null record;
   subtype Record_T is T;

   procedure Init (X : access Record_T'Class; Spacing : Integer := 0) is null;
end Foo;
