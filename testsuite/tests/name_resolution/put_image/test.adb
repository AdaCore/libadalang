with Ada.Strings.Text_Buffers; use Ada.Strings.Text_Buffers;

procedure Test is
   package With_Attribute_Def is
      type T is null record;

      --  introduce dummy overloads around the right subprogram to make sure
      --  we resolve it correctly.
      procedure Foo (X : in out Root_Buffer_Type'Class; V: Integer) is null;
      procedure Foo (X : in out Root_Buffer_Type'Class; V: T) is null;
      procedure Foo (X : in out Root_Buffer_Type'Class; V: Float) is null;

      for T'Put_Image use Foo;
   end With_Attribute_Def;

   package With_Aspect is
      type T is null record
         with Put_Image => Foo;

      --  introduce dummy overloads around the right subprogram to make sure
      --  we resolve it correctly.
      procedure Foo (X : in out Root_Buffer_Type'Class; V: Integer) is null;
      procedure Foo (X : in out Root_Buffer_Type'Class; V: T) is null;
      procedure Foo (X : in out Root_Buffer_Type'Class; V: Float) is null;
   end With_Aspect;

   package Unknown is
      type T is null record;
   end Unknown;

   procedure Main (B : in out Root_Buffer_Type'Class) is
      X : With_Attribute_Def.T;
      Y : With_Aspect.T;
      Z : Unknown.T;
   begin
      With_Attribute_Def.T'Put_Image (B, X);
      pragma Test_Statement;

      With_Aspect.T'Put_Image (B, Y);
      pragma Test_Statement;

      Unknown.T'Put_Image (B, Z);
      pragma Test_Statement;
   end Main;
begin
   null;
end Test;
