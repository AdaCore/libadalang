procedure Test is
   package Trees is
      type Tree is tagged null record with
        Constant_Indexing => Indexed_Element;

      function Has_Element (X : Integer) return Boolean is (False);

      function Indexed_Element (Self : Tree; Pos : Integer) return Tree
      is (Self);

      procedure Foo (X : Tree) is null;
   end Trees;

   X : Trees.Tree;
begin
   X (1).Foo;
   pragma Test_Statement;
end Test;

