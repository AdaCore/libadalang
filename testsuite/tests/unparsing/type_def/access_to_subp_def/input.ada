procedure Find_Matching_Parents
  (Match_1 : access protected function (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean;
   Match_2 : not null access protected function (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean);
