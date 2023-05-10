open Libadalang

(* Test that the auto unit provider correclty works and Libadalang is able to
 * get the reference of a node that is declared in another unit *)

let print_idents ctx =
  let filename = "input.ada" in
  let u = AnalysisContext.get_from_file ctx filename in
  match AnalysisUnit.root u with
  | Some n ->
      Format.printf "%s@." (AdaNode.text n)
  | None ->
      Format.printf "@[<v>Cannot get root node for file %s@ @]" filename ;
      exit 1

let test1 ?line_mode () =
  let project = GPRProject.load "prj.gpr" in
  let unit_provider = UnitProvider.gpr_project project in
  let file_reader = GPRProject.create_preprocessor ?line_mode project in
  let ctx = AnalysisContext.create ~unit_provider ~file_reader () in
  print_idents ctx

let test2 ?line_mode () =
  let project = GPRProject.load ~scenario_vars:[("mode", "prod")] "prj.gpr" in
  let unit_provider = UnitProvider.gpr_project project in
  let file_reader = GPRProject.create_preprocessor ?line_mode project in
  let ctx = AnalysisContext.create ~unit_provider ~file_reader () in
  print_idents ctx

let () =
  Format.printf "==========@." ;
  test1 () ;
  Format.printf "==========@." ;
  test2 () ;
  Format.printf "==========@." ;
  test1 ~line_mode:BlankLines () ;
  Format.printf "==========@." ;
  test2 ~line_mode:BlankLines () ;
  Format.printf "==========@." ;
  test1 ~line_mode:CommentLines () ;
  Format.printf "==========@." ;
  test2 ~line_mode:CommentLines () ;
  Format.printf "==========@."
