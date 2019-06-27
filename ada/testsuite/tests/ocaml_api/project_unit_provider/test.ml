open Libadalang

let () =
  let load_project name =
    try ignore (UnitProvider.for_project name : UnitProvider.t)
    with InvalidProjectError s ->
      Format.printf "@[<v>got InvalidProjectError %s@ @ @]" s
  in
  (* Try to load a project that is not valid *)
  load_project "invalid.gpr" ;
  (* Try to load a project that does not exist *)
  load_project "does_not_exist.gpr"

(* Test that the unit provider correclty works and Libadalang is able to
 * get the reference of a node that is declared in another unit *)

let unit_provider = UnitProvider.for_project "p.gpr"

let ctx = AnalysisContext.create ~unit_provider ()

let filename = "p2.ads"

let u = AnalysisContext.get_from_file ctx (Filename.concat "src" filename)

let root =
  match AnalysisUnit.root u with
  | Some n ->
      n
  | None ->
      Format.printf "@[<v>Cannot get root node for file %s@ @]" filename ;
      exit 1

let subtype_indication = AdaNode.find SubtypeIndication root

let matching_nodes =
  SubtypeIndication.f_name subtype_indication |> Expr.p_matching_nodes

let pp_node fmt node = Format.pp_print_string fmt (AdaNode.short_image node)

let () =
  Format.printf "@[<v>@[<v 2>%a resolves to:@ %a@]@ @]" pp_node
    subtype_indication
    (Format.pp_print_list pp_node)
    matching_nodes
