open Libadalang

(* Test that the auto unit provider correclty works and Libadalang is able to
 * get the reference of a node that is declared in another unit *)

let () =
  let unit_provider = UnitProvider.auto ["src1/p1.ads"; "p2.ads"] in
  let ctx = AnalysisContext.create ~unit_provider () in
  let filename = "p2.ads" in
  let u = AnalysisContext.get_from_file ctx filename in
  let root =
    match AnalysisUnit.root u with
    | Some n ->
        n
    | None ->
        Format.printf "@[<v>Cannot get root node for file %s@ @]" filename ;
        exit 1
  in
  let subtype_indication = AdaNode.find SubtypeIndication root in
  let matching_nodes =
    SubtypeIndication.f_name subtype_indication |> Expr.p_matching_nodes
  in
  let pp_node fmt node =
    Format.pp_print_string fmt (AdaNode.image node)
  in
  Format.printf "@[<v>For @[<v 2>%a resolves to:@ %a@]@ @]" pp_node
    subtype_indication
    (Format.pp_print_list pp_node)
    matching_nodes
