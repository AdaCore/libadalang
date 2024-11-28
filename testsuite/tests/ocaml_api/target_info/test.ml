open Libadalang

let (>>) a f =
  match a with
  | Some x ->
      f x
  | None ->
      assert false

let eval_as_int u =
  match AnalysisUnit.root u >> AdaNode.findall AttributeRef with
  | [result] ->
      Expr.p_eval_as_int result |> Z.to_string
  | _ ->
      assert false

let test_filename filename =
  Format.printf "== %s ==\n@." filename ;
  match TargetInformation.load filename with
  | info ->
      let ctx = AnalysisContext.create () in
      let u = AnalysisContext.get_from_file ctx "pkg.ads" in
      Format.printf "Before setting the target: %s@." (eval_as_int u);
      TargetInformation.set ctx info ;
      Format.printf "After setting the target: %s\n@." (eval_as_int u)
  | (exception (InvalidInput exc)) ->
      Format.printf "Got an InvalidInput exception: %s\n@." exc

let filenames = ["no_such_file.txt"; "invalid.txt"; "small_ints.txt"]

let () = List.iter test_filename filenames
