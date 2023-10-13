[@@@ocaml.warning "-32"]

let display_graphs = false

let to_dot =
  if display_graphs then
    History.Dot.to_dot
  else
    fun _ _ -> ()

let () = Format.printf "@."

let test process =

  let initial = History.create () in

  let chain1 =
    initial
    |> process "A white rabbit"
    |> process " jumped into a hole"
  in

  let () = to_dot [initial] "chain1.dot" in

  let chain2 =
    chain1
    |> process " filled with carrots"
  in

  let () = to_dot [initial] "chain2.dot" in

  let chain3 =
    chain2
    |> process " of all shapes."
  in

  let () = to_dot [initial] "chain3.dot" in

  let chain4 =
    chain1
    |> process " under the tree."
  in

  let () = to_dot [initial; chain1; chain2; chain3; chain4 ] "chain4.dot" in

  let chain5 =
    chain2
    |> process " and other veggies."
  in

  let () = to_dot [initial; chain1; chain2; chain3; chain4; chain5 ] "chain5.dot" in

  let chain6 =
    initial
    |> process "What's up doc"
  in

  let () = to_dot [initial; chain1; chain2; chain3; chain4; chain5; chain6 ] "chain6.dot" in

  let chain7 =
    chain5
    |> process " But he wasn't that hungry."
  in

  let () = to_dot [initial; chain1; chain2; chain3; chain4; chain5; chain6; chain7 ] "chain7.dot" in

  ()


let infinite_context _ctxt _n_common s =
  Format.printf "Unbounded  \"%s\"@." s ;
  10

let () = test (History.process_chunk infinite_context ())

let context_swapping _ctxt n_common s =
  Format.printf "Bounded(10) \"%s\"@." s ;
  let len = String.length s in
  if n_common + len >= 10 then
    Int.min n_common 5
  else
    n_common + len

let () = test (History.process_chunk context_swapping ())
