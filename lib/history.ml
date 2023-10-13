type chunk = string

type desc =
  | End
  | Done of { uid : int ;
              n_common : int ; (** Number of tokens in common with the preceding context. *)
              chunk : chunk ;
              past : chunk list ;
              mutable next : t }
  | Redo of { uid : int ;
              chunk : chunk ;
              next : t }

and t = desc ref

let gen =
  let x = ref 0 in
  fun () ->
    let v = !x in
    incr x;
    v

let chunk_equal (c1 : chunk) (c2 : chunk) =
  String.equal c1 c2

let rec get_n_common_and_reverse prev_desc_ref desc_ref acc k =
  match !desc_ref with
  | Redo _ ->
    (* Impossible because a [Done] node may only point to a [End] or [Done] node. *)
    assert false
  | End ->
    k () ;
    acc
  | Done { uid = _ ; past = _ ; n_common ; next ; chunk } ->
    let acc = Int.min acc n_common in
    get_n_common_and_reverse desc_ref next acc (fun () ->
        k () ;
        desc_ref := Redo { uid = gen () ; chunk ; next = prev_desc_ref }
      )

let create () =
  ref (Done { uid = gen () ; n_common = 0 ; chunk = "" ; past = []; next = ref End })

let rec process_chunk perform_inference ctxt new_chunk base =
  match !base with
  | End ->
    (* Impossible because users are only provided references to [Done] or [Redo]. *)
    assert false
  | Done ({ uid = _; n_common; chunk; past; next } as payload) ->
    (* Invariant: at this point, the chain of nodes reachable from [next] only contains
       [Done] nodes. *)
    let n_common = get_n_common_and_reverse base next n_common (fun () -> ()) in
    (* Invariant: at this point, [next] is rewritten to point to a [Redo] node. *)
    let n_common = perform_inference ctxt n_common new_chunk in
    let hist_elt =
      Done { uid = gen ();
             n_common;
             chunk = new_chunk;
             past = (chunk :: past);
             next = ref End }
    in
    payload.next <- ref hist_elt ;
    payload.next
  | Redo _ ->
    let rec get_root desc_ref acc =
      match !desc_ref with
      | End -> assert false
      | Done { uid = _; chunk = _; past = _; n_common = _; next } ->
        max_prefix desc_ref next acc
      | Redo { uid = _; chunk; next } ->
        get_root next (chunk :: acc)
    and max_prefix prev_desc_ref desc_ref acc =
      match !desc_ref, acc with
      | _, [] ->
        (* Impossible: [get_root] is called from a [Redo] node. *)
        assert false
      | Redo _, _ ->
        (* Impossible because [Done] nodes cannot point to [Redo] nodes. *)
        assert false
      | End, _ -> prev_desc_ref
      | Done { uid = _; n_common = _; past = _; chunk; next }, chunk' :: acc' ->
        if chunk_equal chunk chunk' then
          max_prefix desc_ref next acc'
        else
          (
            List.fold_left (fun base chunk_to_redo ->
                process_chunk perform_inference ctxt chunk_to_redo base
              ) prev_desc_ref acc
          )
    in
    get_root base [new_chunk]

module Dot =
struct
  module G = Graph.Pack.Digraph

  let vertex_labels = Hashtbl.create 11

  let end_ = G.V.create @@ gen ()

  let () =
    Hashtbl.add vertex_labels end_ (`End)

  module Display = struct
    include G
    let escape s =
      Printf.sprintf "\"%s\"" s

    let vertex_name v = G.V.label v |> string_of_int

    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes v =
      match Hashtbl.find_opt vertex_labels v with
      | None -> []
      | Some `End ->
        [`Label "EOS"]
      | Some (`Done chunk) ->
        [`Label chunk; `Shape `Box]
      | Some (`Redo chunk) ->
        [`Label chunk; `Shape `Diamond]

    let default_edge_attributes _ = []
    let edge_attributes e = [ `Label (string_of_int (E.label e) ) ]
    let get_subgraph _ = None
  end
  module Dot_ = Graph.Graphviz.Dot(Display)
  module Neato = Graph.Graphviz.Neato(Display)

  let dot_output g f =
    let oc = open_out f in
    if G.is_directed then Dot_.output_graph oc g else Neato.output_graph oc g;
    close_out oc

  let display_with_gv g =
    let tmp = Filename.temp_file "graph" ".dot" in
    dot_output g tmp;
    ignore (Sys.command ("dot -Tpng " ^ tmp ^ " | display"));
    Sys.remove tmp

  let add_vertex g v =
    if G.mem_vertex g v then () else G.add_vertex g v

  let add_edge g v1 v2 =
    if G.mem_edge g v1 v2 then () else G.add_edge g v1 v2

  let to_dot handles filename =
    let g = G.create () in
    add_vertex g end_ ;

    let visited = Hashtbl.create 11 in

    let rec unfold desc_ref =
      match !desc_ref with
      | End -> end_
      | Done { uid; next ; chunk; _ } -> (
          match Hashtbl.find_opt visited uid with
          | Some label -> label
          | None ->
            (let label = G.V.create uid in
             Hashtbl.add visited uid label ;
             add_vertex g label ;
             let next = unfold next in
             add_edge g label next ;
             Hashtbl.add vertex_labels label (`Done chunk) ;
             label))
      | Redo { uid ; next ; chunk; _ } -> (
          match Hashtbl.find_opt visited uid with
          | Some label -> label
          | None ->
            (let label = G.V.create uid in
             Hashtbl.add visited uid label ;
             add_vertex g label ;
             let next = unfold next in
             add_edge g label next ;
             Hashtbl.add vertex_labels label (`Redo chunk) ;
             label))
    in
    List.iter (fun desc_ref -> ignore @@ unfold desc_ref) handles ;
    dot_output g filename ;
    display_with_gv g
end
