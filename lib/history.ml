module type Action_S =
sig
  type t
  val nil : t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module type State_S =
sig
  type t
  val init : t
end

module type S =
sig
  type action
  type state
  type t
  val create : unit -> t
  val process_action :
    ((state * action) list -> state -> action -> state) -> action -> t -> t

  module Dot :
    sig
      val to_dot : t list -> string -> unit
    end
end

module Make
    (Action : Action_S)
    (State : State_S) : S with type action = Action.t and type state = State.t
=
struct
  type action = Action.t
  type state = State.t

  type 'a trace = { mutable elt : 'a; mutable next : 'a trace option }

  type desc =
    | Done of { uid : int ;
                state : state ; (* Results from the application of [action] on the previous state. *)
                action : action }
    | Redo of { uid : int ;
                action : action }

  type t = desc trace ref

  let gen =
    let x = ref 0 in
    fun () ->
      let v = !x in
      incr x;
      v

  let rec get_acc_and_reverse prev_node node_opt acc k =
    match node_opt with
    | None ->
      k () ;
      acc
    | Some node ->
      match node.elt with
      | Redo _ ->
        (* Impossible because a [Done] node may only point to [None] or [Done] node. *)
        assert false
      | Done { uid = _ ; state ; action } ->
        let acc = (state, action) :: acc in
        get_acc_and_reverse node node.next acc (fun () ->
            k () ;
            node.elt <- Redo { uid = gen () ; action } ;
            node.next <- (Some prev_node)
          )

  let create () =
    ref { elt =
            (Done { uid = gen () ; state = State.init ; action = Action.nil }) ;
          next = None }

  let process_action process_action new_action (base : t) : t =
    let rec loop new_action (base : t) : desc trace =
      let trace = !base in
      match trace.elt with
      | Done { uid = _; state ; action = _ } ->
        (* Invariant: at this point, the chain of nodes reachable from [next] only contains
           [Done] nodes. *)
        let acc = get_acc_and_reverse trace trace.next [] (fun () -> ()) in
        (* Invariant: at this point, [next] is rewritten to point to a [Redo] node. *)
        let state = process_action acc state new_action in
        let hist_elt =
          Done { uid = gen ();
                 state ;
                 action = new_action }
        in
        let next = { elt = hist_elt; next = None } in
        trace.next <- Some next ;
        next
      | Redo _ ->
        (*
         ... -> root <- redo(chunk0) <- redo(chunk1) <- redo(chunk2) <- ... <- base=redo(chunkN)
                 |
                 \-> Done(chunk0) -> Done(chunk1) -> Done(chunk2' <> chunk2) -> ... -> End
                                     ^^^^^^^^^^^
                                     node on which we recursively call [process_action]
         *)
        let rec get_root node acc =
          match node.elt with
          | Done _ ->
            max_prefix node node.next acc
          | Redo { uid = _; action } ->
            (match node.next with
             | None ->
               (* Invariant: a [Redo] node {b must} point to a [Done] node. *)
               assert false
             | Some next ->
               get_root next (action :: acc)
            )
        and max_prefix prev_node node_opt acc =
          match node_opt, acc with
          | _, [] ->
            (* Impossible: [get_root] is called from a [Redo] node. *)
            assert false
          | None, _ ->
            (* TODO (to test!):
               - we have to update [base] both in this case and in the [Done] case below, no?
            *)
            replay prev_node acc
          | Some node, action' :: acc' ->
            (match node.elt with
             | Redo _ ->
               (* Impossible because [Done] nodes cannot point to [Redo] nodes. *)
               assert false
             | Done { uid = _; state = _; action } ->
               if Action.equal action action' then
                 max_prefix node node.next acc'
               else
                 replay prev_node acc
            )
        and replay node acc =
          let replayed =
            List.fold_left (fun node chunk_to_redo ->
                loop chunk_to_redo (ref node)
              ) node acc
          in
          base := replayed ;
          loop new_action (ref replayed)
        in
        get_root trace []
    in
    let result = loop new_action base in
    ref result

  module Dot =
  struct
    module G = Graph.Pack.Digraph

    let vertex_labels = Hashtbl.create 11

    let end_ = G.V.create @@ gen ()

    let () =
      Hashtbl.add vertex_labels end_ (`End)

    module Display = struct
      include G

      let vertex_name v = G.V.label v |> string_of_int

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_attributes v =
        match Hashtbl.find_opt vertex_labels v with
        | None -> []
        | Some `End ->
          [`Label "EOS"]
        | Some (`Done action) ->
          let s = Format.asprintf "%a" Action.pp action in
          [`Label s; `Shape `Box]
        | Some (`Redo action) ->
          let s = Format.asprintf "%a" Action.pp action in
          [`Label s; `Shape `Diamond]

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

    let display_with_png g file =
      let tmp = Filename.temp_file "graph" ".dot" in
      dot_output g tmp;
      ignore (Sys.command ("dot -Tpng " ^ tmp ^ " > " ^ file));
      Sys.remove tmp

    let add_vertex g v =
      if G.mem_vertex g v then () else G.add_vertex g v

    let add_edge g v1 v2 =
      if G.mem_edge g v1 v2 then () else G.add_edge g v1 v2

    let to_dot (handles : t list) filename =
      let g = G.create () in
      add_vertex g end_ ;

      let visited = Hashtbl.create 11 in

      let rec unfold node_opt =
        match node_opt with
        | None -> end_
        | Some node ->
          (match node.elt with
           | Done { uid; action; _ } -> (
               match Hashtbl.find_opt visited uid with
               | Some label -> label
               | None ->
                 (let label = G.V.create uid in
                  Hashtbl.add visited uid label ;
                  add_vertex g label ;
                  let next = unfold node.next in
                  add_edge g label next ;
                  Hashtbl.add vertex_labels label (`Done action) ;
                  label))
           | Redo { uid ; action; _ } -> (
               match Hashtbl.find_opt visited uid with
               | Some label -> label
               | None ->
                 (let label = G.V.create uid in
                  Hashtbl.add visited uid label ;
                  add_vertex g label ;
                  let next = unfold node.next in
                  add_edge g label next ;
                  Hashtbl.add vertex_labels label (`Redo action) ;
                  label)))
      in
      List.iter (fun node_ref -> ignore @@ unfold (Some !node_ref)) handles ;
      dot_output g filename ;
      display_with_png g (filename ^ ".png")
  end

end
