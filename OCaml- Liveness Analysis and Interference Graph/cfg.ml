open Cfg_ast
exception Implement_Me
exception FatalError


type igraph_node = RegNode of Mips.reg | VarNode of var

let string_of_node (n: igraph_node) : string =
  match n with
  | RegNode r -> Mips.reg2string r
  | VarNode v -> v
;;

module IGraphNode =
  struct
    type t = igraph_node
    let compare = compare
  end

module NodeSet = Set.Make(IGraphNode)                                                   
(* These are the registers that must be generated / killed as part of
   liveness analysis for call instructions to reflect MIPS calling
   conventions *)

let call_gen_list = ["$4";"$5";"$6";"$7"]
let call_kill_list = ["$1";"$2";"$3";"$4";"$5";"$6";"$7";"$8";"$9";"$10";
                      "$11";"$12";"$13";"$14";"$15";"$24";"$25";"$31"]

(* Undirected graphs where nodes are identified by igraph_node type above. Look at
   graph.ml for the interface description.  *)

module IUGraph = Graph.UndirectedGraph(IGraphNode)

(* this is a wrapper to addEdge that prevents adding self edges.
   to do all sorts of other complicated stuff for eg coloring *)
let specialAddEdge u v g =
  if (u = v) then
    g
  else
    IUGraph.addEdge u v g

(* An interference graph is an SUGraph where a node is temp variable
   or a register (to be able to handle pre-colored nodes)

   The adjacency set of variable x should be the set of variables
   y such that x and y are live at the same point in time. *)
type interfere_graph = IUGraph.graph

(* To help you printing an igraph for debugging *)
let string_of_igraph (g: interfere_graph) : string =
  let rec string_of_row (n: IUGraph.node) =
    let ns = IUGraph.adj n g in
    Printf.sprintf "%s : {%s}"
      (string_of_node n)
      (String.concat "," (List.map string_of_node (NodeSet.elements ns)))
  in
  let rows = String.concat "\n" (List.map string_of_row (NodeSet.elements (IUGraph.nodes g))) in
  Printf.sprintf "{\n%s\n}" rows
  

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)






 let update_live_out_block live_out_map block live_in_block =
  live_out_map := NodeSet.add block live_in_block
;;

let rec compute_live_out live_out_map block live_in_block =
  let live_out_block = ref live_in_block in
  let update_live_out instr =
    live_out_block := NodeSet.union (!live_out_block) (use instr)
  in
  List.iter (fun instr ->
    update_live_out instr;
    let live_out_instr = NodeSet.diff (!live_out_block) (def instr) in
    let live_in_instr = NodeSet.union (use instr) live_out_instr in
    update_live_out_block live_out_map instr live_in_instr;
    live_out_block := live_in_instr
  ) block;
  !live_out_block
;;
let rec compute_live_out_block live_out_map block =
  if NodeSet.mem block (!live_out_map) then
    NodeSet.find block (!live_out_map)
  else
    let live_out_block = compute_live_out live_out_map block NodeSet.empty in
    update_live_out_block live_out_map block live_out_block;
    live_out_block
;;



let build_interfere_graph (f : func) : interfere_graph =
  let igraph = IUGraph.empty in
  let live_out_map = ref NodeSet.empty in

  let rec compute_live_out_block block =
    if NodeSet.mem block (!live_out_map) then
      NodeSet.find block (!live_out_map)
    else
      let live_out_block = compute_live_out block NodeSet.empty in
      update_live_out_block block live_out_block;
      live_out_block
  in

  let rec compute_live_out block live_in_block =
    let live_out_block = ref live_in_block in
    let update_live_out instr =
      live_out_block := NodeSet.union (!live_out_block) (use instr)
    in
    List.iter (fun instr ->
      update_live_out instr;
      let live_out_instr = NodeSet.diff (!live_out_block) (def instr) in
      let live_in_instr = NodeSet.union (use instr) live_out_instr in
      update_live_out_block instr live_in_instr;
      live_out_block := live_in_instr
    ) block;
    !live_out_block
  in

  List.iter (fun block ->
    let live_out_block = compute_live_out_block block in
    update_live_out_block live_out_map block live_out_block
  ) f.basic_blocks;

  List.iter (fun block ->
    let live_out_block = NodeSet.find block (!live_out_map) in
    let interference_edges = compute_interference_edges live_out_block in
    List.iter (fun (u, v) ->
      ignore (specialAddEdge u v igraph)
    ) interference_edges
  ) f.basic_blocks;

  igraph
