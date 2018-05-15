(**************************************************************************)
(*                                                                        *)
(*  Letters from Whitechapel: A Model and Solver                          *)
(*  Copyright (C) 2018                                                    *)
(*  Christian Kaas                                                        *)
(*                                                                        *)
(*  Based on the Cobblepot Games' popular "Letters from Whitechapel"      *)
(*  boardgame by Gabriele Mari and Gianluca Santopietro                   *)
(*  http://www.sirchestercobblepot.com/en/letters-from-whitechapel/       *)
(*                                                                        *)
(**************************************************************************)

(* This module provides a model for a Whitechapel-like board game, implemented
   as an adjacency hashtable *)

(* Steps to use graph library in REPL:

      #use "topfind" ;;
      #require "ocamlgraph" ;;
      open Graph ;;
      #directory "/home/christian/CS/Projects/Whitechapel" ;;
      #mod_use "Geometry.ml";;
      open Geometry;;
      #mod_use "Header.ml";;
      open Header;;
 *) 

open Graph
open Geometry
open Header

(* Describe vertex interface and implementation here *)
module Vertex =
  struct
    type t = node_type
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

(* Describe edge interface and implementation here *)
module Edge =
  struct
    type t = move_type 
    let compare = Pervasives.compare
    let default = Step
    let equal = (=)
  end

(* Describe choice of graph implementation here *)
module type GAMEBOARD =
  sig
    type t
    val create : unit -> t
    val single : t -> Vertex.t -> Vertex.t list
    val carriage : t -> Vertex.t -> Vertex.t list
    val alleyway : t -> Vertex.t -> Vertex.t list
    val remove_circle : t -> Vertex.t -> unit
  end


module Whitechapel =
  struct
    
    (* Implements a standard graph structure, and make all the default
       graph methods available to the Whitechapel graph *)
    module Graph = Imperative.Graph.ConcreteLabeled(Vertex)(Edge)

    (* Aliases the graph's type signature, to be consistent with the
       GAMEBOARD signature *)
    type t = Graph.t

    (* Implements a set data structure, to help keep track of visited states *)
    module S = Set.Make(struct
                          type t = Vertex.t
                          let compare = Pervasives.compare
                        end)

    (* Returns the set of vertices reachable from vertex n through an edge
       of type e, in graph g. Optionally, exclude from results any values
       already present in set f *)
    let succ_e (g : Graph.t) (n : Vertex.t) (e : Edge.t)
               ?(f : S.t = S.empty) () : S.t =
      let open List in
      (* Keep track of identified vertices *)
      let results : S.t ref = ref S.empty in
      (* Returns a list of all edges between vertices elt and n *)
      let edges (elt : Vertex.t) : Graph.edge list =
        Graph.find_all_edges g n elt
      in
      (* Filters a list of edges, adding to results set those whose type
         matches e AND whose destination vertex v2 is NOT in f *)
      let filter (lst : Graph.edge list) : unit =
        iter (fun (_, ctype, v2) ->
                if Edge.equal ctype e && not (S.mem v2 f)
                  then results := S.add v2 !results else ()) lst
      in
      (* List all vertices reachable from n *)
      let reachable : Vertex.t list = Graph.succ g n in
      (* Iterate through all vertices reachable from n, adding to results
         set only those meeting the criteria set out in filter *)
      iter (fun elt -> filter (edges elt)) (reachable);
      !results

    (* While the Geometry data does not explicit Circle to Circle moves, we
       can infer these at load time from the step data, and create explicit
       edges for more efficient traversal *)
    let infer_moves (g : Graph.t) (n : Vertex.t) : S.t =
      (* Keep track of visited vertices *)
      let visited = ref (S.singleton n) in
      (* Define a shorthand for unvisited successors of a vertex *)
      let next_vs (n : Vertex.t) = succ_e g n Step ~f:(!visited) () in
      (* Keep track of remaining candidates to visit *)
      let candidates = ref (next_vs n) in
      (* Keep track of circles successfully identified *)
      let result = ref S.empty in
      (* While the candidate set still has members... *)
      while not (S.is_empty !candidates) do
        (* Get the next vertex and move it from the candidate to the
           visited set *)
        let cand_0 = S.find_first (fun elt -> true) !candidates in
        visited := S.add cand_0 !visited;
        candidates := S.remove cand_0 !candidates;
        match cand_0 with
        (* If the vertex is a Circle, add it to result set *)
        | Circle _ -> result := S.add cand_0 !result;
        (* If the vertex is a Square, access successor vertices, and add any
           not yet visited to the candidate set *)
        | Square _ -> candidates := S.union (next_vs cand_0) !candidates;
      done;
      !result

    (* While the Geometry data does not explicit carriages moves, we
       can also infer these from the infered Move data, and again create
       explicit edges for more efficient traversal *)
    let infer_carriages (g : Graph.t) (n : Vertex.t) : S.t = 
      (* Keep track of possible vertices *)
      let result : S.t ref = ref (S.empty) in
      (* Updates the result set to be the union of result and a set s *)
      let add_to_result (s : S.t) : unit = result := S.union s !result in
      (* Returns a list of possible vertices after a single Move from n *)
      let get_moves (n : Vertex.t) : S.t = succ_e g n Move () in
      (* Returns a list of possible vertices after the first Move *)
      let first : Vertex.t list = S.elements (get_moves n) in
      (* Iterate through all the vertices possible after the first Move,
         adding all vertices possible after a second Move to the result set *)
      List.iter (fun v -> (add_to_result (succ_e g v Move ()))) first;
      (* As rules disallow for a carriage move to end up in the same place it
       started, ensure origin doesn appear in the result set *)
      S.remove n !result

    (* Loads a dataset of edges into the graph g, assigning type t to the
       edges created *)
    let rec load_geometry (g : Graph.t) (t : Edge.t)
                          (dataset : (Vertex.t * Vertex.t list) list) : unit =
      match dataset with
      | [] -> ()
      | (v1, v2_lst) :: tl ->
          List.iter (fun v2 -> Graph.add_edge_e g (v1, t, v2)) v2_lst;
          load_geometry g t tl

    (* Given a circle, returns its index *)
    let extract_circle (elt : Vertex.t) : int =
      match elt with
      | Circle n -> n
      | _ -> raise (Invalid_argument "extract_circle: unexpected match")

    (* Given a list of infered connnections, removes any redundant links and
       filters the list for vertices with empty connection lists *)
    let filter_inference (lst : (Vertex.t * Vertex.t list) list)
                       : (Vertex.t * Vertex.t list) list =
      let open List in
      (* Filters a list lst for any Circles whose index is less than m *)
      let reduce_vlist (m : int) (lst : Vertex.t list) : Vertex.t list =
        filter (fun elt -> (extract_circle elt) > m) lst
      in
      (* Removes redundant links in the inference *)
      let unique_connections =
        map (fun (v, vlist) -> (v, reduce_vlist (extract_circle v) vlist)) lst
      in
      (* Filters the inference list for any empty connection lists *)
      filter (fun (v, vlist) -> vlist <> []) unique_connections

    (* Formats edge relationships infered by function f into the format
       expected by load_geometry *)
    let structure_inference (g : Graph.t)
                            (f : Graph.t -> Vertex.t -> S.t)
                          : (Vertex.t * Vertex.t list) list =
      let open List in
      let circles = 195 in
      let vertices = init (circles) (fun x -> circles - x) in
      let format acc elt =
        let vertex = Circle elt in
        let links = S.elements (f g vertex) in
        (vertex, links) :: acc
      in
      filter_inference (fold_left format [] vertices)

    (* Given an integer n, pretty-prints a Circle as a string *)
    let serialize_circle (n : int) : string = "Circle " ^ string_of_int n

    (* Given a list of vertices, pretty prints the list as a string *)
    let serialize_vlist (s : Vertex.t list) : string =
      let f acc elt = "Circle " ^ string_of_int (extract_circle elt)
                      ^ (if acc <> "" then "; " ^ acc else "")
      in
      "[" ^ List.fold_left f "" s ^ "]"

    (* Given an integer n, and a list of vertices, pretty prints a tuple as 
       a string *)
    let serialize_connections (n : int) (lst : Vertex.t list) : string =
      "(" ^ serialize_circle n ^ ", " ^ (serialize_vlist lst) ^ ")"

    (* Given a list of connection tuples, pretty prints the list as a string to
       the channel c *)
    let serialize_connection_list (lst : (Vertex.t * Vertex.t list) list)
                                  (oc : out_channel) : unit =
      let open Printf in
      let rec print_loop lst pref =
        match lst with
        | [] -> ()
        | (v, vlist) :: tl ->
            match v with
            | Circle n -> let line =
                            pref ^ (serialize_connections n vlist)
                          in fprintf oc "%s" line;
                          print_loop tl ";\n   "
            | _ -> raise (Invalid_argument "serialize_connection_list: \
                                            unexpected match")
      in
      print_loop lst "\n  [" ;;    

    (* Given a list of connection tuples, prints the result to a file f, stored
       with variable name v *)
    let print_inference (lst : (Vertex.t * Vertex.t list) list)
                        (v : string) (f : string)
                       : unit =
      let open Printf in
      (* Open an out channel to the file named f *)
      let oc = open_out f in
      (* First, print a copy of the project header *)
      fprintf oc "%s\n\n" header;
      (* Then, introduce a let statement for a new variable named v *)
      fprintf oc ("let %s =") v;
      (* Next, print the data in list lst *)
      serialize_connection_list lst oc;
      (* Finally, print a close for the list *)
      fprintf oc "]";
      (* Close the out channel *)
      close_out oc;
    ;;

    let create () : Graph.t =
      let board = Graph.create () in 
      (* Load steps first *)
      load_geometry board Step steps;
      board;;

    let () =
      (* Create an empty board *)
      let board = create () in
      (* Load steps data into board *)
      load_geometry board Step steps;
      (* Infer moves based on steps, and add to board *)
      let moves = structure_inference board infer_moves in
      load_geometry board Move moves;
      (* Print moves to file *)
      print_inference moves "moves" "Moves.ml";
      (* Infer carriages based on moves, and add to board *)
      let carriages = structure_inference board infer_carriages in
      (* Print carriages to file *)
      print_inference carriages "carriages" "Carriages.ml";

   end