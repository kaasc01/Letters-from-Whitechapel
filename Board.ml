(* 
   Christian Kaas
   Letters from Whitechapel - A Game Solver
   Spring 2018
*)

(* This module provides a model for a Whitechapel-like board game, implemented
   as an adjacency hashtable *)

(* Steps to use graph library in REPL:

      #use "topfind" ;;
      #require "ocamlgraph" ;;
      open Graph ;;
      #directory "/home/christian/CS/Projects/Whitechapel" ;;
      #mod_use "Geometry.ml";;
      open Geometry;;

 *) 

open Graph
open Geometry

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
    val pull_circles : t -> Vertex.t -> Edge.t -> Vertex.t list
    val single : t -> Vertex.t -> Vertex.t list
    val carriage : t -> Vertex.t -> Vertex.t list
    val alleyway : t -> Vertex.t -> Vertex.t list
  end


module Whitechapel  =
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
       can infer these at load time, and create explicit edges for more
       efficient traversal *)
    let pull_circles (g : Graph.t) (n : Vertex.t) : S.t =
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

    (* While the Geometry data does not explicit Circle to Circle moves, we
       can infer these at load time, and create explicit edges for more
       efficient traversal *)
    let infer_moves (g : Graph.t) =
      let open List in
      let circles = 195 in
      let vertices = init (circles) (fun x -> circles - x) in
      let format acc elt =
        let vertex = Circle elt in
        let links = S.elements (pull_circles g vertex) in
        (vertex, links) :: acc
      in
      fold_left format [] vertices

    (* Loads a dataset of edges into the graph g, assigning type t to the
       edges created *)
    let rec load_geometry (g : Graph.t) (t : Edge.t)
                          (dataset : (Vertex.t * Vertex.t list) list) : unit =
      match dataset with
      | [] -> ()
      | (v1, v2_lst) :: tl ->
          List.iter (fun v2 -> Graph.add_edge_e g (v1, t, v2)) v2_lst;
          load_geometry g t tl

    let make_moves (g : Graph.t) (n : Vertex.t) (c : int) : Vertex.t list = 
    


    let single (g : Graph.t) (n : Vertex.t) : Vertex.t list =
      S.elements (succ_e g n Move ())

    let carriage (g : Graph.t) (n : Vertex.t) : Vertex.t list =
      let first = single g n in
      let second_with_overlap =
        List.fold_left (fun elt -> ) first in 

    let alleyway (g : Graph.t) (n : Vertex.t) : Vertex.t list =
      S.elements (succ_e g n Alleyway ())

    let create () : Graph.t =
      let board = Graph.create () in 
      (* Load steps first *)
      load_geometry board Step steps;
      (* Use steps data to infer Move geometry *)
      load_geometry board Move (infer_moves board);
      (* Load alleyways last*)
      load_geometry board Alleyway alleyways;

      board

   end