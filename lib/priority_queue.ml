[@@@warning "-32"]

open! Base

let name = "Priority queue"

module type Comparable = sig
  type t = Task.t * int

  val compare : t -> t -> int
end

module Comparable_task = struct
  type t = Task.t * int

  let compare (_, a) (_, b) = compare a b
end

type elem = Comparable_task.t

type t =
  { mutable capacity : int
  ; mutable size : int
  ; mutable items : Comparable_task.t array
  }

let capacity { capacity; _ } = capacity

let size { size; _ } = size

(* indicies *)
let get_left_child_idx pi = (2 * pi) + 1

let get_right_child_idx pi = (2 * pi) + 2

let get_parent_idx ci = (ci - 1) / 2

(* child + parnet predicates *)
let has_left_child i t = get_left_child_idx i < t.size

let has_right_child i t = get_right_child_idx i < t.size

let has_parent i _ = get_parent_idx i >= 0

(* child + parent items *)
let left_child i t = t.items.(get_left_child_idx i)

let right_child i t = t.items.(get_right_child_idx i)

let parent i t = t.items.(get_parent_idx i)

(* maintenance *)
let swap = Array.swap

let default = (Task.empty, 0)

let ensure_extra_capacity t =
  let cap = t.capacity in
  if t.size = cap then (
    let copy () =
      let open Array in
      let new_items = create ~len:(2 * cap) default in
      iteri t.items ~f:(fun i x -> new_items.(i) <- x);
      new_items
    in
    t.capacity <- 2 * cap;
    t.items <- copy ())

(* implementation *)
let is_empty t = t.size = 0

let peek t = if is_empty t then None else Some t.items.(t.size - 1)

let rec bubble_up x i t =
  if has_parent i t then
    let px = parent i t in
    let pi = get_parent_idx i in
    if Comparable_task.compare x px = -1 then (
      swap t.items i pi;
      bubble_up x pi t)

let log =
  let rec aux acc pow n =
    if n < 2 * acc then pow else aux (2 * acc) (pow + 1) n
  in
  aux 1 0

let pow =
  let rec aux acc b n = if n <= 0 then acc else aux (b * acc) b (n - 1) in
  aux 1

let max_sort t =
  let start = pow 2 (log t.size) - 1 in
  let stop = t.size - 1 in
  if stop > start then
    Array.sort ~pos:start ~len:(stop - start) t.items
      ~compare:Comparable_task.compare

let push x t =
  ensure_extra_capacity t;
  let i = t.size in
  t.size <- i + 1;
  t.items.(i) <- x;
  bubble_up x i t;
  max_sort t

let pop t =
  if t.size = 0 then None
  else
    let x = t.items.(t.size - 1) in
    t.items.(t.size - 1) <- (Task.empty, 0);
    t.size <- t.size - 1;
    Some x

let create ~capacity =
  { capacity; size = 0; items = Array.create ~len:capacity default }

let mk_positive_list items =
  Array.to_list items |> List.filter ~f:(fun (_, x) -> x > 0)

let to_list { items; _ } = mk_positive_list items

let to_sorted { items; _ } =
  mk_positive_list items |> List.sort ~compare:Comparable_task.compare
