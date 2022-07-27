open! Base
open! QCheck
open! Simple_task_scheduler.Task
open! Simple_task_scheduler.Priority_queue

let check_size_and_cap n =
  let t = create ~capacity:n in
  is_empty t && capacity t = n

let creation_size_and_capacity =
  Test.make ~count:100
    ~name:"heaps have the correct size and capacity when created" (1 -- 10)
    check_size_and_cap

let push_empty x = push (empty, x)

let make_of_list l =
  let t = create ~capacity:128 in
  let rec add = function
    | [] -> ()
    | x :: tl ->
      push_empty x t;
      add tl
  in
  add l;
  t

let push_test =
  Test.make ~count:100 ~name:"push max to top" (small_list small_int) (fun l ->
      let t = make_of_list l in
      let m =
        List.fold_left l ~init:0 ~f:(fun m x -> if m < x then x else m) + 1
      in
      push_empty m t;
      match pop t with
      | Some (_, x) -> x = m
      | None -> assert false)

let pop_test =
  Test.make ~count:100 ~name:"heap sort" (small_list small_int) (fun l ->
      let t = make_of_list l in
      let rec loop acc =
        match pop t with
        | None -> acc
        | Some (_, x) -> loop (x :: acc)
      in
      let heap_sorted = loop [] in
      List.equal ( = ) heap_sorted @@ List.sort l ~compare)

let suite =
  List.map ~f:QCheck_alcotest.to_alcotest
    [ creation_size_and_capacity; push_test; pop_test ]

let named_suite = (name, suite)
