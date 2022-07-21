open Simple_task_scheduler.Priority_queue

(* TODO *)

let check_size_and_cap n =
  let t = create ~capacity:n in
  is_empty t && capacity t = n

let creation_size_and_capacity =
  let open QCheck in
  Test.make ~count:100
    ~name:"heaps have the correct size and capacity when created" (1 -- 10)
    check_size_and_cap

let suite = List.map QCheck_alcotest.to_alcotest [ creation_size_and_capacity ]

let named_suite = (name, suite)
