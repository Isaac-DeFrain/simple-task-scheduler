let log =
  let rec aux acc pow n =
    if n < 2 * acc then pow else aux (2 * acc) (pow + 1) n
  in
  aux 1 0

let pow =
  let rec aux acc b n = if n <= 0 then acc else aux (b * acc) b (n - 1) in
  aux 1
