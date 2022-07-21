module Priority_queue = struct
  module type S = sig
    type t

    type elem

    val name : string

    val peek : t -> elem option

    val create : capacity:int -> t

    val is_empty : t -> bool

    val size : t -> int

    val capacity : t -> int

    val push : elem -> t -> unit

    val pop : t -> elem option

    val to_list : t -> elem list

    val to_sorted : t -> elem list
  end
end

module Task = struct
  module type S = sig
    type t

    val empty : t

    val of_char : char -> t

    val to_string : t -> string

    val to_char : t -> char
  end
end

module Scheduler (Task : Task.S) = struct
  module type S = sig
    type t

    val name : string

    val id : t -> int

    val heap : t -> (string * int) list

    val heap_size : t -> int

    val heap_peek : t -> (string * int) option

    val queue : t -> (Task.t * int * int) Base.Queue.t

    val queue_size : t -> int

    val print_schedule : t -> unit

    val time : t -> int

    val init : delay:int -> Task.t list -> t

    val run : t -> unit
  end
end
