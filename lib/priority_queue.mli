module type Comparable = sig
  type t = Task.t * int

  val compare : t -> t -> int
end

module Comparable_task : Comparable

include Intf.Priority_queue.S with type elem = Comparable_task.t
