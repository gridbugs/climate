module Result = struct
  include Result

  let map t ~f = map f t
  let map_error t ~f = map_error f t
  let bind t ~f = bind t f

  module List = struct
    type ('a, 'error) t = ('a, 'error) result list

    let rec all = function
      | [] -> Ok []
      | Ok x :: xs -> map (all xs) ~f:(fun xs -> x :: xs)
      | Error error :: _xs -> Error error
  end
end

module List = struct
  include StdLabels.List

  let find_duplicate ~eq t =
    let contains xs x = exists ~f:(eq x) xs in
    let rec loop = function
      | [] -> None
      | x :: xs -> if contains xs x then Some x else loop xs
    in
    loop t
end

module Map = struct
  include MoreLabels.Map

  module type S = sig
    include S

    val find : 'a t -> key -> 'a option
    val set : 'a t -> key -> 'a -> 'a t
    val of_list : (key * 'a) list -> ('a t, key * 'a * 'a) Result.t
  end

  module Make (Key : OrderedType) : S with type key = Key.t = struct
    include MoreLabels.Map.Make (struct
      type t = Key.t

      let compare = Key.compare
    end)

    let find key t = find_opt t key
    let set t k v = add ~key:k ~data:v t

    let of_list =
      let rec loop acc = function
        | [] -> Result.Ok acc
        | (k, v) :: l -> (
            match find acc k with
            | None -> loop (set acc k v) l
            | Some v_old -> Error (k, v_old, v))
      in
      fun l -> loop empty l
  end
end

module Nonempty_list = struct
  type 'a t = ( :: ) of ('a * 'a list)

  let of_list = function [] -> None | x :: xs -> Some (x :: xs)
  let to_list (x :: xs) = List.(x :: xs)
  let map (x :: xs) ~f = f x :: List.map xs ~f
end

module Nonnegative_int = struct
  type t = int

  let of_int x = if x < 0 then None else Some x
  let to_int x = x
end

module String = struct
  include StdLabels.String

  let lsplit2 s ~on =
    match index_opt s on with
    | None -> None
    | Some i ->
        Some (sub s ~pos:0 ~len:i, sub s ~pos:(i + 1) ~len:(length s - i - 1))

  let is_empty s = String.length s == 0

  module Set = Set.Make (String)
  module Map = Map.Make (String)
end

module Int = struct
  include Int
  module Set = Set.Make (Int)
  module Map = Map.Make (Int)
end
