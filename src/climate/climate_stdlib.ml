module Result = struct
  include Result

  let map t ~f = map f t
  let map_error t ~f = map_error f t
  let bind t ~f = bind t f

  let both a b =
    match a with
    | Error e -> Error e
    | Ok a ->
      (match b with
       | Error e -> Error e
       | Ok b -> Ok (a, b))
  ;;

  module List = struct
    type ('a, 'error) t = ('a, 'error) result list

    let rec all = function
      | [] -> Ok []
      | Ok x :: xs -> map (all xs) ~f:(fun xs -> x :: xs)
      | Error error :: _xs -> Error error
    ;;

    let rec fold_left ~f ~init = function
      | [] -> Ok init
      | x :: xs -> bind (fold_left ~f ~init xs) ~f:(fun acc -> f acc x)
    ;;
  end

  module O = struct
    let ( >>= ) t f = bind t ~f
    let ( >>| ) t f = map t ~f
    let ( let* ) = ( >>= )
    let ( let+ ) = ( >>| )
    let ( and+ ) = both
  end
end

module Option = struct
  include Option

  let map t ~f = map f t
  let iter t ~f = iter f t
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
  ;;

  let rec split_n t n =
    match t with
    | [] -> [], []
    | x :: xs ->
      if n <= 0
      then [], t
      else (
        let l, r = split_n xs (n - 1) in
        x :: l, r)
  ;;

  let is_empty = function
    | [] -> true
    | _ -> false
  ;;

  let filter_opt t = filter_map t ~f:Fun.id

  let rec last = function
    | [] -> None
    | [ x ] -> Some x
    | _ :: xs -> last xs
  ;;
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
        | (k, v) :: l ->
          (match find acc k with
           | None -> loop (set acc k v) l
           | Some v_old -> Error (k, v_old, v))
      in
      fun l -> loop empty l
    ;;
  end
end

module Nonempty_list = struct
  type 'a t = ( :: ) of ('a * 'a list)

  let singleton x = [ x ]

  let of_list = function
    | [] -> None
    | x :: xs -> Some (x :: xs)
  ;;

  let to_list (x :: xs) = List.(x :: xs)
  let map (x :: xs) ~f = f x :: List.map xs ~f

  let hd = function
    | x :: _ -> x
  ;;
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
    | Some i -> Some (sub s ~pos:0 ~len:i, sub s ~pos:(i + 1) ~len:(length s - i - 1))
  ;;

  let is_empty s = String.length s == 0

  let rec check_prefix s ~prefix len i =
    i = len || (s.[i] = prefix.[i] && check_prefix s ~prefix len (i + 1))
  ;;

  let is_prefix s ~prefix =
    let len = length s in
    let prefix_len = length prefix in
    len >= prefix_len && check_prefix s ~prefix prefix_len 0
  ;;

  let drop_prefix s ~prefix =
    if is_prefix s ~prefix
    then
      if length s = length prefix
      then Some ""
      else Some (sub s ~pos:(length prefix) ~len:(length s - length prefix))
    else None
  ;;

  module Set = Set.Make (String)
  module Map = Map.Make (String)
end

module Int = struct
  include Int
  module Set = Set.Make (Int)
  module Map = Map.Make (Int)
end
