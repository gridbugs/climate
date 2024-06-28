open Climate.For_test
open Climate_stdlib

let%test _ =
  let l, r = List.split_n [ 0; 1; 2; 3; 4 ] 2 in
  List.equal ~eq:Int.equal l [ 0; 1 ] && List.equal ~eq:Int.equal r [ 2; 3; 4 ]
;;

let%test _ =
  let l, r = List.split_n [] 2 in
  List.equal ~eq:Int.equal l [] && List.equal ~eq:Int.equal r []
;;

let%test _ =
  let l, r = List.split_n [ 0; 1; 2; 3; 4 ] (-1) in
  List.equal ~eq:Int.equal l [] && List.equal ~eq:Int.equal r [ 0; 1; 2; 3; 4 ]
;;

let%test _ =
  let l, r = List.split_n [ 0; 1; 2; 3; 4 ] 7 in
  List.equal ~eq:Int.equal l [ 0; 1; 2; 3; 4 ] && List.equal ~eq:Int.equal r []
;;
