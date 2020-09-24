open Zlist

let print t =
  let sep ppf () =
    Format.pp_print_string ppf "," ;
    Format.pp_print_space ppf () in
  Format.fprintf Format.std_formatter "@[%a@]@." (pp ~sep Format.pp_print_int) t

let%expect_test "items" =
  print (items [1; 2; 3]) ;
  [%expect {| 1, 2, 3 |}]

let%expect_test "of_array" =
  print (of_array [|1; 2; 3|]) ;
  [%expect {| 1, 2, 3 |}]

let%expect_test "unit" =
  print (unit 1) ;
  [%expect {| 1 |}]

let%expect_test "fill" =
  print (fill 3 10) ;
  [%expect {| 10, 10, 10 |}]

let%expect_test "unfold" =
  print (unfold 0 (fun i -> if i < 3 then Some (i + 1, 10) else None)) ;
  [%expect {| 10, 10, 10 |}]

let%expect_test "iterate" =
  print (iterate 0 (fun x -> x + 1) |> take 5) ;
  [%expect {| 0, 1, 2, 3, 4 |}]

let%expect_test "continually" =
  print (continually 1 |> take 3) ;
  [%expect {| 1, 1, 1 |}]

let%expect_test "enum_from" =
  print (enum_from 0 |> take 3) ;
  [%expect {| 0, 1, 2 |}]

let%expect_test "enum_from_to" =
  print (enum_from_to 0 3) ;
  [%expect {| 0, 1, 2, 3 |}]

let%expect_test "cycle" =
  print (cycle (items [1; 2; 3]) |> take 7) ;
  [%expect {| 1, 2, 3, 1, 2, 3, 1 |}]

let%test "head - some" = head (items [1; 2; 3]) = Some 1
let%test "head - none" = head (lazy Nil) = None

let%expect_test "tail" =
  print (tail (items [1; 2; 3])) ;
  [%expect {| 2, 3 |}]

let%expect_test "take_while" =
  print (items [1; 2; 3; 4; 5] |> take_while (fun x -> x <> 3)) ;
  [%expect {| 1, 2 |}]

let%expect_test "drop" =
  print (items [1; 2; 3; 4] |> drop 2) ;
  [%expect {| 3, 4 |}]

let%expect_test "drop_while" =
  print (items [1; 3; 2; 5] |> drop_while (fun x -> x <> 2)) ;
  [%expect {| 2, 5 |}]

let%expect_test "map" =
  print (continually 0 |> map (fun x -> x + 1) |> take 3) ;
  [%expect {| 1, 1, 1 |}]

let%expect_test "flat_map" =
  print (enum_from 0 |> flat_map (fun x -> items [x; x + 1]) |> take 6) ;
  [%expect {| 0, 1, 1, 2, 2, 3 |}]

let%expect_test "filter" =
  print (cycle (items [1; 2]) |> filter (fun x -> x < 2) |> take 3) ;
  [%expect {| 1, 1, 1 |}]

let%expect_test "map_filter" =
  print
    ( enum_from_to 0 10
    |> map_filter (fun x -> if x < 5 then Some (10 * x) else None) ) ;
  [%expect {| 0, 10, 20, 30, 40 |}]

let%expect_test "flatten" =
  print (continually (items [1; 2; 3]) |> flatten |> take 5) ;
  [%expect {| 1, 2, 3, 1, 2 |}]

let%test "exists - yes" = exists (fun x -> x = 1) (items [1; 2; 3])
let%test "exists - no" = not (exists (fun x -> x = 1) (items [2; 3]))
let%test "for_all - empty" = for_all (fun _ -> false) (lazy Nil)
let%test "for_all - yes" = for_all (fun x -> x mod 2 = 0) (items [0; 2; 4])
let%test "for_all - no" = not (for_all (fun x -> x mod 2 = 0) (items [0; 1; 4]))
let%test "find - some" = Some 3 = find (fun x -> x = 3) (items [1; 3; 5])
let%test "find - none" = None = find (fun x -> x > 100) (items [10; 20])

let%expect_test "concat" =
  print (concat (items [1; 2; 3]) (items [4; 5; 6])) ;
  [%expect {| 1, 2, 3, 4, 5, 6 |}]

let%expect_test "zip_with" =
  print (zip_with (fun x y -> x + y) (items [1; 2; 3]) (items [10; 20; 30])) ;
  [%expect {| 11, 22, 33 |}]

let%test "zip" =
  [(1, 10); (2, 20); (3, 30)]
  = (zip (items [1; 2; 3]) (items [10; 20; 30]) |> strict)

let%expect_test "zip_all_with" =
  print
    (zip_all_with
       (fun x y -> match (x, y) with Some x, Some y -> x + y | _ -> -1)
       (items [2; 3])
       (items [10; 20; 30])) ;
  [%expect {| 12, 23, -1 |}]

let%test "zip_all" =
  [(Some 1, Some 1); (Some 2, Some 2); (None, Some 3)]
  = (zip_all (items [1; 2]) (items [1; 2; 3]) |> strict)

let%test "strict" = [1; 2; 3] = strict (items [1; 2; 3])

let%test "fold_right" =
  55 = fold_right (fun x n -> x + Lazy.force n) (enum_from_to 1 10) (lazy 0)

let%test "fold_left" = 55 = fold_left (fun n x -> n + x) 0 (enum_from_to 1 10)
let%test "length" = 3 = length (items [1; 2; 3])
let%test "equal - yes" = equal ( = ) (items [1; 2; 3]) (items [1; 2; 3])
let%test "equal - no" = not (equal ( = ) (items [1; 2; 3]) (continually 1))
