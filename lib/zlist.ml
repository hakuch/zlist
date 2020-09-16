let ( !! ) = Lazy.force

type 'a t = 'a node Lazy.t

and 'a node = Nil | Cons of 'a * 'a t

let rec iter f = function
  | (lazy Nil) -> ()
  | (lazy (Cons (x, t))) -> f x ; iter f t

let pp ~sep pp_item ppf t =
  let is_first = ref true in
  let print ppf v =
    if !is_first then is_first := false else sep ppf () ;
    pp_item ppf v in
  iter (print ppf) t

let rec fold_right f t z =
  match t with
  | (lazy Nil) -> !!z
  | (lazy (Cons (x, t))) -> f x (lazy (fold_right f t z))

let strict t = fold_right (fun x t -> x :: !!t) t (lazy [])
let unit x = lazy (Cons (x, lazy Nil))
let head = function (lazy Nil) -> None | (lazy (Cons (x, _))) -> Some x
let tail = function (lazy Nil) -> lazy Nil | (lazy (Cons (_, t))) -> t
let items xs = List.fold_right (fun x t -> lazy (Cons (x, t))) xs (lazy Nil)

let rec concat t1 t2 =
  match t1 with
  | (lazy Nil) -> t2
  | (lazy (Cons (lx, t))) -> lazy (Cons (lx, concat t t2))

let rec continually x = lazy (Cons (x, continually x))

let fold_left f z t =
  let rec loop acc = function
    | (lazy Nil) -> acc
    | (lazy (Cons (x, t))) -> loop (f acc x) t in
  loop z t

let rec take n t =
  if n <= 0 then lazy Nil
  else
    match t with
    | (lazy Nil) -> lazy Nil
    | (lazy (Cons (x, t))) -> lazy (Cons (x, take (n - 1) t))

let rec take_while p = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t))) ->
      if p x then lazy (Cons (x, take_while p t)) else lazy Nil

let rec iterate z f = lazy (Cons (z, iterate (f z) f))
let exists f t = fold_right (fun a lb -> f a || !!lb) t (lazy false)

let rec for_all f = function
  | (lazy Nil) -> true
  | (lazy (Cons (x, t))) -> f x && for_all f t

let rec drop n t =
  if n <= 0 then t
  else
    match t with
    | (lazy Nil) -> lazy Nil
    | (lazy (Cons (_, t))) -> drop (n - 1) t

let rec drop_while p = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t))) as tt -> if p x then drop_while p t else tt

let fill n x = continually x |> take n

let rec map f = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, lxs))) -> lazy (Cons (f x, map f lxs))

let rec zip_with f t1 t2 =
  match (t1, t2) with
  | (lazy Nil), _ | _, (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t1))), (lazy (Cons (y, t2))) ->
      lazy (Cons (f x y, zip_with f t1 t2))

let zip t1 t2 = zip_with (fun x y -> (x, y)) t1 t2

let flatten t =
  let lazy_concat t lt = fold_right (fun x lt -> lazy (Cons (x, !!lt))) t lt in
  fold_right lazy_concat t (lazy (lazy Nil))

let cycle t = continually t |> flatten

let filter p t =
  fold_right
    (fun x lt -> if p x then lazy (Cons (x, !!lt)) else !!lt)
    t
    (lazy (lazy Nil))

let of_array = function
  | [||] -> lazy Nil
  | arr ->
      let length = Array.length arr in
      let rec loop i =
        if i = length then lazy Nil else lazy (Cons (arr.(i), loop (i + 1)))
      in
      loop 0

let enum_from z = iterate z (fun x -> x + 1)
let enum_from_to low high = enum_from low |> take_while (fun x -> x <= high)

let unfold s f =
  let rec loop s =
    match f s with Some (s, x) -> lazy (Cons (x, loop s)) | None -> lazy Nil
  in
  loop s

let zip_all_with f t1 t2 =
  unfold (t1, t2) (function
    | (lazy Nil), (lazy Nil) -> None
    | (lazy (Cons (x, t1))), (lazy Nil) -> Some ((t1, lazy Nil), f (Some x) None)
    | (lazy Nil), (lazy (Cons (y, t2))) -> Some ((lazy Nil, t2), f None (Some y))
    | (lazy (Cons (x, t1))), (lazy (Cons (y, t2))) ->
        Some ((t1, t2), f (Some x) (Some y)))

let zip_all t1 t2 = zip_all_with (fun x y -> (x, y)) t1 t2

let equal f t1 t2 =
  zip_all_with
    (fun xo yo ->
      match (xo, yo) with
      | Some _, None | None, Some _ -> false
      | Some x, Some y -> f x y
      | None, None -> assert false)
    t1 t2
  |> for_all (fun x -> x)

let rec find p = function
  | (lazy Nil) -> None
  | (lazy (Cons (x, t))) -> if p x then Some x else find p t

let flat_map f t = map f t |> flatten

let map_filter f t =
  fold_right
    (fun x lt ->
      match f x with Some y -> lazy (Cons (y, !!lt)) | None -> !!lt)
    t
    (lazy (lazy Nil))

let length t = fold_left (fun n _ -> n + 1) 0 t

let%test_module _ =
  ( module struct
    let print t =
      let sep ppf () =
        Format.pp_print_string ppf "," ;
        Format.pp_print_space ppf () in
      Format.fprintf Format.std_formatter "@[%a@]@."
        (pp ~sep Format.pp_print_int)
        t

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

    let%test "for_all - no" =
      not (for_all (fun x -> x mod 2 = 0) (items [0; 1; 4]))

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
      55 = fold_right (fun x n -> x + !!n) (enum_from_to 1 10) (lazy 0)

    let%test "fold_left" =
      55 = fold_left (fun n x -> n + x) 0 (enum_from_to 1 10)

    let%test "length" = 3 = length (items [1; 2; 3])
    let%test "equal - yes" = equal ( = ) (items [1; 2; 3]) (items [1; 2; 3])
    let%test "equal - no" = not (equal ( = ) (items [1; 2; 3]) (continually 1))
  end )
