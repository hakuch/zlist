let ( !! ) = Lazy.force

type 'a t = 'a node Lazy.t

and 'a node = Nil | Cons of 'a * 'a t

let rec fold_right z f = function
  | (lazy Nil) -> !!z
  | (lazy (Cons (x, t))) -> f x (lazy (fold_right z f t))

let strict t = fold_right (lazy []) (fun x t -> x :: !!t) t

let%test _ = strict (lazy Nil) = []
let%test _ = strict (lazy (Cons (10, lazy (Cons (20, lazy Nil))))) = [10; 20]

let test_elems t xs = strict t = xs
let unit x = lazy (Cons (x, lazy Nil))
let head = function (lazy Nil) -> None | (lazy (Cons (x, _))) -> Some x
let tail = function (lazy Nil) -> lazy Nil | (lazy (Cons (_, t))) -> t
let elems xs = List.fold_right (fun x t -> lazy (Cons (x, t))) xs (lazy Nil)

let%test _ = test_elems (elems [1; 2; 3]) [1; 2; 3]

let rec concat t1 t2 =
  match t1 with
  | (lazy Nil) -> t2
  | (lazy (Cons (lx, t))) -> lazy (Cons (lx, concat t t2))

let%test _ =
  test_elems (concat (elems [1; 2; 3]) (elems [4; 5; 6])) [1; 2; 3; 4; 5; 6]

let rec continually x = lazy (Cons (x, continually x))

let fold_left z f t =
  let rec loop acc = function
    | (lazy Nil) -> acc
    | (lazy (Cons (x, t))) -> loop (f acc x) t in
  loop z t

let%test _ = fold_left 0 ( + ) (elems [1; 2; 3]) = 6

let rec take n t =
  if n <= 0 then lazy Nil
  else
    match t with
    | (lazy Nil) -> lazy Nil
    | (lazy (Cons (x, t))) -> lazy (Cons (x, take (n - 1) t))

let%test _ = test_elems (continually 1 |> take 3) [1; 1; 1]
let%test _ = test_elems (unit 1 |> take 3) [1]

let rec take_while p = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t))) ->
      if p x then lazy (Cons (x, take_while p t)) else lazy Nil

let%test _ = test_elems (elems [1; 2; 3] |> take_while (fun x -> x <> 3)) [1; 2]

let rec iterate z f = lazy (Cons (z, iterate (f z) f))

let%test _ =
  let t = iterate 0 (fun x -> x + 1) in
  test_elems (take 5 t) [0; 1; 2; 3; 4]

let exists f = fold_right (lazy false) (fun a lb -> f a || !!lb)

let%test _ = exists (fun x -> x = 1) (elems [1; 2; 3])
let%test _ = not (exists (fun x -> x = 1) (elems [2; 3]))
let%test _ = exists (fun x -> x = 5) (iterate 0 (fun x -> x + 1))

let rec for_all f = function
  | (lazy Nil) -> true
  | (lazy (Cons (x, t))) -> f x && for_all f t

let%test _ = for_all (fun _ -> true) (lazy Nil)
let%test _ = for_all (fun x -> x mod 2 = 0) (elems [0; 2; 4])
let%test _ = not (for_all (fun x -> x mod 2 = 0) (elems [0; 1; 4]))

let rec drop n t =
  if n <= 0 then t
  else
    match t with
    | (lazy Nil) -> lazy Nil
    | (lazy (Cons (_, t))) -> drop (n - 1) t

let%test _ = test_elems (drop 5 (lazy Nil)) []
let%test _ = test_elems (drop 2 (elems [1; 2; 3; 4])) [3; 4]

let rec drop_while p = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t))) as tt -> if p x then drop_while p t else tt

let%test _ =
  test_elems (elems [1; 3; 2; 5] |> drop_while (fun x -> x <> 2)) [2; 5]

let fill n x = continually x |> take n

let%test _ = test_elems (fill 3 'a') ['a'; 'a'; 'a']

let rec map f = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, lxs))) -> lazy (Cons (f x, map f lxs))

let%test _ =
  test_elems (continually 0 |> map (fun x -> x + 1) |> take 3) [1; 1; 1]

let rec zip_with f t1 t2 =
  match (t1, t2) with
  | (lazy Nil), _ | _, (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t1))), (lazy (Cons (y, t2))) ->
      lazy (Cons (f x y, zip_with f t1 t2))

let%test _ =
  test_elems
    (zip_with (fun x y -> x + y) (elems [1; 2; 3]) (elems [10; 20; 30]))
    [11; 22; 33]

let zip t1 t2 = zip_with (fun x y -> (x, y)) t1 t2

let%test _ =
  test_elems
    (zip (elems [1; 2; 3]) (elems ['a'; 'b'; 'c']))
    [(1, 'a'); (2, 'b'); (3, 'c')]

let flatten t =
  let lazy_concat t lt = fold_right lt (fun x lt -> lazy (Cons (x, !!lt))) t in
  fold_right (lazy (lazy Nil)) lazy_concat t

let%test _ =
  test_elems (continually (elems [1; 2; 3]) |> flatten |> take 5) [1; 2; 3; 1; 2]

let cycle t = continually t |> flatten

let%test _ = test_elems (cycle (elems [1; 2; 3]) |> take 7) [1; 2; 3; 1; 2; 3; 1]

let filter p t =
  fold_right
    (lazy (lazy Nil))
    (fun x lt -> if p x then lazy (Cons (x, !!lt)) else !!lt)
    t

let%test _ =
  test_elems
    (cycle (elems [1; 2]) |> filter (fun x -> x < 2) |> take 3)
    [1; 1; 1]

let of_array = function
  | [||] -> lazy Nil
  | arr ->
      let length = Array.length arr in
      let rec loop i =
        if i = length then lazy Nil else lazy (Cons (arr.(i), loop (i + 1)))
      in
      loop 0

let%test _ = test_elems (of_array [|1; 2; 3|]) [1; 2; 3]

let enum_from z = iterate z (fun x -> x + 1)

let%test _ = test_elems (enum_from 0 |> take 3) [0; 1; 2]

let enum_from_to low high = enum_from low |> take_while (fun x -> x <= high)

let%test _ = test_elems (enum_from_to 0 3) [0; 1; 2; 3]

let rec iter f = function
  | (lazy Nil) -> ()
  | (lazy (Cons (x, t))) -> f x ; iter f t

let%test _ =
  let xs = ref [] in
  elems [1; 2; 3] |> iter (fun x -> xs := x :: !xs) ;
  !xs = [3; 2; 1]

let unfold s f =
  let rec loop s =
    match f s with Some (s, x) -> lazy (Cons (x, loop s)) | None -> lazy Nil
  in
  loop s

let%test _ =
  let f i = if i < 3 then Some (i + 1, 10) else None in
  test_elems (unfold 0 f) [10; 10; 10]

let zip_all_with f t1 t2 =
  unfold (t1, t2) (function
    | (lazy Nil), (lazy Nil) -> None
    | (lazy (Cons (x, t1))), (lazy Nil) -> Some ((t1, lazy Nil), f (Some x) None)
    | (lazy Nil), (lazy (Cons (y, t2))) -> Some ((lazy Nil, t2), f None (Some y))
    | (lazy (Cons (x, t1))), (lazy (Cons (y, t2))) ->
        Some ((t1, t2), f (Some x) (Some y)))

let%test _ =
  let f mx my = match (mx, my) with Some x, Some y -> x + y | _ -> -1 in
  test_elems (zip_all_with f (elems [2; 3]) (elems [10; 20; 30])) [12; 23; -1]

let zip_all t1 t2 = zip_all_with (fun x y -> (x, y)) t1 t2

let%test _ =
  test_elems
    (zip_all (elems [1; 2]) (elems [1; 2; 3]))
    [(Some 1, Some 1); (Some 2, Some 2); (None, Some 3)]

let equal f t1 t2 =
  zip_all_with
    (fun xo yo ->
      match (xo, yo) with
      | Some _, None | None, Some _ -> false
      | Some x, Some y -> f x y
      | None, None -> assert false)
    t1 t2
  |> for_all (fun x -> x)

let%test _ = equal ( = ) (elems [1; 2; 3]) (elems [1; 2; 3])
let%test _ = not (equal ( = ) (elems [1; 2; 4]) (elems [1; 2; 3]))

let rec find p = function
  | (lazy Nil) -> None
  | (lazy (Cons (x, t))) -> if p x then Some x else find p t

let%test _ = find (fun x -> 2 * x = 6) (elems [1; 3; 5]) = Some 3
let%test _ = find (fun x -> x > 100) (elems [10; 20]) = None

let flat_map f t = map f t |> flatten

let%test _ =
  test_elems
    (enum_from 0 |> flat_map (fun x -> elems [x; x + 1]) |> take 4)
    [0; 1; 1; 2]

let map_filter f t =
  fold_right
    (lazy (lazy Nil))
    (fun x lt ->
      match f x with Some y -> lazy (Cons (y, !!lt)) | None -> !!lt)
    t

let%test _ =
  test_elems
    ( enum_from_to 0 10
    |> map_filter (fun x -> if 10 * x < 50 then Some x else None) )
    [0; 1; 2; 3; 4]

let length t = fold_left 0 (fun n _ -> n + 1) t

let%test _ = length (elems [1; 2; 3]) = 3
