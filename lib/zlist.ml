(** For convenience. *)
let ( !! ) =
  Lazy.force

module Lazy_list = struct
  type 'a t =
    | Nil
    | Cons of 'a Lazy.t * 'a t Lazy.t

  let unit a =
    Cons (lazy a, lazy Nil)

  let head = function
    | Nil -> None
    | Cons (x, _) -> Some !!x

  let tail = function
    | Nil -> Nil
    | Cons (_, xs) -> !!xs

  let rec concat a b =
    match a with
    | Nil -> b
    | Cons (x, xs) -> Cons (x, lazy (concat !!xs b))

  let rec continually x =
    Cons (lazy x, lazy (continually x))

  let rec fold_right z f = function
    | Nil -> !!z
    | Cons (x, xs) -> f !!x (lazy (fold_right z f !!xs))

  let fold_left z f t =
    let rec loop accum = function
      | Nil -> accum
      | Cons (x, xs) -> loop (f accum !!x) !!xs
    in
    loop z t

  let exists f =
    fold_right (lazy false) (fun a lb -> f a || !!lb)

  let rec for_all f = function
    | Nil -> true
    | Cons (x, xs) -> f !!x && (for_all f !!xs)

  let rec take n t =
    if n <= 0 then Nil
    else
      match t with
      | Nil -> Nil
      | Cons (x, xs) -> Cons (x, lazy (take (n - 1) !!xs))

  let rec take_while p = function
    | Nil -> Nil
    | Cons (x, xs) -> begin
        if p !!x then Cons (x, lazy (take_while p !!xs))
        else Nil
      end

  let rec drop n t =
    if n <= 0 then t
    else
      match t with
      | Nil -> Nil
      | Cons (_, xs) -> drop (n - 1) !!xs

  let rec drop_while p = function
    | Nil -> Nil
    | Cons (x, xs) as t -> begin
        if p !!x then drop_while p !!xs
        else t
      end

  let rec map f = function
    | Nil -> Nil
    | Cons (x, xs) -> Cons (lazy (f !!x), lazy (map f !!xs))

  let filter f =
    fold_right (lazy Nil)
      (fun x xs ->
         if f x then Cons (lazy x, xs)
         else !!xs)

  let rec fill n x =
    if n <= 0 then Nil
    else
      Cons (lazy x, lazy (fill (n - 1) x))

  let of_array = function
    | [||] -> Nil
    | arr -> begin
        let rec loop index =
          if index = Array.length arr then Nil
          else
            Cons (lazy arr.(index), lazy (loop (index + 1)))
        in
        loop 0
      end

  let of_list xs =
    List.fold_right (fun x xs -> Cons (lazy x, lazy xs)) xs Nil

  let to_list (t : 'a t) =
    fold_right (lazy []) (fun x xs -> x :: !!xs) t

  let rec iterate z f =
    Cons (lazy z, lazy (iterate (f z) f))

  let enum_from z =
    iterate z (fun x -> x + 1)

  let enum_from_to low high =
    enum_from low |> take_while (fun x -> x <= high)

  let rec zip_with f tx ty =
    match (tx, ty) with
    | (Nil, _) | (_, Nil) -> Nil
    | (Cons (x, lxs), Cons (y, lys)) -> Cons (lazy (f !!x !!y), lazy (zip_with f !!lxs !!lys))

  let zip (tx : 'a t) (ty : 'b t) =
    zip_with (fun x y -> (x, y)) tx ty

  let rec iter f = function
    | Nil -> ()
    | Cons (x, xs) -> begin
        f !!x;
        iter f !!xs
      end

  let unfold s f =
    let rec loop s =
      match f s with
      | Some (s, a) -> Cons (lazy a, lazy (loop s))
      | None -> Nil
    in
    loop s

  let zip_all_with f tx ty  =
    unfold (tx, ty) begin function
      | Nil, Nil -> None
      | Cons (x, xs), Nil -> Some ((!!xs, Nil), f (Some !!x) None)
      | Nil, Cons (y, ys) -> Some ((Nil, !!ys), f None (Some !!y))
      | Cons (x, xs), Cons (y, ys) -> Some ((!!xs, !!ys), f (Some !!x) (Some !!y))
    end

  let zip_all tx ty =
    zip_all_with (fun x y -> (x, y)) tx ty

  let rec find p = function
    | Cons (x, xs) -> if p !!x then Some !!x else find p !!xs
    | Nil -> None

  let flat_map f t =
    let lazy_concat t lt = fold_right lt (fun x lt -> Cons (lazy x, lt)) t in
    fold_right (lazy Nil) (fun x lt -> lazy_concat (f x) lt) t

  let flatten t =
    flat_map (fun xs -> xs) t

  let rec map_filter f = function
    | Nil -> Nil
    | Cons (x, xs) -> begin
        match f !!x with
        | None -> map_filter f !!xs
        | Some y -> Cons (lazy y, lazy (map_filter f !!xs))
      end

  let length t =
    fold_left 0 (+) t

  let cycle t =
    continually t |> flatten
end

type 'a lazy_list =
  'a Lazy_list.t
