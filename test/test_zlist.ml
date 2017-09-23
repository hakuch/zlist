open Zlist

open Alcotest

let lazy_list (type a) inner =
  let module M = struct
    type t =
      a Lazy_list.t

    let equal expected actual =
      Lazy_list.equal Pervasives.(=) expected actual

    let pp =
      Fmt.using Lazy_list.strict (Fmt.list (pp inner))
  end
  in
  (module M : TESTABLE with type t = a Lazy_list.t)

module Test_lazy_list = struct
  open Lazy_list

  let construction = [
    begin "of_array", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [1; 2; 3])
          (of_array [| 1; 2; 3 |]);

        check (lazy_list int) "empty" (lazy Nil)
          (of_array [| |])
    end;

    begin "elems", `Quick, fun () ->
        check (lazy_list string) "basic" (elems ["abc"])
          (unit "abc")
    end;
  ]

  let generation = [
    begin "fill", `Quick, fun () ->
        check (lazy_list string) "basic" (elems ["aa"; "aa"; "aa"])
          (fill 3 "aa")
    end;

    begin "unfold", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [8; 6; 4; 2; 0])
          (unfold 4 (fun s -> if s = -1 then None else Some (s - 1, 2 * s)))
    end;

    begin "iterate", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [0; 1; 2; 3; 4])
          (iterate 0 ((+) 1) |> take 5)
    end;

    begin "continually", `Quick, fun () ->
        check (lazy_list bool) "basic" (elems [false; false; false])
          (continually false |> take 3)
    end;

    begin "enum_from", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [5; 6; 7; 8])
          (enum_from 5 |> take 4)
    end;

    begin "enum_from_to", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [3; 4; 5; 6])
          (enum_from_to 3 6)
    end;

    begin "cycle", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [1; 2; 3; 1; 2; 3; 1])
          (cycle (elems [1; 2; 3]) |> take 7)
    end;
  ]

  let manipulation = [
    begin "head", `Quick, fun () ->
        check (option char) "basic" (Some 'a')
          (elems ['a'; 'b'; 'c'] |> head);

        check (option char) "empty" None
          (head (lazy Nil));
    end;

    begin "tail", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [2; 3])
          (elems [1; 2; 3] |> tail);

        check (lazy_list int) "empty" (lazy Nil)
          (tail (lazy Nil))
    end;

    begin "take", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [1; 2; 3])
          (elems [1; 2; 3; 4] |> take 3);

        check (lazy_list int) "insufficient" (elems [1; 2; 3])
          (elems [1; 2; 3] |> take 200);
    end;

    begin "take_while", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [0; 1; 2])
          (enum_from 0 |> take_while (fun x -> x <= 2))
    end;

    begin "drop", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [3; 4; 5])
          (elems [1; 2; 3; 4; 5] |> drop 2);

        check (lazy_list int) "insufficient" (elems [])
          (unit 2 |> drop 10)
    end;

    begin "drop_while", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [3; 4; 5])
          (enum_from_to 0 5 |> drop_while (fun x -> x <= 2))
    end;

    begin "map", `Quick, fun () ->
        check (lazy_list (pair int int)) "basic" (elems [(1, 1); (2, 2); (3, 3)])
          (enum_from 1 |> map (fun x -> (x, x)) |> take 3)
    end;

    begin "flat_map", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [1; 1; 1; 2; 2; 2; 3])
          begin
            enum_from 1
            |> flat_map (fun x -> elems [x; x; x])
            |> take 7
          end
    end;

    begin "filter", `Quick, fun () ->
        check (lazy_list string) "basic" (elems ["dog"; "cat"])
          begin
            elems ["lion"; "dog"; "parrot"; "cat"]
            |> filter (fun s -> String.length s <= 3)
          end
    end;

    begin "map_filter", `Quick, fun () ->
        check (lazy_list char) "basic" (elems ['a'; 'b'])
          begin
            elems ['a', 100; 'b', 200; 'c', 3]
            |> map_filter (fun (c, n) -> if n >= 100 then Some c else None)
          end
    end;

    begin "flatten", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [1; 2; 1; 2])
          begin
            continually (elems [1; 2])
            |> flatten
            |> take 4
          end
    end;
  ]

  let combining = [
    begin "concat", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [1; 2; 3; 4])
          (concat (elems [1; 2]) (elems [3; 4]))
    end;

    begin "zip_with", `Quick, fun () ->
        check (lazy_list int) "basic" (elems [2; 4; 6])
          begin
            zip_with (fun a b -> a + b) (enum_from 1) (enum_from 1)
            |> take 3
          end
    end;

    begin "zip", `Quick, fun () ->
        check (lazy_list (pair int int)) "basic" (elems [(1, 10); (2, 20)])
          begin
            zip (enum_from 1) (enum_from 1 |> map (fun x -> 10 * x))
            |> take 2
          end
    end;

    begin "zip_all_with", `Quick, fun () ->
        let map2 f tx ty =
          match tx, ty with
          | Some x, Some y -> Some (f x y)
          | _ -> None
        in

        check (lazy_list (option int)) "same size" (elems [Some 11; Some 13; Some 15])
          begin
            zip_all_with (map2 (+)) (enum_from 1) (enum_from 10)
            |> take 3
          end;

        check (lazy_list (option int)) "different size" (elems [Some 11; None; None])
          begin
            zip_all_with (map2 (+)) (enum_from 1) (unit 10)
            |> take 3
          end
    end;

    begin "zip_all", `Quick, fun () ->
        check (lazy_list (pair (option int) (option int))) "same size"
          (elems [Some 1, Some 10; Some 2, Some 11; Some 3, Some 12])
          begin
            zip_all (enum_from 1) (enum_from 10)
            |> take 3
          end;

        check (lazy_list (pair (option int) (option int))) "different size"
          (elems [Some 1, Some 10; Some 2, None; Some 3, None])
          begin
            zip_all (enum_from 1) (unit 10)
            |> take 3
          end
    end;
  ]

  let folding = [
    begin "fold_right", `Quick, fun () ->
        check int "basic" (-2)
          begin
            elems [1; 2; 3; 4]
            |> fold_right (lazy 0) (fun a lb -> a - Lazy.force lb)
          end
    end;

    begin "length", `Quick, fun () ->
        check int "empty" 0 (length (lazy Nil));
        check int "basic" 3 (length (elems [10; 20; 30]))
    end;

    begin "fold_left", `Quick, fun () ->
        check int "basic" 10
          begin
            elems [1; 2; 3; 4]
            |> fold_left 0 ((+))
          end
    end;
  ]

  let iterating = [
    begin "iter", `Quick, fun () ->
        let xs = ref [] in

        elems [1; 2; 3; 4] |> iter (fun x -> xs := x :: !xs);

        check (list int) "basic" [4; 3; 2; 1]
          !xs
    end;
  ]

  let test_suite = [
    "construction", construction;
    "generation", generation;
    "manipulation", manipulation;
    "combining", combining;
    "folding", folding;
    "iterating", iterating;
  ]
end

let () =
  run "lazy_list" Test_lazy_list.test_suite
