#use "topfind" ;;
#require "topkg" ;;

open Topkg

let () =
  Pkg.describe
    ~licenses:[Pkg.std_file "LICENSE"]
    "zlist" @@ fun c ->
  Ok [
    Pkg.mllib "lib/zlist.mllib";
    Pkg.test "test/test_zlist";
  ]
