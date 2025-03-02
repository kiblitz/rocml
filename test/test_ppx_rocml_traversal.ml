open! Core
open! Import

let[@kernel debug_string] f (x : _) (y : _) = x + y

let%expect_test "Top level function" =
  print_endline f;
  [%expect
    {|
    ((func_args (((name x)) ((name y))))
     (body
      (Apply (f (Identifier ((name +))))
       (args
        (((label Unnamed) (expression (Identifier ((name x)))))
         ((label Unnamed) (expression (Identifier ((name y)))))))))) |}]
;;

let%expect_test "Non-top level function" =
  let[@kernel debug_string] f (x : _) (y : _) = x + y in
  print_endline f;
  [%expect
    {|
    ((func_args (((name x)) ((name y))))
     (body
      (Apply (f (Identifier ((name +))))
       (args
        (((label Unnamed) (expression (Identifier ((name x)))))
         ((label Unnamed) (expression (Identifier ((name y)))))))))) |}]
;;

let g =
  let[@kernel debug_string] f (x : _) (y : _) = x + y in
  f
;;

let%expect_test "Non-top level function from top level" =
  print_endline g;
  [%expect
    {|
    ((func_args (((name x)) ((name y))))
     (body
      (Apply (f (Identifier ((name +))))
       (args
        (((label Unnamed) (expression (Identifier ((name x)))))
         ((label Unnamed) (expression (Identifier ((name y)))))))))) |}]
;;

module T = struct
  let h =
    let[@kernel debug_string] f (x : _) (y : _) = x + y in
    f
  ;;

  let%expect_test "Function from module" =
    print_endline h;
    [%expect
      {|
    ((func_args (((name x)) ((name y))))
     (body
      (Apply (f (Identifier ((name +))))
       (args
        (((label Unnamed) (expression (Identifier ((name x)))))
         ((label Unnamed) (expression (Identifier ((name y)))))))))) |}]
  ;;
end
