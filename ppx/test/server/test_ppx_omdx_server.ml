(* This file will be preprocessed by ppx_mdx *)

(* Make sure Omdx types are available if needed for type checking *)
(* open Omdx *)

let%expect_test "Debug JSX String of Basic OMDX Inclusion" =
  let generated_code_string : string = [%debug_jsx_of_file "../test.md"] in
  print_endline generated_code_string;
  [%expect
    {|
    (([((h1 ~children:[React.string "Hello"] ~id:"hello" ())
      [@JSX ]);
      ((p
          ~children:[React.string "This is ";
                    ((strong ~children:[React.string "markdown"] ())
                    [@JSX ]);
                    React.string "."] ())
      [@JSX ]);
      ((pre
          ~children:[((code ~children:[React.string "let x = 1\n"]
                         ~className:"language-ocaml" ())
                    [@JSX ])] ())
      [@JSX ]);
      ((p ~children:[React.null; React.null] ())
      [@JSX ])])
    [@JSX ])
    |}]
;;

let%expect_test "JSX of Basic OMDX Inclusion" =
  let _generated_element : React.element = [%jsx_of_file "../test.md"] in
  print_endline "none";
  [%expect
    {| none |}]
;;

(* let%expect_test "JSX Elt String of Basic OMDX Inclusion" = *)
(*   let react_elt : React.element = [%ppx_omdx.jsx_of_file "../test.md"] in *)
(*   print_endline react_elt; *)
(*   [%expect *)
(*     {| *)
(*     [[[((h1 ~children:[React.string "Hello"] ~id:"hello" ()) *)
(*       [@JSX ]); *)
(*       ((p *)
(*           ~children:[[[React.string "This is "; *)
(*                       ((strong ~children:[React.string "markdown"] ()) *)
(*                       [@JSX ])]; *)
(*                      React.string "."]] ()) *)
(*       [@JSX ])]; *)
(*      ((pre *)
(*          ~children:[((code ~children:[React.string "let x = 1\n"] *)
(*                         ~className:"language-ocaml" ()) *)
(*                    [@JSX ])] ()) *)
(*      [@JSX ])]; *)
(*     ((p ~children:[[React.null; React.null]] ()) *)
(*     [@JSX ])] *)
(*     |}] *)
(* ;; *)

(* let%expect_test "MDX with HTML Block" = *)
(*   (* Add another test file: test/html_block.mdx *) *)
(*   (* <my-tag class="foo"> <p>Inner content</p> </my-tag> *) *)
(*   let generated_html : Omdx.Html.t = [%omdx "html_block.mdx"] in *)
(*   print_endline (Omdx.Html.to_string generated_html); *)
(*   [%expect {| <my-tag class="foo"><p>Inner content</p></my-tag> |}] *)
(* ;; *)
(**)
(* (* Add more tests for different features, errors, etc. *) *)
(* let%expect_test "File Not Found" = *)
(*   try *)
(*     let (_ : Omdx.Html.t) = [%omdx "nonexistent.mdx"] in *)
(*     print_endline "Should have failed" *)
(*   with *)
(*   (* The exact error message might depend on ppxlib version etc. *)
(*       Adjust the pattern accordingly. Use `dune runtest` to see the *)
(*       actual error output the first time. *) *)
(*   | Location.Error err -> print_endline (Location.Error.message err) *)
(*   | Failure msg -> *)
(*     print_endline msg (* Fallback for simple failwith *); *)
(*     [%expect *)
(*       {| ppx_mdx error: Unable to read file: test/nonexistent.mdx - No such file or directory |}] *)
(* ;; *)
