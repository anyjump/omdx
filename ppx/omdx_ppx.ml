open Ppxlib
module Builder = Ast_builder.Default

(* Assuming Omdx module is available *)
(* open Omdx *)
module StringMap = Map.Make (String)

(* There's no pexp_list on Ppxlib since it isn't a constructor of the Parsetree *)
let pexp_list ~loc xs =
  ListLabels.fold_left
    ~init:[%expr []]
    ~f:(fun xs x ->
      let loc = x.pexp_loc in
      [%expr [%e x] :: [%e xs]])
    (ListLabels.rev xs)
;;

(* Helper to check if a tag looks like a React component *)
let is_react_component tag_name =
  if String.length tag_name = 0
  then false
  else (
    let first_char = String.get tag_name 0 in
    first_char >= 'A' && first_char <= 'Z')
;;

(* Helper to create React.string "foo" *)
(* Needed because reason_react_ppx expects text leaves as React.string *)
let react_string ~loc s = [%expr React.string [%e Builder.estring ~loc s]]

(* Helper to create React.null *)
(* Needed because reason_react_ppx expects null leaves as React.null *)
let react_null ~loc = [%expr React.null]

(* Helper to create an OCaml list expression: [ expr1; expr2; ... ] *)
(* This is used for the ~children prop value and for top-level fragments *)
let list_expr ~loc expr_list = Builder.elist ~loc expr_list

(* Helper: Create the [@JSX] attribute *)
let jsx_attribute ~loc =
  Builder.attribute ~loc ~name:{ txt = "JSX"; loc } ~payload:(PStr [])
;;

(* Convert Omdx.Html.t into a list of JSX-ready expressions.
   Flattens Concat nodes. Returns an empty list for Null. *)
let rec jsx_expr_list_of_html ~loc (t : Omdx.Html.t) : expression list =
  match t with
  | Null -> []
  | Text s -> [ react_string ~loc s ] (* Use React.string for text *)
  | Raw _ ->
    (* Represent ignored raw HTML as React.null *)
    [ react_null ~loc (* ppx_mdx_jsx: Raw HTML ignored *) ]
  | Element (_etype, tag_name, attrs, child_opt) ->
    (* Create a single JSX element expression (which will have [@JSX] added) *)
    [ create_jsx_element ~loc tag_name attrs child_opt ]
  | Concat (t1, t2) ->
    (* Recursively process and flatten *)
    jsx_expr_list_of_html ~loc t1 @ jsx_expr_list_of_html ~loc t2

(* Create the actual JSX element application (div(...) [@JSX] or Comp.make(...) [@JSX]) *)
and create_jsx_element ~loc tag_name attrs child_opt =
  (* 1. Process Attributes *)
  let props =
    attrs
    |> List.map (fun (key, value_str) ->
      let value_expr = Builder.estring ~loc value_str in
      let arg_name =
        match key with
        | "class" -> "className"
        | "for" -> "htmlFor"
        | "aria-labelledby" -> "ariaLabelledBy"
        | "aria-checked" -> "ariaChecked"
        | "aria-expanded" -> "ariaExpanded"
        | "aria-pressed" -> "ariaPressed"
        | key when String.starts_with ~prefix:"data-" key -> key
        | _ -> key
      in
      Labelled arg_name, value_expr)
  in
  (* 2. Process Children *)
  let children_expr_list =
    match child_opt with
    | None -> []
    | Some child_t -> jsx_expr_list_of_html ~loc child_t
  in
  (* Create the OCaml list expression for the ~children prop value *)
  let children_list_prop_val = list_expr ~loc children_expr_list in
  let children_prop = Labelled "children", children_list_prop_val in
  let props_with_children = children_prop :: props in
  (* 3. Add the final Nolabel unit arg *)
  let final_props_with_unit = props_with_children @ [ Nolabel, [%expr ()] ] in
  (* 4. Construct the application *)
  let element_fn_expr =
    if is_react_component tag_name
    then Builder.pexp_ident ~loc { txt = Ldot (Lident tag_name, "make"); loc }
    else Builder.pexp_ident ~loc { txt = Lident tag_name; loc }
  in
  let apply_expr = Builder.pexp_apply ~loc element_fn_expr final_props_with_unit in
  (* 5. Add the [@JSX] attribute to the application *)
  { apply_expr with pexp_attributes = [ jsx_attribute ~loc ] }
;;

(* --- File Reading and Debugging/Other Extensions remain the same --- *)

(* Helper to read a file at compile time *)
let read_file ~loc filename =
  let source_dir = Filename.dirname loc.loc_start.pos_fname in
  let absolute_filename = Filename.concat source_dir filename in
  try
    let ic = open_in absolute_filename in
    let rec read_lines acc =
      match input_line ic with
      | line -> read_lines (line :: acc)
      | exception End_of_file ->
        close_in ic;
        List.rev acc
    in
    let result = String.concat "\n" (read_lines []) in
    result
  with
  | exn ->
    Printf.eprintf
      "[ERROR] Unable to read file: %s - %s\n%!"
      filename
      (Printexc.to_string exn);
    failwith ("Unable to read file: " ^ filename)
;;

(* ------- Original Omdx.Html AST Generation (for reference/debugging) -------- *)
let rec expr_of_html ~loc (html_t : Omdx.Html.t) : expression =
  match html_t with
  | Element (etype, name, attrs, child_opt) ->
    [%expr
      Omdx.Html.Element
        ( [%e etype_to_expr ~loc etype]
        , [%e Builder.estring ~loc name]
        , [%e attrs_to_expr ~loc attrs]
        , [%e child_opt_to_expr ~loc child_opt] )]
  | Text s -> [%expr Omdx.Html.Text [%e Builder.estring ~loc s]]
  | Raw s -> [%expr Omdx.Html.Raw [%e Builder.estring ~loc s]]
  | Null -> [%expr Omdx.Html.Null]
  | Concat (t1, t2) ->
    [%expr Omdx.Html.Concat ([%e expr_of_html ~loc t1], [%e expr_of_html ~loc t2])]

and etype_to_expr ~loc (etype : Omdx.Html.element_type) : expression =
  match etype with
  | Inline -> [%expr Omdx.Html.Inline]
  | Block -> [%expr Omdx.Html.Block]
  | Table -> [%expr Omdx.Html.Table]

and attrs_to_expr ~loc (attrs : Omdx.attributes) : expression =
  attrs
  |> List.map (fun (k, v) ->
    Builder.pexp_tuple ~loc [ Builder.estring ~loc k; Builder.estring ~loc v ])
  |> Builder.elist ~loc

and child_opt_to_expr ~loc (child_opt : Omdx.Html.t option) : expression =
  match child_opt with
  | None -> [%expr None]
  | Some t -> [%expr Some [%e expr_of_html ~loc t]]
;;

(* Helper to pretty print an expression AST node to a string *)
let string_of_expression (expr : expression) : string =
  let buffer = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buffer in
  Pprintast.expression fmt expr;
  Format.pp_print_flush fmt ();
  Buffer.contents buffer
;;

(* ------- Extension Definitions -------- *)

let html_of_file_extension =
  let expand ~ctxt filename =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let html_of_omdx =
      read_file ~loc filename |> Omdx.of_string |> Omdx.content |> Omdx.Html.of_content
    in
    expr_of_html ~loc html_of_omdx
  in
  Extension.V3.declare
    "html_of_file"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand
;;

let jsx_of_file_extension =
  let expand ~ctxt filename =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let html_t =
      read_file ~loc filename |> Omdx.of_string |> Omdx.content |> Omdx.Html.of_content
    in
    let jsx_expr_list = jsx_expr_list_of_html ~loc html_t in
    match jsx_expr_list with
    | [] -> react_null ~loc (* Use React.null for empty *)
    | [ single_expr ] -> single_expr
    | items ->
      (* Multiple top-level items -> Fragment ([@JSX] [...]) *)
      let list_construct = pexp_list ~loc items in
      { list_construct with pexp_attributes = [ jsx_attribute ~loc ] }
  in
  Extension.V3.declare
    "jsx_of_file"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand
;;

let debug_jsx_of_file_extension =
  let expand ~ctxt filename =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let html_t =
      read_file ~loc filename |> Omdx.of_string |> Omdx.content |> Omdx.Html.of_content
    in
    let jsx_expr_list = jsx_expr_list_of_html ~loc html_t in
    let final_expr =
      match jsx_expr_list with
      | [] -> react_null ~loc
      | [ single_expr ] -> single_expr
      | items ->
        let list_construct = pexp_list ~loc items in
        { list_construct with pexp_attributes = [ jsx_attribute ~loc ] }
    in
    let code_string = string_of_expression final_expr in
    Builder.estring ~loc code_string
  in
  Extension.V3.declare
    "debug_jsx_of_file"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand
;;

let html_of_file_rule = Ppxlib.Context_free.Rule.extension html_of_file_extension
let jsx_of_file_rule = Ppxlib.Context_free.Rule.extension jsx_of_file_extension

let debug_jsx_of_file_rule =
  Ppxlib.Context_free.Rule.extension debug_jsx_of_file_extension
;;

let () =
  Driver.register_transformation ~rules:[ jsx_of_file_rule ] "jsx_of_file";
  Driver.register_transformation ~rules:[ html_of_file_rule ] "html_of_file";
  Driver.register_transformation ~rules:[ debug_jsx_of_file_rule ] "debug_jsx_of_file"
;;
