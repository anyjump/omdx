(* src/sexp.ml *)
open Ast.Impl (* Use types from Ast.Impl *)

type t =
  | Atom of string
  | List of t list

let atom s = Atom s

(* Function to convert a list of attributes or metadata to Sexp *)
let sexp_of_kv_list kvs = List (List.map (fun (k, v) -> List [ Atom k; Atom v ]) kvs)

(* Function to convert metadata option to Sexp *)
let sexp_of_metadata (meta_opt : metadata option) : t =
  match meta_opt with
  | None -> List [ Atom "metadata"; Atom "none" ]
  | Some kvs -> List [ Atom "metadata"; sexp_of_kv_list kvs ]
;;

(* Keep existing inline and block converters *)
let rec link { label; destination; title; _ } =
  let title_sexp =
    match title with
    | Some title -> [ Atom title ]
    | None -> []
  in
  List (Atom "link" :: inline label :: Atom destination :: title_sexp)

and inline = function
  | Concat (_, xs) -> List (Atom "concat" :: List.map inline xs)
  | Text (_, s) -> Atom s
  | Emph (_, il) -> List [ Atom "emph"; inline il ]
  | Strong (_, il) -> List [ Atom "strong"; inline il ]
  | Code (_, s) -> List [ Atom "code"; Atom s ] (* Fix: include the code string *)
  | Hard_break _ -> Atom "hard-break"
  | Soft_break _ -> Atom "soft-break"
  | Link (attrs, def) ->
    List (Atom "url" :: sexp_of_kv_list attrs :: [ link def ]) (* Add attributes *)
  | Html (_, s) -> List [ Atom "html"; Atom s ]
  | Image (attrs, def) -> List (Atom "img" :: sexp_of_kv_list attrs :: [ link def ])
;;

(* Add attributes and link structure *)

let table_header (header, alignment) =
  List
    [ inline header
    ; (match alignment with
       | Default -> Atom "default"
       | Left -> Atom "left"
       | Centre -> Atom "centre"
       | Right -> Atom "right")
    ]
;;

let rec block = function
  | Paragraph (attrs, x) ->
    List (Atom "paragraph" :: sexp_of_kv_list attrs :: [ inline x ])
  | List (attrs, ty, sp, bls) ->
    let list_type_sexp =
      match ty with
      | Ordered (n, c) ->
        List [ Atom "ordered"; Atom (string_of_int n); Atom (String.make 1 c) ]
      | Bullet c -> List [ Atom "bullet"; Atom (String.make 1 c) ]
    in
    let spacing_sexp =
      match sp with
      | Tight -> Atom "tight"
      | Loose -> Atom "loose"
    in
    List
      (Atom "list"
       :: sexp_of_kv_list attrs
       :: list_type_sexp
       :: spacing_sexp
       :: List.map (fun xs -> List (Atom "list-item" :: List.map block xs)) bls)
  | Blockquote (attrs, xs) ->
    List (Atom "blockquote" :: sexp_of_kv_list attrs :: List.map block xs)
  | Thematic_break attrs -> List [ Atom "thematic-break"; sexp_of_kv_list attrs ]
  | Heading (attrs, level, text) ->
    List
      [ Atom "heading"; sexp_of_kv_list attrs; Atom (string_of_int level); inline text ]
  | Code_block (attrs, info, code) ->
    List [ Atom "code-block"; sexp_of_kv_list attrs; Atom info; Atom code ]
    (* Include code *)
  | Html_block (attrs, tag, xs) ->
    List (Atom "html-block" :: Atom tag :: sexp_of_kv_list attrs :: List.map block xs)
  | Definition_list (attrs, l) ->
    List
      [ Atom "def-list"
      ; sexp_of_kv_list attrs
      ; List
          (List.map
             (fun elt -> List [ inline elt.term; List (List.map inline elt.defs) ])
             l)
      ]
  | Table (attrs, headers, rows) ->
    List
      [ Atom "table"
      ; sexp_of_kv_list attrs
      ; List (List.map table_header headers)
      ; List (List.map (fun row -> List (List.map inline row)) rows)
      ]
;;

(* Modify create to take the whole document *)
let create (doc : document) : t =
  List
    [ sexp_of_metadata doc.metadata (* Add metadata Sexp *)
    ; List (List.map block doc.content) (* Existing content Sexp *)
    ]
;;

(* Keep existing print logic *)
let needs_quotes s =
  let rec loop i =
    if i >= String.length s
    then false
    else (
      match s.[i] with
      | ' ' | '\t' | '\x00' .. '\x1F' | '\x7F' .. '\x9F' | '(' | ')' | '"' | '\\' ->
        true (* Added parens, quote, backslash *)
      | _ -> loop (succ i))
  in
  s = "" || loop 0 (* Handle empty string and special chars *)
;;

let rec print ppf = function
  | Atom s when needs_quotes s -> Format.fprintf ppf "%S" s
  | Atom s -> Format.pp_print_string ppf s
  | List [] -> Format.pp_print_string ppf "()" (* Explicit empty list *)
  | List l ->
    Format.fprintf
      ppf
      "@[<1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print)
      l
;;
