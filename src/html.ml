(* src/html.ml *)
open Ast.Impl (* Make sure Ast.Impl is accessible *)

type element_type =
  | Inline
  | Block
  | Table

(* Intermediate HTML representation type *)
type t =
  | Element of element_type * string * attributes * t option
  | Text of string
  | Raw of string (* For raw HTML output, like <br /> or inline html *)
  | Null (* Represents empty content *)
  | Concat of t * t (* Concatenation of HTML elements/text *)

(* Constructor helpers for the intermediate type *)
let elt etype name attrs childs = Element (etype, name, attrs, childs)
let text s = Text s
let raw s = Raw s

(* Concatenation helper, handling Null *)
let concat t1 t2 =
  match t1, t2 with
  | Null, t | t, Null -> t
  | _ -> Concat (t1, t2)
;;

(* Map and concatenate over a list *)
let concat_map f l = List.fold_left (fun accu x -> concat accu (f x)) Null l

(* Map and concatenate over two lists *)
let concat_map2 f l1 l2 = List.fold_left2 (fun accu x y -> concat accu (f x y)) Null l1 l2

(* HTML entity escaping *)
let htmlentities s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s
    then Buffer.contents b
    else (
      (match s.[i] with
       | '"' -> Buffer.add_string b "&quot;"
       | '&' -> Buffer.add_string b "&amp;"
       | '<' -> Buffer.add_string b "&lt;"
       | '>' -> Buffer.add_string b "&gt;"
       (* Add other entities if needed, e.g., ' -> ' *)
       | c -> Buffer.add_char b c);
      loop (succ i))
  in
  loop 0
;;

(* Helper to add attributes to a buffer *)
let add_attrs_to_buffer buf attrs =
  let f (k, v) = Printf.bprintf buf " %s=\"%s\"" k (htmlentities v) in
  List.iter f attrs
;;

(* Recursive function to serialize the intermediate HTML type 't' to a buffer *)
let rec add_to_buffer buf = function
  | Element (eltype, name, attrs, None) ->
    (* Self-closing tag format - adjust if not desired *)
    Printf.bprintf buf "<%s%a />" name add_attrs_to_buffer attrs;
    if eltype = Block then Buffer.add_char buf '\n'
  | Element (eltype, name, attrs, Some c) ->
    Printf.bprintf
      buf
      "<%s%a>%s%a</%s>%s"
      name
      add_attrs_to_buffer
      attrs
      (* Add newline inside table/block elements for readability *)
      (match eltype with
       | Table | Block -> "\n"
       | _ -> "")
      add_to_buffer
      c
      name
      (match eltype with
       | Table | Block -> "\n"
       | _ -> "")
  | Text s -> Buffer.add_string buf (htmlentities s)
  | Raw s -> Buffer.add_string buf s
  | Null -> ()
  | Concat (t1, t2) ->
    add_to_buffer buf t1;
    add_to_buffer buf t2
;;

(* URI escaping *)
let escape_uri s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      (* List of allowed characters in URIs *)
      | ( 'A' .. 'Z'
        | 'a' .. 'z'
        | '0' .. '9'
        | '-'
        | '_'
        | '.'
        | '~'
        | ':'
        | '/'
        | '?'
        | '#'
        | '['
        | ']'
        | '@'
        | '!'
        | '$'
        | '&'
        | '\''
        | '('
        | ')'
        | '*'
        | '+'
        | ','
        | ';'
        | '=' ) as c -> Buffer.add_char b c
      (* Percent-encode other characters *)
      | _ as c -> Printf.bprintf b "%%%02X" (Char.code c))
    s;
  Buffer.contents b
;;

(* --- Slugify logic (unchanged) --- *)
let trim_start_while p s =
  let start = ref true in
  let b = Buffer.create (String.length s) in
  Uutf.String.fold_utf_8
    (fun () _ -> function
       | `Malformed _ -> Buffer.add_string b s
       | `Uchar u when p u && !start -> ()
       | `Uchar u when !start ->
         start := false;
         Uutf.Buffer.add_utf_8 b u
       | `Uchar u -> Uutf.Buffer.add_utf_8 b u)
    ()
    s;
  Buffer.contents b
;;

let underscore = Uchar.of_char '_'
let hyphen = Uchar.of_char '-'
let period = Uchar.of_char '.'
let is_white_space = Uucp.White.is_white_space
let is_alphabetic = Uucp.Alpha.is_alphabetic
let is_hex_digit = Uucp.Num.is_hex_digit

module Identifiers = struct
  module SMap = Map.Make (String)

  (* type t = int SMap.t *)

  let empty = SMap.empty

  let count s t =
    match SMap.find_opt s t with
    | None -> 0
    | Some x -> x
  ;;

  let incr s t = SMap.add s (count s t + 1) t

  let touch s t =
    let count = count s t in
    count, incr s t
  ;;
end

let slugify s =
  let s = trim_start_while (fun c -> not (is_alphabetic c)) s in
  let length = String.length s in
  let b = Buffer.create length in
  let last_is_ws = ref false in
  let add_to_buffer u =
    if !last_is_ws = true
    then (
      Uutf.Buffer.add_utf_8 b (Uchar.of_char '-');
      last_is_ws := false);
    Uutf.Buffer.add_utf_8 b u
  in
  let fold () _ = function
    | `Malformed _ -> add_to_buffer Uutf.u_rep
    | `Uchar u when is_white_space u && not !last_is_ws -> last_is_ws := true
    | `Uchar u when is_white_space u && !last_is_ws -> ()
    | `Uchar u ->
      if is_alphabetic u || is_hex_digit u
      then (
        match Uucp.Case.Map.to_lower u with
        | `Self -> add_to_buffer u
        | `Uchars us -> List.iter add_to_buffer us);
      if u = underscore || u = hyphen || u = period then add_to_buffer u
  in
  Uutf.String.fold_utf_8 fold () s;
  Buffer.contents b
;;

(* --- End Slugify Logic --- *)

(* Helper to convert inline AST to plain text (for alt tags, etc.) *)
let to_plain_text t =
  let buf = Buffer.create 1024 in
  let rec go : 'attr Ast.Impl.inline -> unit = function
    (* Use fully qualified type *)
    | Concat (_, l) -> List.iter go l
    | Text (_, t) | Code (_, t) -> Buffer.add_string buf t
    | Emph (_, i)
    | Strong (_, i)
    | Link (_, { label = i; _ })
    | Image (_, { label = i; _ }) -> go i
    | Hard_break _ | Soft_break _ -> Buffer.add_char buf ' '
    | Html (_, s) ->
      Buffer.add_string buf s (* Include raw HTML content in plain text? Optional. *)
  in
  go t;
  Buffer.contents buf
;;

let nl = Raw "\n" (* Represents a newline in the HTML output *)

(* --- Inline AST to Intermediate HTML ('t') Conversion --- *)

(* Convert Link AST to <a> element *)
let rec url label destination title attrs =
  let attrs =
    match title with
    | None -> attrs
    | Some t -> ("title", t) :: attrs
  in
  let attrs = ("href", escape_uri destination) :: attrs in
  elt Inline "a" attrs (Some (inline label))

(* Convert Image AST to <img> element *)
and img label destination title attrs =
  let attrs =
    match title with
    | None -> attrs
    | Some t -> ("title", t) :: attrs
  in
  let attrs = ("src", escape_uri destination) :: ("alt", to_plain_text label) :: attrs in
  elt Inline "img" attrs None (* Images are typically self-closing *)

(* Main inline conversion function *)
and inline : attributes inline -> t = function
  (* Provide the 'attributes' type argument *)
  | Concat (_attr, l) ->
    (* attr is now explicitly 'attributes' type *)
    (* If you want to include attributes on Concat spans (though often ignored): *)
    (* elt Inline "span" attr (Some (concat_map inline l)) *)
    (* Or ignore them: *)
    concat_map inline l
  | Text (_attr, t) ->
    (* Decide how to handle attributes on Text. Usually ignored, but could wrap in span *)
    (* elt Inline "span" attr (Some (text t)) *)
    text t
  | Emph (attr, il) -> elt Inline "em" attr (Some (inline il))
  | Strong (attr, il) -> elt Inline "strong" attr (Some (inline il))
  | Code (attr, s) -> elt Inline "code" attr (Some (text s))
  | Hard_break attr -> concat (elt Inline "br" attr None) nl
  | Soft_break _attr ->
    (* attr is likely ignored here too *)
    nl
  | Html (_attr, body) ->
    (* attr is likely ignored here *)
    raw body
  | Link (attr, { label; destination; title }) -> url label destination title attr
  | Image (attr, { label; destination; title }) -> img label destination title attr
;;

(* --- Block AST to Intermediate HTML ('t') Conversion --- *)

(* Helper for table cell alignment *)
let alignment_attributes = function
  | Default -> []
  | Left -> [ "align", "left" ]
  | Right -> [ "align", "right" ]
  | Centre -> [ "align", "center" ]
;;

(* Convert table header AST to <thead>...</thead> *)
let table_header headers =
  elt
    Table
    "thead"
    []
    (Some
       (elt
          Table
          "tr"
          []
          (Some
             (concat_map
                (fun (header, alignment) ->
                   let attrs = alignment_attributes alignment in
                   elt Block "th" attrs (Some (inline header))
                   (* Header cells are block-like *))
                headers))))
;;

(* Convert table rows AST to <tbody>...</tbody> *)
let table_body headers rows =
  elt
    Table
    "tbody"
    []
    (Some
       (concat_map
          (fun row ->
             elt
               Table
               "tr"
               []
               (Some
                  (concat_map2
                     (fun (_, alignment) cell ->
                        let attrs = alignment_attributes alignment in
                        elt Block "td" attrs (Some (inline cell))
                        (* Data cells are block-like *))
                     headers
                     row)))
          rows))
;;

(* Main block conversion function *)
let rec block ~auto_identifiers : attributes block -> t = function
  (* Use fully qualified type *)
  | Blockquote (attr, q) ->
    elt
      Block
      "blockquote"
      attr
      (Some (concat nl (concat_map (block ~auto_identifiers) q)))
  | Paragraph (attr, md) ->
    (* md is 'attributes inline' *)
    elt Block "p" attr (Some (inline md))
  | List (attr, ty, sp, bl) ->
    let name =
      match ty with
      | Ordered _ -> "ol"
      | Bullet _ -> "ul"
    in
    let attr =
      match ty with
      | Ordered (n, _) when n <> 1 -> ("start", string_of_int n) :: attr
      | _ -> attr
    in
    let li t =
      (* Handle tight vs loose lists *)
      let block' item =
        match item, sp with
        | Paragraph (_, content), Tight ->
          concat (inline content) nl (* No <p> in tight lists *)
        | _ -> block ~auto_identifiers item
      in
      let content = concat_map block' t in
      let nl_before = if sp = Tight then Null else nl in
      elt Block "li" [] (Some (concat nl_before content))
    in
    elt Block name attr (Some (concat nl (concat_map li bl)))
  | Code_block (attr, label, code) ->
    let code_attr =
      if String.trim label = "" then [] else [ "class", "language-" ^ label ]
    in
    let c = text code in
    (* Code is already plain text *)
    elt Block "pre" attr (Some (elt Inline "code" code_attr (Some c)))
  | Thematic_break attr -> elt Block "hr" attr None (* <hr> is self-closing *)
  | Html_block (attr, tag, nested_blocks) ->
    (* nested_blocks is 'attributes block list' *)
    let content = concat_map (block ~auto_identifiers) nested_blocks in
    elt Block tag attr (Some (concat nl content))
  | Heading (attr, level, text) ->
    (* text is 'attributes inline' *)
    let name = "h" ^ string_of_int (max 1 (min 6 level)) in
    elt Block name attr (Some (inline text))
  | Definition_list (attr, l) ->
    let f { term; defs } =
      concat
        (elt Block "dt" [] (Some (inline term)))
        (concat_map (fun s -> elt Block "dd" [] (Some (inline s))) defs)
    in
    elt Block "dl" attr (Some (concat nl (concat_map f l)))
  | Table (attr, headers, rows) ->
    let header_html = table_header headers in
    let body_html = if rows = [] then Null else table_body headers rows in
    elt Table "table" attr (Some (concat header_html body_html))
;;

(* Convert the entire document AST to the intermediate HTML type 't' *)
let of_doc ?(auto_identifiers = true) doc =
  let identifiers = ref Identifiers.empty in
  (* Use ref for mutable map *)
  let process_block b =
    match b with
    | Heading (attr, level, text) ->
      let attr', ids' =
        if (not auto_identifiers) || List.mem_assoc "id" attr
        then attr, !identifiers
        else (
          let plain_text = to_plain_text text in
          let id_base = if plain_text = "" then "section" else slugify plain_text in
          let count, ids_updated = Identifiers.touch id_base !identifiers in
          let id = if count = 0 then id_base else Printf.sprintf "%s-%i" id_base count in
          ("id", id) :: attr, ids_updated)
      in
      identifiers := ids';
      (* Update the ref *)
      block ~auto_identifiers (Heading (attr', level, text))
    | other -> block ~auto_identifiers other
  in
  concat_map process_block doc
;;

(* Convert the intermediate HTML type 't' to a string *)
let to_string t =
  let buf = Buffer.create 1024 in
  add_to_buffer buf t;
  Buffer.contents buf
;;
