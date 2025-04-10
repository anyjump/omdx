open Ast.Impl

type 'attr link_def =
  { label : string
  ; destination : string
  ; title : string option
  ; attributes : 'attr
  }

let is_whitespace = function
  | ' ' | '\t' | '\010' .. '\013' -> true
  | _ -> false
;;

let is_punct = function
  | '!'
  | '"'
  | '#'
  | '$'
  | '%'
  | '&'
  | '\''
  | '('
  | ')'
  | '*'
  | '+'
  | ','
  | '-'
  | '.'
  | '/'
  | ':'
  | ';'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '['
  | '\\'
  | ']'
  | '^'
  | '_'
  | '`'
  | '{'
  | '|'
  | '}'
  | '~' -> true
  | _ -> false
;;

(** Raised when a parser fails, used for control flow rather than error
    handling *)
exception Fail

(* Stateful parser combinators (Module P remains unchanged) *)
module P : sig
  type state
  type 'a t = state -> 'a

  val of_string : string -> state
  val peek : char option t
  val peek_exn : char t
  val peek_before : char -> state -> char
  val peek_after : char -> state -> char
  val pos : state -> int
  val range : state -> int -> int -> string
  val set_pos : state -> int -> unit
  val junk : unit t
  val char : char -> unit t
  val next : char t
  val ws : unit t
  val sp : unit t
  val ws1 : unit t
  val ( ||| ) : 'a t -> 'a t -> 'a t
  val ( >>> ) : unit t -> 'a t -> 'a t
  val ( <<< ) : 'a t -> unit t -> 'a t
  val protect : 'a t -> 'a t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val on_sub : (Str_slice.t -> 'a * Str_slice.t) -> 'a t
end = struct
  type state =
    { str : string
    ; mutable pos : int
    }

  let of_string str = { str; pos = 0 }

  type 'a t = state -> 'a

  let char c st =
    if st.pos >= String.length st.str
    then raise Fail
    else if st.str.[st.pos] <> c
    then raise Fail
    else st.pos <- st.pos + 1
  ;;

  let next st =
    if st.pos >= String.length st.str
    then raise Fail
    else (
      let c = st.str.[st.pos] in
      st.pos <- st.pos + 1;
      c)
  ;;

  let peek st = if st.pos >= String.length st.str then None else Some st.str.[st.pos]

  let peek_exn st =
    match peek st with
    | Some c -> c
    | None -> raise Fail
  ;;

  let peek_before c st = if st.pos = 0 then c else st.str.[st.pos - 1]

  let peek_after c st =
    if st.pos + 1 >= String.length st.str then c else st.str.[st.pos + 1]
  ;;

  let pos st = st.pos
  let range st pos n = String.sub st.str pos n
  let set_pos st pos = st.pos <- pos
  let junk st = if st.pos < String.length st.str then st.pos <- st.pos + 1

  let protect p st =
    let off = pos st in
    try p st with
    | e ->
      set_pos st off;
      raise e
  ;;

  let ( ||| ) p1 p2 st =
    try protect p1 st with
    | Fail -> p2 st
  ;;

  let ws st =
    let rec loop () =
      match peek st with
      | Some c when is_whitespace c ->
        junk st;
        loop ()
      | _ -> ()
      | exception Fail -> () (* End of input is also fine *)
    in
    loop ()
  ;;

  let sp st =
    let rec loop () =
      match peek st with
      | Some (' ' | '\t') ->
        junk st;
        loop ()
      | _ -> ()
      | exception Fail -> () (* End of input is also fine *)
    in
    loop ()
  ;;

  let ws1 st =
    match peek st with
    | Some c when is_whitespace c ->
      junk st;
      ws st
    | _ -> raise Fail
    | exception Fail -> raise Fail
  ;;

  let ( >>> ) p q st =
    p st;
    q st
  ;;

  let ( <<< ) p q st =
    let x = p st in
    q st;
    x
  ;;

  let pair p q st =
    let x = p st in
    let y = q st in
    x, y
  ;;

  let on_sub fn st =
    let result, s = fn (Str_slice.of_string ~off:st.pos st.str) in
    st.pos <- Str_slice.get_offset s;
    result
  ;;
end

(* Removed html_content_kind as we are simplifying *)

type code_block_kind =
  | Tilde
  | Backtick

(* Updated line type definitions *)
type t =
  | Lempty
  | Lblockquote of Str_slice.t
  | Lthematic_break
  | Latx_heading of int * string * attributes
  | Lsetext_heading of
      { level : int
      ; len : int
      } (** the level of the heading and how long the underline marker is *)
  | Lfenced_code of int * int * code_block_kind * (string * string) * attributes
  | Lindented_code of Str_slice.t
  | Lhtml_start of string * attributes (* tag_name, attrs *)
  | Lhtml_end of string (* tag_name *)
  | Llist_item of list_type * int * Str_slice.t
  | Lparagraph
  | Ldef_list of string
  | Ltable_line of Str_slice.t list

(* --- Slice-based helpers --- *)

let sp3 s =
  match Str_slice.take 3 s with
  | [ ' '; ' '; ' ' ] -> 3, Str_slice.drop 3 s
  | ' ' :: ' ' :: _ -> 2, Str_slice.drop 2 s
  | ' ' :: _ -> 1, Str_slice.drop 1 s
  | _ -> 0, s
;;

let ( ||| ) p1 p2 s =
  try p1 s with
  | Fail -> p2 s
;;

let trim_leading_ws s = Str_slice.drop_while is_whitespace s
let trim_trailing_ws s = Str_slice.drop_last_while is_whitespace s
let trim_ws s = trim_leading_ws s |> trim_trailing_ws
let is_empty s = Str_slice.is_empty (trim_leading_ws s)

(* --- Standard Markdown Block Parsers (thematic_break, setext_heading, etc.) --- *)
(* These remain largely unchanged from your original file *)

let thematic_break =
  let f symb c count =
    if Char.equal symb c
    then succ count
    else if is_whitespace c
    then count
    else raise Fail
  in
  fun s ->
    match Str_slice.head s with
    | Some (('*' | '_' | '-') as symb) ->
      if Str_slice.fold_left (f symb) 0 s >= 3 then Lthematic_break else raise Fail
    | Some _ | None -> raise Fail
;;

let setext_heading s =
  let level, symb =
    match Str_slice.head s with
    | Some '=' -> 1, '='
    | Some '-' -> 2, '-'
    | _ -> raise Fail
  in
  let heading_chars, rest = Str_slice.split_at (fun c -> not (Char.equal c symb)) s in
  let len = Str_slice.length heading_chars in
  if Char.equal symb '-' && len = 1
  then raise Fail (* potential list item *)
  else if not (Str_slice.for_all is_whitespace rest)
  then raise Fail
  else Lsetext_heading { level; len }
;;

(* Pandoc attribute parsing - keep as is for now, might be used by other parts *)
let parse_attributes s =
  let attributes = String.split_on_char ' ' s in
  let f (id, classes, acc) s =
    if s = ""
    then id, classes, acc
    else (
      match s.[0] with
      | '#' -> Some (String.sub s 1 (String.length s - 1)), classes, acc
      | '.' -> id, String.sub s 1 (String.length s - 1) :: classes, acc
      | _ ->
        let attr = String.split_on_char '=' s in
        (match attr with
         | [] -> id, classes, acc
         | h :: t -> id, classes, (h, String.concat "=" t) :: acc))
  in
  let id, classes, acc = List.fold_left f (None, [], []) attributes in
  let acc = List.rev acc in
  let acc =
    match classes with
    | [] -> acc
    | _ :: _ -> ("class", String.concat " " (List.rev classes)) :: acc
  in
  match id with
  | Some id -> ("id", id) :: acc
  | None -> acc
;;

let attribute_string s =
  let buf = Buffer.create 64 in
  let rec loop s =
    match Str_slice.head s with
    | None -> Str_slice.of_string (Buffer.contents buf), None
    | Some ('\\' as c) ->
      let s' = Str_slice.tail s in
      (match Str_slice.head s' with
       | Some c when is_punct c ->
         Buffer.add_char buf c;
         loop (Str_slice.tail s')
       | _ ->
         Buffer.add_char buf c;
         loop s')
    | Some '{' ->
      let buf' = Buffer.create 64 in
      let rec loop' s =
        match Str_slice.head s with
        | Some '}' ->
          let s' = Str_slice.tail s in
          (match Str_slice.head s' with
           | None ->
             Str_slice.of_string (Buffer.contents buf), Some (Buffer.contents buf')
           | Some _ ->
             Buffer.add_char buf '{';
             Buffer.add_buffer buf buf';
             Buffer.add_char buf '}';
             loop s')
        | None ->
          Buffer.add_char buf '{';
          Buffer.add_buffer buf buf';
          Str_slice.of_string (Buffer.contents buf), None
        | Some '{' ->
          Buffer.add_char buf '{';
          Buffer.add_buffer buf buf';
          Buffer.reset buf';
          loop' (Str_slice.tail s)
        | Some c ->
          Buffer.add_char buf' c;
          loop' (Str_slice.tail s)
      in
      loop' (Str_slice.tail s)
    | Some c ->
      Buffer.add_char buf c;
      loop (Str_slice.tail s)
  in
  let s', a = loop (trim_leading_ws s) in
  let attrs = Option.map parse_attributes a |> Option.value ~default:[] in
  s', attrs
;;

let atx_heading s =
  let rec loop n s =
    if n > 6 then raise Fail;
    match Str_slice.head s with
    | Some '#' -> loop (succ n) (Str_slice.tail s)
    | Some w when is_whitespace w ->
      let s, a =
        match Str_slice.last s with
        | Some '}' -> attribute_string s
        | _ -> s, []
      in
      let s = trim_ws s in
      let rec loop_trim_hash t =
        match Str_slice.last t with
        | Some '#' -> loop_trim_hash (Str_slice.drop_last t)
        | Some w when is_whitespace w -> trim_trailing_ws t
        | None -> trim_trailing_ws t
        | Some _ -> s
      in
      Latx_heading (n, Str_slice.to_string (trim_leading_ws (loop_trim_hash s)), a)
    | Some _ -> raise Fail
    | None -> Latx_heading (n, Str_slice.to_string s, [])
  in
  loop 0 s
;;

(* Entity parsing remains unchanged *)
let entity s =
  match Str_slice.take 2 s with
  | '#' :: ('x' | 'X') :: _ ->
    let rec loop m n s =
      if m > 6 then raise Fail;
      match Str_slice.head s with
      | Some ('a' .. 'f' as c) ->
        loop (succ m) ((n * 16) + Char.code c - Char.code 'a' + 10) (Str_slice.tail s)
      | Some ('A' .. 'F' as c) ->
        loop (succ m) ((n * 16) + Char.code c - Char.code 'A' + 10) (Str_slice.tail s)
      | Some ('0' .. '9' as c) ->
        loop (succ m) ((n * 16) + Char.code c - Char.code '0') (Str_slice.tail s)
      | Some ';' ->
        if m = 0 then raise Fail;
        let u = if n = 0 || not (Uchar.is_valid n) then Uchar.rep else Uchar.of_int n in
        [ u ], Str_slice.tail s
      | Some _ | None -> raise Fail
    in
    loop 0 0 (Str_slice.drop 2 s)
  | '#' :: _ ->
    let rec loop m n s =
      if m > 7 then raise Fail;
      match Str_slice.head s with
      | Some ('0' .. '9' as c) ->
        loop (succ m) ((n * 10) + Char.code c - Char.code '0') (Str_slice.tail s)
      | Some ';' ->
        if m = 0 then raise Fail;
        let u = if n = 0 || not (Uchar.is_valid n) then Uchar.rep else Uchar.of_int n in
        [ u ], Str_slice.tail s
      | Some _ | None -> raise Fail
    in
    loop 0 0 (Str_slice.tail s)
  | ('a' .. 'z' | 'A' .. 'Z') :: _ ->
    let rec loop len t =
      match Str_slice.head t with
      | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') -> loop (succ len) (Str_slice.tail t)
      | Some ';' ->
        let name = Str_slice.to_string (Str_slice.sub ~len s) in
        (match Entities.lookup name with
         | [] -> raise Fail
         | cps -> cps, Str_slice.tail t)
      | Some _ | None -> raise Fail
    in
    loop 1 (Str_slice.tail s)
  | _ -> raise Fail
;;

(* Fenced code block parsing remains unchanged *)
let info_string c s =
  let buf = Buffer.create 17 in
  let s, a =
    match Str_slice.last s with
    | Some '}' -> attribute_string s
    | _ -> s, []
  in
  let s = trim_ws s in
  let rec loop s =
    match Str_slice.head s with
    | Some (' ' | '\t' | '\010' .. '\013') | None ->
      if c = '`' && Str_slice.exists (( = ) '`') s then raise Fail;
      (Buffer.contents buf, Str_slice.to_string (trim_leading_ws s)), a
    | Some '`' when c = '`' -> raise Fail
    | Some ('\\' as c) ->
      let s' = Str_slice.tail s in
      (match Str_slice.head s' with
       | Some c when is_punct c ->
         Buffer.add_char buf c;
         loop (Str_slice.tail s')
       | _ ->
         Buffer.add_char buf c;
         loop s')
    | Some ('&' as c) ->
      let s' = Str_slice.tail s in
      (match entity s' with
       | ul, s'' ->
         List.iter (Buffer.add_utf_8_uchar buf) ul;
         loop s''
       | exception Fail ->
         Buffer.add_char buf c;
         loop s')
    | Some c ->
      Buffer.add_char buf c;
      loop (Str_slice.tail s)
  in
  loop (trim_leading_ws s)
;;

let fenced_code ind s =
  match Str_slice.head s with
  | Some (('`' | '~') as c) ->
    let rec loop n s =
      match Str_slice.head s with
      | Some c1 when c = c1 -> loop (succ n) (Str_slice.tail s)
      | Some _ | None ->
        if n < 3 then raise Fail;
        let s, a = info_string c s in
        let c = if c = '`' then Backtick else Tilde in
        Lfenced_code (ind, n, c, s, a)
    in
    loop 1 (Str_slice.tail s)
  | Some _ | None -> raise Fail
;;

(* Indent, List items, etc. remain unchanged *)
let indent s =
  let rec loop n s =
    match Str_slice.head s with
    | Some ' ' -> loop (n + 1) (Str_slice.tail s)
    | Some '\t' -> loop (n + 4) (Str_slice.tail s) (* Tabs count as 4 spaces *)
    | Some _ | None -> n
  in
  loop 0 s
;;

let unordered_list_item ind s =
  match Str_slice.head s with
  | Some (('+' | '-' | '*') as c) ->
    let s' = Str_slice.tail s in
    if is_empty s'
    then Llist_item (Bullet c, 1 + ind + 1, s') (* Marker + 1 space *)
    else (
      let n = indent s' in
      if n = 0 then raise Fail;
      (* Must have space after marker *)
      let indent_after_marker = if n <= 4 then n else 1 in
      (* Max 4 spaces, otherwise 1 *)
      Llist_item (Bullet c, 1 + ind + indent_after_marker, Str_slice.offset n s'))
  | Some _ | None -> raise Fail
;;

let ordered_list_item ind s =
  let rec loop n m s =
    match Str_slice.head s with
    | Some ('0' .. '9' as c) ->
      if n >= 9 then raise Fail;
      (* Max 9 digits *)
      loop (succ n) ((m * 10) + Char.code c - Char.code '0') (Str_slice.tail s)
    | Some (('.' | ')') as c) ->
      let marker_len = n + 1 in
      let s' = Str_slice.tail s in
      if is_empty s'
      then Llist_item (Ordered (m, c), marker_len + ind + 1, s')
      else (
        let content_indent = indent s' in
        if content_indent = 0 then raise Fail;
        (* Must have space after marker *)
        let indent_after_marker = if content_indent <= 4 then content_indent else 1 in
        Llist_item
          ( Ordered (m, c)
          , marker_len + ind + indent_after_marker
          , Str_slice.offset content_indent s' ))
    | Some _ | None -> raise Fail
  in
  loop 0 0 s
;;

(* --- NEW/MODIFIED HTML Parsing Logic --- *)

(* Helper: Parse HTML tag name *)
let html_tag_name s0 =
  match Str_slice.head s0 with
  | Some ('a' .. 'z' | 'A' .. 'Z') ->
    let rec loop len s =
      match Str_slice.head s with
      | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-') ->
        loop (succ len) (Str_slice.tail s)
      | Some _ | None -> Str_slice.to_string (Str_slice.sub s0 ~len), s
    in
    loop 1 (Str_slice.tail s0)
  | Some _ | None -> raise Fail
;;

(* List of tags that expect raw content (simplified list) *)
let raw_html_tags = [ "script"; "style"; "pre"; "textarea" ]

(* Helper: Check if a tag is a raw content tag *)
let is_raw_tag s = List.mem (String.lowercase_ascii s) raw_html_tags

(* Helper: Skip whitespace on a slice *)
let html_ws s = trim_leading_ws s

(* Helper: Parse HTML attribute name *)
let html_attr_name s0 =
  match Str_slice.head s0 with
  | Some ('a' .. 'z' | 'A' .. 'Z' | '_' | ':') ->
    let rec loop len s =
      match Str_slice.head s with
      | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | ':' | '-') ->
        loop (succ len) (Str_slice.tail s)
      | _ -> Str_slice.to_string (Str_slice.sub s0 ~len), s
    in
    loop 1 (Str_slice.tail s0)
  | _ -> raise Fail
;;

(* Helper: Parse HTML attribute value *)
let html_attr_value s0 =
  let s = html_ws s0 in
  match Str_slice.head s with
  | Some ('"' as quote) | Some ('\'' as quote) ->
    let s = Str_slice.tail s in
    let buf = Buffer.create (Str_slice.length s) in
    let rec loop s =
      match Str_slice.head s with
      | Some c when c = quote -> Buffer.contents buf, Str_slice.tail s
      | Some c ->
        Buffer.add_char buf c;
        loop (Str_slice.tail s)
      | None -> raise Fail (* Unclosed quote *)
    in
    loop s
  | Some _ ->
    (* Unquoted value *)
    let rec loop len s =
      match Str_slice.head s with
      | Some (' ' | '\t' | '\n' | '\r' | '>' | '/') | None ->
        if len = 0 then raise Fail;
        (* Need at least one char *)
        Str_slice.to_string (Str_slice.sub s0 ~len), s
      | Some _ -> loop (succ len) (Str_slice.tail s)
    in
    (* Adjust s0 to start at the actual value, not after whitespace *)
    loop 0 s
  | None -> raise Fail (* Value expected *)
;;

(* Helper: Parse a single HTML attribute (name[=value]) *)
let html_attribute s0 =
  let name, s1 = html_attr_name s0 in
  let s2 = html_ws s1 in
  match Str_slice.head s2 with
  | Some '=' ->
    let s3 = Str_slice.tail s2 in
    let value, s4 = html_attr_value s3 in
    (name, value), s4
  | _ -> (name, ""), s1 (* Attribute without value, treat as empty string value *)
;;

(* Helper: Parse multiple HTML attributes *)
let html_attributes s0 =
  let rec loop s acc =
    let s' = html_ws s in
    match Str_slice.head s' with
    | Some '>' | Some '/' | None -> List.rev acc, s' (* End of attributes *)
    | Some _ ->
      (* Try parsing an attribute *)
      (try
         let attr, s_next = html_attribute s' in
         loop s_next (attr :: acc)
       with
       | Fail -> List.rev acc, s')
    (* Stop if attribute parsing fails *)
  in
  loop s0 []
;;

(* Main function to classify HTML block lines *)
let classify_html_block_line s =
  match Str_slice.head s with
  | Some '<' ->
    let s_after_lt = Str_slice.tail s in
    (match Str_slice.head s_after_lt with
     | Some '/' ->
       (* Potential closing tag *)
       let s_after_slash = Str_slice.tail s_after_lt in
       (try
          let tag, s_after_tag = html_tag_name s_after_slash in
          let s_after_ws = html_ws s_after_tag in
          match Str_slice.head s_after_ws with
          | Some '>' ->
            let s_after_gt = Str_slice.tail s_after_ws in
            if is_empty s_after_gt
            then Lhtml_end tag
            else raise Fail (* Junk after closing tag *)
          | _ -> raise Fail (* Expected '>' *)
        with
        | Fail -> raise Fail)
       (* Malformed closing tag *)
     | Some ('!' | '?') -> raise Fail (* Ignore comments, PIs, CDATA for now *)
     | Some _ ->
       (* Potential opening tag *)
       (try
          let tag, s_after_tag = html_tag_name s_after_lt in
          if is_raw_tag tag then raise Fail;
          (* Ignore raw tags for now *)
          let attrs, s_after_attrs = html_attributes s_after_tag in
          let s_after_ws = html_ws s_after_attrs in
          let self_closing =
            match Str_slice.head s_after_ws with
            | Some '/' -> true
            | _ -> false
          in
          let s_after_slash =
            if self_closing then Str_slice.tail s_after_ws else s_after_ws
          in
          match Str_slice.head s_after_slash with
          | Some '>' ->
            let s_after_gt = Str_slice.tail s_after_slash in
            if is_empty s_after_gt
            then
              (* For now, treat self-closing like regular opening tags,
                       block parser needs to handle immediate closing if needed *)
              Lhtml_start (tag, attrs)
            else raise Fail (* Junk after opening tag *)
          | _ -> raise Fail (* Expected '>' *)
        with
        | Fail -> raise Fail)
       (* Malformed opening tag *)
     | None -> raise Fail (* Just '<' *))
  | _ -> raise Fail (* Doesn't start with '<' *)
;;

(* Other block parsers remain unchanged *)
let blank s =
  if not (is_empty s) then raise Fail;
  Lempty
;;

let def_list s =
  let s' = Str_slice.tail s in
  match Str_slice.head s' with
  | Some w when is_whitespace w -> Ldef_list (String.trim (Str_slice.to_string s'))
  | _ -> raise Fail
;;

let indented_code ind s =
  if indent s + ind < 4 then raise Fail;
  Lindented_code (Str_slice.offset (4 - ind) s)
;;

let table_row ~pipe_prefix s =
  let rec loop items seen_pipe s =
    match Str_slice.index_unescaped '|' s with
    | None ->
      if Str_slice.for_all is_whitespace s then items, seen_pipe else s :: items, false
    | Some i ->
      let item = Str_slice.take_prefix i s in
      loop (item :: items) true (Str_slice.drop (i + 1) s)
  in
  let items, terminating_pipe = loop [] pipe_prefix s in
  match pipe_prefix, items, terminating_pipe with
  | true, _, _ | _, _ :: _, true | _, _ :: _ :: _, _ ->
    Ltable_line (List.rev_map Str_slice.trim items)
  | _ -> raise Fail
;;

(* Updated main parse function *)
let parse s0 =
  let ind, s = sp3 s0 in
  match Str_slice.head s with
  | Some '>' ->
    let s' = Str_slice.offset 1 s in
    let s'' = if indent s' > 0 then Str_slice.offset 1 s' else s' in
    Lblockquote s''
  | Some '=' -> (setext_heading ||| table_row ~pipe_prefix:false) s
  | Some '-' ->
    (setext_heading
     ||| thematic_break
     ||| unordered_list_item ind
     ||| table_row ~pipe_prefix:false)
      s
  | Some '_' -> thematic_break s
  | Some '#' -> atx_heading s
  | Some ('~' | '`') -> fenced_code ind s
  | Some '<' -> classify_html_block_line s (* Use the new function *)
  | Some '*' -> (thematic_break ||| unordered_list_item ind) s
  | Some '+' -> unordered_list_item ind s
  | Some '0' .. '9' -> (ordered_list_item ind ||| table_row ~pipe_prefix:false) s
  | Some ':' -> (def_list ||| table_row ~pipe_prefix:false) s
  | Some '|' -> table_row ~pipe_prefix:true (Str_slice.tail s)
  | Some _ -> (blank ||| indented_code ind ||| table_row ~pipe_prefix:false) s
  | None -> Lempty
;;

(* Fallback to Lparagraph *)
let parse s =
  try parse s with
  | Fail -> Lparagraph
;;

(* --- Inline Parsing Logic (Module Pre, inline, link_reference_definition, etc.) --- *)
(* These remain unchanged from your original file *)
open P

let is_empty st =
  let off = pos st in
  try
    let rec loop () =
      match next st with
      | c when is_whitespace c -> loop ()
      | _ ->
        set_pos st off;
        false
    in
    loop ()
  with
  | Fail ->
    set_pos st off;
    true
;;

let inline_attribute_string s =
  let ppos = pos s in
  ws s;
  let a =
    match peek s with
    | Some '{' ->
      let buf = Buffer.create 64 in
      let rec loop s pos =
        match peek s with
        | Some '}' ->
          junk s;
          Some (Buffer.contents buf)
        | None | Some '{' ->
          set_pos s pos;
          None
        | Some c ->
          Buffer.add_char buf c;
          junk s;
          loop s pos
      in
      junk s;
      loop s (pos s)
    | _ -> None
  in
  let attr = Option.map parse_attributes a |> Option.value ~default:[] in
  if attr = [] then set_pos s ppos;
  attr
;;

let entity buf st =
  junk st;
  match on_sub entity st with
  | cs -> List.iter (Buffer.add_utf_8_uchar buf) cs
  | exception Fail -> Buffer.add_char buf '&'
;;

module Pre = struct
  type delim =
    | Ws
    | Punct
    | Other

  type emph_style =
    | Star
    | Underscore

  type link_kind =
    | Img
    | Url

  type t =
    | Bang_left_bracket
    | Left_bracket of link_kind
    | Emph of delim * delim * emph_style * int
    | R of attributes inline

  let concat = function
    | [ x ] -> x
    | l -> Concat ([], l)
  ;;

  let left_flanking = function
    | Emph (_, Other, _, _) | Emph ((Ws | Punct), Punct, _, _) -> true
    | _ -> false
  ;;

  let right_flanking = function
    | Emph (Other, _, _, _) | Emph (Punct, (Ws | Punct), _, _) -> true
    | _ -> false
  ;;

  let is_opener = function
    | Emph (pre, _, Underscore, _) as x ->
      left_flanking x && ((not (right_flanking x)) || pre = Punct)
    | Emph (_, _, Star, _) as x -> left_flanking x
    | _ -> false
  ;;

  let is_closer = function
    | Emph (_, post, Underscore, _) as x ->
      right_flanking x && ((not (left_flanking x)) || post = Punct)
    | Emph (_, _, Star, _) as x -> right_flanking x
    | _ -> false
  ;;

  let classify_delim = function
    | '!'
    | '"'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '('
    | ')'
    | '*'
    | '+'
    | ','
    | '-'
    | '.'
    | '/'
    | ':'
    | ';'
    | '<'
    | '='
    | '>'
    | '?'
    | '@'
    | '['
    | '\\'
    | ']'
    | '^'
    | '_'
    | '`'
    | '{'
    | '|'
    | '}'
    | '~' -> Punct
    | ' ' | '\t' | '\010' .. '\013' | '\160' -> Ws
    | _ -> Other
  ;;

  let to_r = function
    | Bang_left_bracket -> Text ([], "![")
    | Left_bracket Img -> Text ([], "![")
    | Left_bracket Url -> Text ([], "[")
    | Emph (_, _, Star, n) -> Text ([], String.make n '*')
    | Emph (_, _, Underscore, n) -> Text ([], String.make n '_')
    | R x -> x
  ;;

  let rec find_next_emph = function
    | Emph (pre, post, style, n) :: _ -> Some (pre, post, style, n)
    | _ :: xs -> find_next_emph xs
    | [] -> None
  ;;

  let rec find_next_closer_emph = function
    | (Emph (pre, post, style, n) as e) :: _ when is_closer e -> Some (pre, post, style, n)
    | _ :: xs -> find_next_closer_emph xs
    | [] -> None
  ;;

  let is_emph_match n1 n2 =
    if (n1 + n2) mod 3 = 0 && n1 mod 3 != 0 && n2 mod 3 != 0 then false else true
  ;;

  let rec parse_emph = function
    | (Emph (pre, _, q1, n1) as x1) :: xs when is_opener x1 ->
      let rec loop acc = function
        | (Emph (_, post, q2, n2) as x2) :: xs1 as xs when is_closer x2 && q1 = q2 ->
          if (is_opener x2 || is_closer x1) && not (is_emph_match n1 n2)
          then (
            match find_next_emph xs1 with
            | Some (_, _, _, n3) when is_emph_match n3 n2 -> loop acc (parse_emph xs)
            | _ -> loop (x2 :: acc) xs1)
          else (
            let xs' =
              if n1 >= 2 && n2 >= 2
              then if n2 > 2 then Emph (Other, post, q2, n2 - 2) :: xs1 else xs1
              else if n2 > 1
              then Emph (Punct, post, q2, n2 - 1) :: xs1
              else xs1
            in
            let il = concat (List.map to_r (List.rev acc)) in
            let r =
              (if n1 >= 2 && n2 >= 2 then R (Strong ([], il)) else R (Emph ([], il)))
              :: xs'
            in
            let r' =
              if n1 >= 2 && n2 >= 2
              then if n1 > 2 then Emph (pre, Other, q1, n1 - 2) :: r else r
              else if n1 > 1
              then Emph (pre, Punct, q1, n1 - 1) :: r
              else r
            in
            parse_emph r')
        | (Emph (_, _, q2, _) as x2) :: xs1 as xs when is_opener x2 ->
          let is_next_closer_same =
            match find_next_closer_emph xs1 with
            | None -> false
            | Some (_, _, q3, _) -> q2 = q3
          in
          if not is_next_closer_same
          then loop (x2 :: acc) xs1
          else loop acc (parse_emph xs)
        | x :: xs -> loop (x :: acc) xs
        | [] -> x1 :: List.rev acc
      in
      loop [] xs
    | x :: xs -> x :: parse_emph xs
    | [] -> []
  ;;

  let parse_emph xs = concat (List.map to_r (parse_emph xs))
end

let escape buf st =
  if next st <> '\\' then raise Fail;
  match peek st with
  | Some c when is_punct c ->
    junk st;
    Buffer.add_char buf c
  | _ -> Buffer.add_char buf '\\'
;;

let link_label allow_balanced_brackets st =
  if peek_exn st <> '[' then raise Fail;
  junk st;
  let buf = Buffer.create 17 in
  let rec loop n nonempty =
    match peek_exn st with
    | ']' when n = 0 ->
      junk st;
      if not nonempty then raise Fail;
      Buffer.contents buf
    | ']' as c ->
      assert (n > 0);
      junk st;
      Buffer.add_char buf c;
      loop (pred n) true
    | '\\' as c ->
      junk st;
      Buffer.add_char buf c;
      (match peek st with
       | Some c when is_punct c ->
         junk st;
         Buffer.add_char buf c
       | _ -> ());
      loop n true
    | '[' when not allow_balanced_brackets -> raise Fail
    | '[' as c ->
      junk st;
      Buffer.add_char buf c;
      loop (succ n) true
    | c when is_whitespace c ->
      junk st;
      Buffer.add_char buf c;
      loop n nonempty
    | c ->
      junk st;
      Buffer.add_char buf c;
      loop n true
  in
  loop 0 false
;;

type add_uchar_result =
  { start : bool
  ; seen_ws : bool
  }

let normalize s =
  let b = Buffer.create (String.length s * 2) in
  let to_nfd_and_utf_8 =
    let n = Uunf.create `NFD in
    let rec add v =
      match Uunf.add n v with
      | `Await | `End -> ()
      | `Uchar u ->
        Uutf.Buffer.add_utf_8 b u;
        add `Await
    in
    add
  in
  let add_nfd =
    let n = Uunf.create `NFD in
    let rec add v =
      match Uunf.add n v with
      | `Await | `End -> ()
      | `Uchar u ->
        (match Uucp.Case.Fold.fold u with
         | `Self -> to_nfd_and_utf_8 (`Uchar u)
         | `Uchars us -> List.iter (fun u -> to_nfd_and_utf_8 (`Uchar u)) us);
        add `Await
    in
    add
  in
  let uspace = `Uchar (Uchar.of_char ' ') in
  let add_uchar { start; seen_ws } _ = function
    | `Malformed _ ->
      add_nfd (`Uchar Uutf.u_rep);
      { start = false; seen_ws = false }
    | `Uchar u as uchar ->
      if Uucp.White.is_white_space u
      then { start; seen_ws = true }
      else (
        if (not start) && seen_ws then add_nfd uspace;
        add_nfd uchar;
        { start = false; seen_ws = false })
  in
  ignore (Uutf.String.fold_utf_8 add_uchar { start = true; seen_ws = false } s);
  add_nfd `End;
  to_nfd_and_utf_8 `End;
  Buffer.contents b
;;

let tag_name st =
  match peek_exn st with
  | 'a' .. 'z' | 'A' .. 'Z' ->
    junk st;
    let rec loop () =
      match peek st with
      | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-') ->
        junk st;
        loop ()
      | _ -> ()
    in
    loop ()
  | _ -> raise Fail
;;

let ws_buf buf st =
  let rec loop () =
    match peek st with
    | Some w when is_whitespace w ->
      Buffer.add_char buf w;
      junk st;
      loop ()
    | _ -> ()
  in
  loop ()
;;

let closing_tag st =
  let start = pos st in
  if next st <> '<' then raise Fail;
  if next st <> '/' then raise Fail;
  tag_name st;
  ws st;
  if next st <> '>' then raise Fail;
  range st start (pos st - start)
;;

let list p st =
  let rec loop () =
    match protect p st with
    | () -> loop ()
    | exception Fail -> ()
  in
  loop ()
;;

let single_quoted_attribute st =
  if next st <> '\'' then raise Fail;
  let rec loop () =
    match peek_exn st with
    | '\'' -> junk st
    | _ ->
      junk st;
      loop ()
  in
  loop ()
;;

let double_quoted_attribute st =
  if next st <> '"' then raise Fail;
  let rec loop () =
    match peek_exn st with
    | '"' -> junk st
    | _ ->
      junk st;
      loop ()
  in
  loop ()
;;

let unquoted_attribute st =
  let rec loop n =
    match peek_exn st with
    | ' ' | '\t' | '\010' .. '\013' | '"' | '\'' | '=' | '<' | '>' | '`' ->
      if n = 0 then raise Fail
    | _ ->
      junk st;
      loop (succ n)
  in
  loop 0
;;

let attribute_value st =
  match peek_exn st with
  | '\'' -> single_quoted_attribute st
  | '"' -> double_quoted_attribute st
  | _ -> unquoted_attribute st
;;

let attribute_name st =
  match peek_exn st with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | ':' ->
    junk st;
    let rec loop () =
      match peek st with
      | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | ':' | '-') ->
        junk st;
        loop ()
      | _ -> ()
    in
    loop ()
  | _ -> raise Fail
;;

let option d p st =
  match protect p st with
  | r -> r
  | exception Fail -> d
;;

let some p st = Some (p st)
let attribute_value_specification = ws >>> char '=' >>> ws >>> attribute_value

let ws1_buf buf st =
  match peek st with
  | Some w when is_whitespace w -> ws_buf buf st
  | _ -> raise Fail
;;

let attribute st =
  ws1 st;
  attribute_name st;
  option () attribute_value_specification st
;;

let open_tag st =
  let start = pos st in
  if next st <> '<' then raise Fail;
  tag_name st;
  list attribute st;
  ws st;
  (match peek st with
   | Some '/' -> junk st
   | _ -> ());
  if next st <> '>' then raise Fail;
  range st start (pos st - start)
;;

let html_comment st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  if next st <> '-' then raise Fail;
  if next st <> '-' then raise Fail;
  Buffer.add_string buf "<!--";
  let rec loop start =
    match peek_exn st with
    | '-' as c ->
      junk st;
      (match peek_exn st with
       | '-' ->
         junk st;
         if next st <> '>' then raise Fail;
         Buffer.add_string buf "-->";
         Buffer.contents buf
       | '>' when start -> raise Fail
       | _ ->
         Buffer.add_char buf c;
         loop false)
    | '>' when start -> raise Fail
    | '&' ->
      entity buf st;
      loop false
    | _ as c ->
      junk st;
      Buffer.add_char buf c;
      loop false
  in
  loop true
;;

let processing_instruction st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '?' then raise Fail;
  Buffer.add_string buf "<?";
  let rec loop () =
    match peek_exn st with
    | '?' as c ->
      junk st;
      (match peek_exn st with
       | '>' ->
         junk st;
         Buffer.add_string buf "?>";
         Buffer.contents buf
       | _ ->
         Buffer.add_char buf c;
         loop ())
    | '&' ->
      entity buf st;
      loop ()
    | _ as c ->
      junk st;
      Buffer.add_char buf c;
      loop ()
  in
  loop ()
;;

let cdata_section st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  if next st <> '[' then raise Fail;
  if next st <> 'C' then raise Fail;
  if next st <> 'D' then raise Fail;
  if next st <> 'A' then raise Fail;
  if next st <> 'T' then raise Fail;
  if next st <> 'A' then raise Fail;
  if next st <> '[' then raise Fail;
  Buffer.add_string buf "<![CDATA[";
  let rec loop () =
    match peek_exn st with
    | ']' as c ->
      junk st;
      (match peek_exn st with
       | ']' as c1 ->
         junk st;
         (match peek_exn st with
          | '>' ->
            junk st;
            Buffer.add_string buf "]]>";
            Buffer.contents buf
          | _ ->
            Buffer.add_char buf c;
            Buffer.add_char buf c1;
            loop ())
       | _ ->
         Buffer.add_char buf c;
         loop ())
    | '&' ->
      entity buf st;
      loop ()
    | _ as c ->
      junk st;
      Buffer.add_char buf c;
      loop ()
  in
  loop ()
;;

let declaration st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  Buffer.add_string buf "<!";
  match peek_exn st with
  | 'A' .. 'Z' ->
    let rec loop () =
      match peek_exn st with
      | 'A' .. 'Z' as c ->
        junk st;
        Buffer.add_char buf c;
        loop ()
      | w when is_whitespace w ->
        ws1_buf buf st;
        let rec loop () =
          match peek_exn st with
          | '>' as c ->
            junk st;
            Buffer.add_char buf c;
            Buffer.contents buf
          | '&' ->
            entity buf st;
            loop ()
          | _ as c ->
            junk st;
            Buffer.add_char buf c;
            loop ()
        in
        loop ()
      | _ -> raise Fail
    in
    loop ()
  | _ -> raise Fail
;;

let link_destination st =
  let buf = Buffer.create 17 in
  match peek_exn st with
  | '<' ->
    junk st;
    let rec loop () =
      match peek_exn st with
      | '>' ->
        junk st;
        Buffer.contents buf
      | '\010' .. '\013' | '<' -> raise Fail
      | '\\' ->
        escape buf st;
        loop ()
      | '&' ->
        entity buf st;
        loop ()
      | _ as c ->
        junk st;
        Buffer.add_char buf c;
        loop ()
    in
    loop ()
  | _ ->
    let rec loop n =
      match peek st with
      | Some ('(' as c) ->
        junk st;
        Buffer.add_char buf c;
        loop (succ n)
      | Some ')' when n = 0 ->
        if Buffer.length buf = 0 then raise Fail;
        Buffer.contents buf
      | Some (')' as c) ->
        junk st;
        Buffer.add_char buf c;
        loop (pred n)
      | Some '\\' ->
        escape buf st;
        loop n
      | Some '&' ->
        entity buf st;
        loop n
      | Some (' ' | '\t' | '\x00' .. '\x1F' | '\x7F') | None ->
        if n > 0 || Buffer.length buf = 0 then raise Fail;
        Buffer.contents buf
      | Some c ->
        junk st;
        Buffer.add_char buf c;
        loop n
    in
    loop 0
;;

let eol st =
  match peek st with
  | Some '\n' -> junk st
  | Some _ -> raise Fail
  | None -> ()
;;

let link_title st =
  let buf = Buffer.create 17 in
  match peek_exn st with
  | ('\'' | '"') as c ->
    junk st;
    let rec loop () =
      match peek_exn st with
      | '\\' ->
        escape buf st;
        loop ()
      | '&' ->
        entity buf st;
        loop ()
      | _ as c1 when c = c1 ->
        junk st;
        Buffer.contents buf
      | _ as c1 ->
        junk st;
        Buffer.add_char buf c1;
        loop ()
    in
    loop ()
  | '(' ->
    junk st;
    let rec loop () =
      match peek_exn st with
      | '\\' ->
        escape buf st;
        loop ()
      | '&' ->
        entity buf st;
        loop ()
      | ')' ->
        junk st;
        Buffer.contents buf
      | _ as c ->
        junk st;
        Buffer.add_char buf c;
        loop ()
    in
    loop ()
  | _ -> raise Fail
;;

let space st =
  match peek_exn st with
  | ' ' -> junk st
  | _ -> raise Fail
;;

let many p st =
  try
    while true do
      p st
    done
  with
  | Fail -> ()
;;

let scheme st =
  match peek_exn st with
  | 'a' .. 'z' | 'A' .. 'Z' ->
    let rec loop n =
      if n < 32
      then (
        match peek st with
        | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '.' | '-') ->
          junk st;
          loop (succ n)
        | _ -> n)
      else n
    in
    let n = loop 0 in
    if n < 2 then raise Fail
  | _ -> raise Fail
;;

let absolute_uri st =
  let p = pos st in
  scheme st;
  if next st <> ':' then raise Fail;
  let rec loop () =
    match peek st with
    | Some
        (' ' | '\t' | '\010' .. '\013' | '\x00' .. '\x1F' | '\x7F' .. '\x9F' | '<' | '>')
    | None ->
      let txt = range st p (pos st - p) in
      txt, txt
    | Some _ ->
      junk st;
      loop ()
  in
  loop ()
;;

let email_address st =
  let p = pos st in
  let rec loop n =
    match peek_exn st with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '.'
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '*'
    | '+'
    | '/'
    | '='
    | '?'
    | '^'
    | '_'
    | '`'
    | '{'
    | '|'
    | '}'
    | '~'
    | '-' ->
      junk st;
      loop (succ n)
    | '@' ->
      junk st;
      let label st =
        let let_dig st =
          match peek_exn st with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
            junk st;
            false
          | '-' ->
            junk st;
            true
          | _ -> raise Fail
        in
        if let_dig st then raise Fail;
        let rec loop last =
          match let_dig st with
          | r -> loop r
          | exception Fail -> if last then raise Fail
        in
        loop false
      in
      label st;
      list (char '.' >>> label) st;
      let txt = range st p (pos st - p) in
      txt, "mailto:" ^ txt
    | _ -> raise Fail
  in
  loop 0
;;

let autolink st =
  match peek_exn st with
  | '<' ->
    junk st;
    let label, destination = (absolute_uri ||| email_address) st in
    if next st <> '>' then raise Fail;
    { Ast.Impl.label = Text ([], label); destination; title = None }
  | _ -> raise Fail
;;

let inline_link =
  char '('
  >>> ws
  >>> option ("", None) (pair link_destination (option None (ws1 >>> some link_title)))
  <<< ws
  <<< char ')'
;;

let get_buf buf =
  let s = Buffer.contents buf in
  Buffer.clear buf;
  s
;;

let text buf acc =
  if Buffer.length buf = 0 then acc else Pre.R (Text ([], get_buf buf)) :: acc
;;

let inline_pre buf acc st =
  let p = pos st in
  let rec gobble_open_backtick n =
    match peek st with
    | Some '`' ->
      junk st;
      gobble_open_backtick (succ n)
    | Some _ ->
      let acc = text buf acc in
      let bufcode = Buffer.create 17 in
      let finish () =
        let content = Buffer.contents bufcode in
        let content =
          if String.for_all (( = ) ' ') content
          then content
          else if
            String.length content >= 2
            && content.[0] = ' '
            && content.[String.length content - 1] = ' '
          then String.sub content 1 (String.length content - 2)
          else content
        in
        let attr = inline_attribute_string st in
        Pre.R (Code (attr, content)) :: acc
      in
      let rec gobble_body start m =
        match peek st with
        | Some '`' ->
          junk st;
          gobble_body start (succ m)
        | _ when m = n -> finish ()
        | Some c when is_whitespace c ->
          if m > 0 then Buffer.add_string bufcode (String.make m '`');
          Buffer.add_char bufcode (if c = '\010' then ' ' else c);
          junk st;
          gobble_body (start && m = 0) 0
        | Some c ->
          junk st;
          if m > 0 then Buffer.add_string bufcode (String.make m '`');
          Buffer.add_char bufcode c;
          gobble_body false 0
        | None ->
          Buffer.add_string buf (range st p n);
          set_pos st (p + n);
          acc
      in
      gobble_body true 0
    | None ->
      Buffer.add_string buf (String.make n '`');
      acc
  in
  gobble_open_backtick 0
;;

let rec inline defs st =
  let buf = Buffer.create 0 in
  let text acc = text buf acc in
  let rec reference_link kind acc st =
    let off0 = pos st in
    match protect (link_label true) st with
    | lab ->
      let reflink lab =
        let s = normalize lab in
        match
          List.find_opt (fun ({ label; _ } : attributes link_def) -> label = s) defs
        with
        | Some { label = _; destination; title; attributes = attr } ->
          let lab1 = inline defs (of_string lab) in
          let r =
            let def = { label = lab1; destination; title } in
            match kind with
            | Pre.Img -> Image (attr, def)
            | Url -> Link (attr, def)
          in
          loop (Pre.R r :: text acc) st
        | None ->
          if kind = Img then Buffer.add_char buf '!';
          Buffer.add_char buf '[';
          let acc = text acc in
          set_pos st (succ off0);
          loop acc st
      in
      (match peek st with
       | Some '[' ->
         if peek_after '\000' st = ']'
         then (
           junk st;
           junk st;
           reflink lab)
         else (
           match protect (link_label false) st with
           | _ ->
             set_pos st off0;
             junk st;
             loop (Left_bracket kind :: text acc) st
           | exception Fail -> reflink lab)
       | Some '(' ->
         (match protect inline_link st with
          | _ ->
            set_pos st off0;
            junk st;
            loop (Left_bracket kind :: text acc) st
          | exception Fail -> reflink lab)
       | _ -> reflink lab)
    | exception Fail ->
      junk st;
      loop (Left_bracket kind :: text acc) st
  and loop ~seen_link acc st =
    match peek_exn st with
    | '<' as c ->
      (match protect autolink st with
       | def ->
         let attr = inline_attribute_string st in
         loop ~seen_link (Pre.R (Link (attr, def)) :: text acc) st
       | exception Fail ->
         (match
            protect
              (closing_tag
               ||| open_tag
               ||| html_comment
               ||| declaration
               ||| cdata_section
               ||| processing_instruction)
              st
          with
          | tag -> loop ~seen_link (Pre.R (Html ([], tag)) :: text acc) st
          | exception Fail ->
            junk st;
            Buffer.add_char buf c;
            loop ~seen_link acc st))
    | '\n' ->
      junk st;
      sp st;
      loop ~seen_link (Pre.R (Soft_break []) :: text acc) st
    | ' ' as c ->
      junk st;
      (match peek st with
       | Some ' ' ->
         (match protect (many space >>> char '\n' >>> many space) st with
          | () -> loop ~seen_link (Pre.R (Hard_break []) :: text acc) st
          | exception Fail ->
            junk st;
            Buffer.add_string buf "  ";
            loop ~seen_link acc st)
       | Some '\n' -> loop ~seen_link acc st
       | _ ->
         Buffer.add_char buf c;
         loop ~seen_link acc st)
    | '`' -> loop ~seen_link (inline_pre buf acc st) st
    | '\\' as c ->
      junk st;
      (match peek st with
       | Some '\n' ->
         junk st;
         loop ~seen_link (Pre.R (Hard_break []) :: text acc) st
       | Some c when is_punct c ->
         junk st;
         Buffer.add_char buf c;
         loop ~seen_link acc st
       | _ ->
         Buffer.add_char buf c;
         loop ~seen_link acc st)
    | '!' as c ->
      junk st;
      (match peek st with
       | Some '[' -> reference_link ~seen_link Img (text acc) st
       | _ ->
         Buffer.add_char buf c;
         loop ~seen_link acc st)
    | '&' ->
      entity buf st;
      loop ~seen_link acc st
    | ']' ->
      junk st;
      let acc = text acc in
      let rec aux ~seen_link xs = function
        | Pre.Left_bracket Url :: acc' when seen_link ->
          Buffer.add_char buf ']';
          let acc'' = List.rev_append (Pre.R (Text ([], "[")) :: xs) acc' in
          loop ~seen_link acc'' st
        | Left_bracket k :: acc' ->
          (match peek st with
           | Some '(' ->
             (match protect inline_link st with
              | destination, title ->
                let attr = inline_attribute_string st in
                let r =
                  let label = Pre.parse_emph xs in
                  let def = { label; destination; title } in
                  match k with
                  | Img -> Image (attr, def)
                  | Url -> Link (attr, def)
                in
                loop ~seen_link (Pre.R r :: acc') st
              | exception Fail ->
                Buffer.add_char buf ']';
                loop ~seen_link acc st)
           | Some '[' ->
             let label = Pre.parse_emph xs in
             let off1 = pos st in
             (match link_label false st with
              | lab ->
                let s = normalize lab in
                (match
                   List.find_opt
                     (fun ({ label; _ } : attributes link_def) -> label = s)
                     defs
                 with
                 | Some { label = _; destination; title; attributes = attr } ->
                   let def = { label; destination; title } in
                   let r =
                     match k with
                     | Img -> Image (attr, def)
                     | Url -> Link (attr, def)
                   in
                   loop ~seen_link (Pre.R r :: acc') st
                 | None ->
                   if k = Img then Buffer.add_char buf '!';
                   Buffer.add_char buf '[';
                   let acc = Pre.R label :: text acc' in
                   Buffer.add_char buf ']';
                   set_pos st off1;
                   loop ~seen_link acc st)
              | exception Fail ->
                if k = Img then Buffer.add_char buf '!';
                Buffer.add_char buf '[';
                let acc = Pre.R label :: text acc in
                Buffer.add_char buf ']';
                set_pos st off1;
                loop ~seen_link acc st)
           | _ ->
             Buffer.add_char buf ']';
             loop ~seen_link acc st)
        | (Pre.R (Link _) as x) :: acc' -> aux ~seen_link:true (x :: xs) acc'
        | x :: acc' -> aux ~seen_link (x :: xs) acc'
        | [] ->
          Buffer.add_char buf ']';
          loop ~seen_link acc st
      in
      aux ~seen_link [] acc
    | '[' -> reference_link ~seen_link Url acc st
    | ('*' | '_') as c ->
      let pre = peek_before ' ' st in
      let f post n st =
        let pre = pre |> Pre.classify_delim in
        let post = post |> Pre.classify_delim in
        let e = if c = '*' then Pre.Star else Pre.Underscore in
        loop ~seen_link (Pre.Emph (pre, post, e, n) :: text acc) st
      in
      let rec aux n =
        match peek st with
        | Some c1 when c1 = c ->
          junk st;
          aux (succ n)
        | Some c1 -> f c1 n st
        | None -> f ' ' n st
      in
      aux 0
    | _ as c ->
      junk st;
      Buffer.add_char buf c;
      loop ~seen_link acc st
    | exception Fail -> Pre.parse_emph (List.rev (text acc))
  in
  loop ~seen_link:false [] st
;;

let sp3 st =
  match peek st with
  | Some ' ' ->
    junk st;
    (match peek st with
     | Some ' ' ->
       junk st;
       (match peek st with
        | Some ' ' ->
          junk st;
          3
        | _ -> 2
        | exception Fail -> 2)
     | _ -> 1
     | exception Fail -> 1)
  | _ -> 0
  | exception Fail -> 0
;;

let link_reference_definition st : attributes link_def =
  let ws st =
    let rec loop seen_nl =
      match peek st with
      | Some w when is_whitespace w ->
        junk st;
        loop seen_nl
      | Some '\n' when not seen_nl ->
        junk st;
        loop true
      | _ -> ()
    in
    loop false
  in
  let ws1 st =
    match next st with
    | w when is_whitespace w -> ws st
    | _ -> raise Fail
  in
  ignore (sp3 st);
  let label = link_label false st in
  if next st <> ':' then raise Fail;
  ws st;
  let destination = link_destination st in
  let attributes = inline_attribute_string st in
  match protect (ws1 >>> link_title <<< sp <<< eol) st with
  | title -> { label; destination; title = Some title; attributes }
  | exception Fail ->
    (sp >>> eol) st;
    { label; destination; title = None; attributes }
;;

let link_reference_definitions st =
  let rec loop acc =
    match protect link_reference_definition st with
    | def -> loop (def :: acc)
    | exception Fail -> acc, pos st
  in
  loop []
;;
