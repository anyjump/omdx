(* The contentument model *)

include Ast.Impl

(* Helper functions for construction contentument AST *)

module Ctor = Ast_constructors.Impl

(* Expose the Html module directly *)
module Html = Html

(* Table of contents *)

let headers = Toc.headers
let toc = Toc.toc

(* Conversion *)

let parse_inline defs s = Parser.inline defs (Parser.P.of_string s)

(* let parse_inlines (md, defs) : content = *)
(*   let defs = *)
(*     let f (def : attributes Parser.link_def) = *)
(*       { def with label = Parser.normalize def.label } *)
(*     in *)
(*     List.map f defs *)
(*   in *)
(*   List.map (Ast_block.Mapper.map (parse_inline defs)) md *)
(* ;; *)

(* Helper to parse content blocks from a string and link definitions *)
let parse_content_string_and_defs content_string =
  let blocks, defs = Block_parser.Pre.of_string content_string in
  let defs =
    let f (def : attributes Parser.link_def) =
      { def with label = Parser.normalize def.label }
    in
    List.map f defs
  in
  List.map (Ast_block.Mapper.map (parse_inline defs)) blocks, defs
;;

(* Helper to parse content blocks from a channel and link definitions,
   handling the first line potentially already read *)
let parse_content_channel_and_defs ic first_line_opt =
  let state = Block_parser.Pre.empty in
  let link_defs = ref [] in
  (* Process the first line if it exists *)
  let initial_state =
    match first_line_opt with
    | Some line -> Block_parser.Pre.process link_defs state line
    | None -> state
  in
  (* Process the rest of the channel *)
  let rec loop state =
    match input_line ic with
    | s -> loop (Block_parser.Pre.process link_defs state s)
    | exception End_of_file ->
      let blocks = Block_parser.Pre.finish link_defs state in
      blocks, List.rev !link_defs
  in
  let blocks, defs = loop initial_state in
  (* Normalize link defs *)
  let defs =
    let f (def : attributes Parser.link_def) =
      { def with label = Parser.normalize def.label }
    in
    List.map f defs
  in
  List.map (Ast_block.Mapper.map (parse_inline defs)) blocks, defs
;;

let escape_html_entities = Html.htmlentities
let content document : content = document.content
let metadata document : metadata option = document.metadata

let of_channel ic : document =
  (* 1. Try parsing frontmatter *)
  let metadata_opt, first_line_opt = Frontmatter_parser.parse_channel ic in
  (* 2. Parse the remaining content *)
  let content_blocks, _defs = parse_content_channel_and_defs ic first_line_opt in
  (* 3. Combine into document *)
  { metadata = metadata_opt; content = content_blocks }
;;

let of_string s : document =
  (* 1. Try parsing frontmatter *)
  let metadata_opt, remaining_content = Frontmatter_parser.parse_string s in
  (* 2. Parse the remaining content *)
  let content_blocks, _defs = parse_content_string_and_defs remaining_content in
  (* 3. Combine into document *)
  { metadata = metadata_opt; content = content_blocks }
;;

(* Implement to_html_t *)
let to_html_t ?auto_identifiers content = Html.of_content ?auto_identifiers content

(* Implement html_to_string *)
let html_to_string t = Html.to_string t

(* Helper function to convert metadata to a string for printing *)
let metadata_to_string (meta_opt : metadata option) : string =
  match meta_opt with
  | None -> "Metadata: None"
  | Some [] ->
    "Metadata: Empty"
    (* Explicitly show if metadata block was present but empty/invalid *)
  | Some kvs ->
    let kv_strings = List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v) kvs in
    "Metadata: " ^ String.concat ", " kv_strings
;;

(* Keep the direct to_html function *)
let to_html ?auto_identifiers content =
  html_to_string (to_html_t ?auto_identifiers content)
;;

(* Or directly: Html.to_string (Html.of_content ?auto_identifiers content) *)

(* Keep the existing to_sexp *)
let to_sexp doc = Format.asprintf "@[%a@]@." Sexp.print (Sexp.create doc)
