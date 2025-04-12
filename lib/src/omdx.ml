(* The document model *)

include Ast.Impl

(* Helper functions for construction document AST *)

module Ctor = Ast_constructors.Impl

(* Expose the Html module directly *)
module Html = Html

(* Table of contents *)

let headers = Toc.headers
let toc = Toc.toc

(* Conversion *)

let parse_inline defs s = Parser.inline defs (Parser.P.of_string s)

let parse_inlines (md, defs) : doc =
  let defs =
    let f (def : attributes Parser.link_def) =
      { def with label = Parser.normalize def.label }
    in
    List.map f defs
  in
  List.map (Ast_block.Mapper.map (parse_inline defs)) md
;;

let escape_html_entities = Html.htmlentities
let of_channel ic : doc = parse_inlines (Block_parser.Pre.of_channel ic)
let of_string s = parse_inlines (Block_parser.Pre.of_string s)

(* Implement to_html_t *)
let to_html_t ?auto_identifiers doc = Html.of_doc ?auto_identifiers doc

(* Implement html_to_string *)
let html_to_string t = Html.to_string t

(* Keep the direct to_html function *)
let to_html ?auto_identifiers doc = html_to_string (to_html_t ?auto_identifiers doc)
(* Or directly: Html.to_string (Html.of_doc ?auto_identifiers doc) *)

(* Keep the existing to_sexp *)
let to_sexp ast = Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)
