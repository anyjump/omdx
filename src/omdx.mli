(** {1 A markdown parser in OCaml} *)

(** {2 The document model}

    The following types define the AST representing Omd's document model. *)

include Ast.Intf

(** {2 Helper functions for constructing the document AST } *)

module Ctor : Ast_constructors.Intf

(** {2 Intermediate HTML Generation}

    Provides access to the intermediate HTML representation and rendering functions.
    Direct manipulation requires understanding the {!Omdx.Html.t} type. *)
module Html : module type of Html (* Exposes the signature of the Html module *)

(** {2 Generating and constructing tables of contents} *)

val headers : ?remove_links:bool -> 'attr block list -> ('attr * int * 'attr inline) list
val toc : ?start:int list -> ?depth:int -> content -> content

(** {2 Helper functions} *)

(** Perform escaping of HTML entities. Turns: ['"'] into ["&quot;"],
    ['&'] into ["&amp;"], ['<'] in ["&lt;"] and ['>'] into ["&gt;"]
*)
val escape_html_entities : string -> string

val content : document -> content
val metadata : document -> metadata option
val metadata_to_string : metadata option -> string

(** {2 Converting to and from documents} *)

(** Parses an entire document from an input channel, including optional frontmatter. *)
val of_channel : in_channel -> document

(** Parses an entire document from a string, including optional frontmatter. *)
val of_string : string -> document

(** Converts the content part of a document to HTML. Ignores metadata. *)
val to_html : ?auto_identifiers:bool -> content -> string

(** Converts the content part of a document to S-expression format. Ignores metadata. *)
val to_sexp : document -> string
