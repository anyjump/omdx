(* src/html.mli *)
open Ast.Impl (* Ensure types from Ast.Impl are available *)

type element_type =
  | Inline
  | Block
  | Table

(* The intermediate HTML representation type *)
type t =
  | Element of element_type * string * attributes * t option
  | Text of string
  | Raw of string
  | Null
  | Concat of t * t

(* Escapes HTML special characters *)
val htmlentities : string -> string

(* Converts an Omdx document AST to the intermediate HTML representation *)
val of_doc : ?auto_identifiers:bool -> doc -> t (* doc is Ast.Impl.doc *)

(* Converts the intermediate HTML representation to a final string *)
val to_string : t -> string
