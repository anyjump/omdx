module type BlockContent = sig
  type 'a t
end

module StringContent = struct
  type 'attr t = string
end

module InlineContent = struct
  type 'attr t = 'attr Ast_inline.inline
end

module List_types = struct
  type list_type =
    | Ordered of int * char
    | Bullet of char

  type list_spacing =
    | Loose
    | Tight
end

module Table_alignments = struct
  type cell_alignment =
    | Default
    | Left
    | Centre
    | Right
end

open List_types
open Table_alignments

module Make (C : BlockContent) = struct
  type 'attr def_elt =
    { term : 'attr C.t
    ; defs : 'attr C.t list
    }

  (* A value of type 'attr is present in all variants of this type. We use it to associate
     extra information to each node in the AST. Cn the common case, the attributes type defined
     above is used. We might eventually have an alternative function to parse blocks while keeping
     concrete information such as source location and we'll use it for that as well. *)
  type 'attr block =
    | Paragraph of 'attr * 'attr C.t
    | List of 'attr * list_type * list_spacing * 'attr block list list
    | Blockquote of 'attr * 'attr block list
    | Thematic_break of 'attr
    | Heading of 'attr * int * 'attr C.t
    | Code_block of 'attr * string * string
    | Html_block of 'attr * string * 'attr block list
    (* attributes, tag, content blocks *)
    | Definition_list of 'attr * 'attr def_elt list
    | Table of 'attr * ('attr C.t * cell_alignment) list * 'attr C.t list list
    (** A table is represented by a header row, which is a list of pairs of
            header cells and alignments, and a list of rows *)
end

module MakeMapper (Src : BlockContent) (Dst : BlockContent) = struct
  module SrcBlock = Make (Src)
  module DstBlock = Make (Dst)

  let rec map (f : 'attr Src.t -> 'attr Dst.t)
    : 'attr SrcBlock.block -> 'attr DstBlock.block
    = function
    | SrcBlock.Paragraph (attr, x) -> DstBlock.Paragraph (attr, f x)
    | List (attr, ty, sp, bl) -> List (attr, ty, sp, List.map (List.map (map f)) bl)
    | Blockquote (attr, xs) -> Blockquote (attr, List.map (map f) xs)
    | Thematic_break attr -> Thematic_break attr
    | Heading (attr, level, text) -> Heading (attr, level, f text)
    | Definition_list (attr, l) ->
      let map_def_elt { SrcBlock.term; defs } =
        (* Use a different name for the inner mapping function *)
        { DstBlock.term = f term; defs = List.map f defs }
      in
      Definition_list (attr, List.map map_def_elt l)
      (* Apply the specific mapping function *)
    | Code_block (attr, label, code) -> Code_block (attr, label, code)
    (* --- MODIFIED Html_block Case --- *)
    (* Assumes the AST definition is now: Html_block of 'attr * string * 'attr block list *)
    | SrcBlock.Html_block (attr, tag, xs) ->
      (* Match the tag name *)
      DstBlock.Html_block (attr, tag, List.map (map f) xs)
      (* Pass the tag name to the destination *)
      (* --- End Modification --- *)
    | Table (attr, headers, rows) ->
      Table
        ( attr
        , List.map (fun (header, alignment) -> f header, alignment) headers
        , List.map (List.map f) rows )
  ;;
end

module Mapper = MakeMapper (StringContent) (InlineContent)
module Raw = Make (StringContent)
module WithInline = Make (InlineContent)
