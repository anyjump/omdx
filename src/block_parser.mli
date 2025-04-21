(* src/block_parser.mli *)
open Ast.Impl
module Raw = Ast_block.Raw

module Pre : sig
  (** The internal state of the block parser. *)
  type t

  (** The initial empty state for the block parser. *)
  val empty : t

  (** Processes a single line of input and updates the parser state.
      It takes the mutable list of link definitions found so far,
      the current parser state, and the input line string.
      It returns the new parser state. *)
  val process : attributes Parser.link_def list ref -> t -> string -> t

  (** Finalizes the parsing process.
      Takes the mutable list of link definitions and the final parser state.
      Returns the list of parsed raw blocks. *)
  val finish : attributes Parser.link_def list ref -> t -> attributes Raw.block list

  (** Parses blocks from an input channel.
      Returns the list of parsed raw blocks and the list of found link definitions. *)
  val of_channel
    :  in_channel
    -> attributes Raw.block list * attributes Parser.link_def list

  (** Parses blocks from a string.
      Returns the list of parsed raw blocks and the list of found link definitions. *)
  val of_string : string -> attributes Raw.block list * attributes Parser.link_def list
end
