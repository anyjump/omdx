(* src/frontmatter_parser.mli *)
open Ast.Impl

(** Parses frontmatter from the beginning of a string or channel. *)

(** Parses YAML-like frontmatter from the beginning of a string.
    Looks for `---` delimiters.
    Returns `(Some metadata, remaining_content_string)` if frontmatter is found and valid.
    Returns `(None, original_string)` if no valid frontmatter is found. *)
val parse_string : string -> metadata option * string

(** Parses YAML-like frontmatter from the beginning of an input channel.
    Looks for `---` delimiters.
    Returns `(Some metadata, first_line_after_frontmatter_option)` if frontmatter is found.
    Returns `(None, first_line_of_original_content_option)` if no valid frontmatter is found.
    The second element of the tuple is the first line *not* part of the frontmatter,
    which might be None if the file ends immediately after the frontmatter or is empty. *)
val parse_channel : in_channel -> metadata option * string option
