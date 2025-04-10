(* Phase 1 block parser *)

open Ast.Util
module Raw = Ast_block.Raw

module Pre = struct
  type container =
    | Rblockquote of t (* An open blockquote containing its own state *)
    | Rlist of list_type * list_spacing * bool * int * attributes Raw.block list list * t
      (* An open list with type, spacing, previous empty line flag, indentation, closed items, and state *)
    | Rparagraph of string list (* An open paragraph with a list of lines *)
    | Rfenced_code of
        int * int * Parser.code_block_kind * (string * string) * string list * attributes
      (* An open fenced code block with indentation, fence length, kind, info string, lines, and attributes *)
    | Rindented_code of string list (* An open indented code block with lines *)
    (* Updated Rhtml state *)
    | Rhtml of
        string
        * attributes
        * t (* opening_tag_name, opening_tag_attributes, nested_parser_state *)
    | Rdef_list of
        string * string list (* An open definition list with term and definitions *)
    | Rtable_header of Str_slice.t list * string
      (* A potential table header with header cells and the original line *)
    | Rtable of
        (string * cell_alignment) list
        * string list list (* An open table with headers and rows *)
    | Rempty (* No open container *)

  and t =
    { blocks : attributes Raw.block list
    ; next : container
    }

  let concat l = String.concat "\n" (List.rev l) ^ "\n"

  let trim_left s =
    let rec loop i =
      if i >= String.length s
      then i
      else (
        match s.[i] with
        | ' ' | '\t' -> loop (succ i)
        | _ -> i)
    in
    let i = loop 0 in
    if i > 0 then String.sub s i (String.length s - i) else s
  ;;

  let link_reference_definitions s =
    let defs, off = Parser.link_reference_definitions (Parser.P.of_string s) in
    let s = String.sub s off (String.length s - off) |> String.trim in
    defs, s
  ;;

  let rec close link_defs { blocks; next } =
    let finish = finish link_defs in
    (* Ensure finish is defined before use *)
    match next with
    | Rblockquote state -> Raw.Blockquote ([], finish state) :: blocks
    | Rlist (ty, sp, _, _, closed_items, state) ->
      List ([], ty, sp, List.rev (finish state :: closed_items)) :: blocks
    | Rparagraph l ->
      let s = concat (List.map trim_left l) in
      let defs, off = Parser.link_reference_definitions (Parser.P.of_string s) in
      let s = String.sub s off (String.length s - off) |> String.trim in
      link_defs := defs @ !link_defs;
      if s = "" then blocks else Paragraph ([], s) :: blocks
    | Rfenced_code (_, _, _kind, (label, _other), [], attr) ->
      Code_block (attr, label, "") :: blocks
    | Rfenced_code (_, _, _kind, (label, _other), l, attr) ->
      Code_block (attr, label, concat l) :: blocks
    | Rdef_list (term, defs) ->
      let l, blocks' =
        (* Use blocks' to avoid shadowing *)
        match blocks with
        | Definition_list (_, l) :: b -> l, b
        | b -> [], b
      in
      Definition_list ([], l @ [ { term; defs = List.rev defs } ]) :: blocks'
    | Rindented_code l ->
      let rec loop = function
        | "" :: ls -> loop ls
        | ls -> ls
      in
      Code_block ([], "", concat (loop l)) :: blocks
      (* Updated Rhtml closing logic *)
    | Rhtml (tag, attr, state) ->
      Html_block (attr, tag, finish state) :: blocks (* Add tag *)
    | Rtable_header (_header, line) ->
      close link_defs { blocks; next = Rparagraph [ line ] }
    | Rtable (header, rows) -> Table ([], header, List.rev rows) :: blocks
    | Rempty -> blocks

  and finish link_defs state = List.rev (close link_defs state)

  let empty = { blocks = []; next = Rempty }
  let classify_line s = Parser.parse s

  let classify_delimiter s =
    let left, s =
      match Str_slice.head s with
      | Some ':' -> true, Str_slice.drop 1 s
      | _ -> false, s
    in
    let right, s =
      match Str_slice.last s with
      | Some ':' -> true, Str_slice.drop_last s
      | _ -> false, s
    in
    if Str_slice.exists (fun c -> c <> '-') s
    then None
    else (
      match left, right with
      | true, true -> Some Centre
      | true, false -> Some Left
      | false, true -> Some Right
      | false, false -> Some Default)
  ;;

  let match_table_headers headers delimiters =
    let rec loop processed = function
      | [], [] -> Some (List.rev processed)
      | header :: hs, line :: ds ->
        (match classify_delimiter line with
         | None -> None
         | Some alignment ->
           loop ((Str_slice.to_string header, alignment) :: processed) (hs, ds))
      | _ -> None
    in
    loop [] (headers, delimiters)
  ;;

  let rec match_row_length l1 l2 =
    match l1, l2 with
    | [], _ -> []
    | _, [] -> List.init (List.length l1) (fun _ -> "")
    | _ :: l1', x :: l2' -> Str_slice.to_string x :: match_row_length l1' l2'
  ;;

  let rec process link_defs { blocks; next } s =
    let process = process link_defs in
    let close = close link_defs in
    let finish = finish link_defs in
    match next, classify_line s with
    (* === TOP-LEVEL: We're not inside any block === *)
    | Rempty, Parser.Lempty -> { blocks; next = Rempty }
    | Rempty, Lblockquote s' -> { blocks; next = Rblockquote (process empty s') }
    | Rempty, Lthematic_break -> { blocks = Thematic_break [] :: blocks; next = Rempty }
    | Rempty, Lsetext_heading { level = 2; len } when len >= 3 ->
      { blocks = Thematic_break [] :: blocks; next = Rempty }
    | Rempty, Latx_heading (level, text, attr) ->
      { blocks = Heading (attr, level, text) :: blocks; next = Rempty }
    | Rempty, Lfenced_code (ind, num, q, info, a) ->
      { blocks; next = Rfenced_code (ind, num, q, info, [], a) }
    (* Updated HTML block start *)
    | Rempty, Lhtml_start (tag, attr) -> { blocks; next = Rhtml (tag, attr, empty) }
    (* Ignore Lhtml_end at top level - treat as paragraph *)
    | Rempty, Lhtml_end _ -> { blocks; next = Rparagraph [ Str_slice.to_string s ] }
    | Rempty, Lindented_code s' ->
      { blocks; next = Rindented_code [ Str_slice.to_string s' ] }
    | Rempty, Llist_item (kind, indent, s') ->
      { blocks; next = Rlist (kind, Tight, false, indent, [], process empty s') }
    | Rempty, (Lsetext_heading _ | Lparagraph | Ldef_list _ | Ltable_line []) ->
      { blocks; next = Rparagraph [ Str_slice.to_string s ] }
    | Rempty, Ltable_line items ->
      { blocks; next = Rtable_header (items, Str_slice.to_string s) }
    (* === DEFINITION LIST HANDLING === *)
    | Rparagraph [ h ], Ldef_list def -> { blocks; next = Rdef_list (h, [ def ]) }
    | Rdef_list (term, defs), Ldef_list def ->
      { blocks; next = Rdef_list (term, def :: defs) }
    (* === PARAGRAPH HANDLING === *)
    | Rparagraph _, Llist_item ((Ordered (1, _) | Bullet _), _, s1)
      when not (Parser.is_empty (Parser.P.of_string (Str_slice.to_string s1))) ->
      process { blocks = close { blocks; next }; next = Rempty } s
    (* Close paragraph before starting certain blocks *)
    | ( Rparagraph _
      , ( Lempty
        | Lblockquote _
        | Lthematic_break
        | Latx_heading _
        | Lfenced_code _
        | Lhtml_start _
        (* Starting HTML block closes paragraph *)
        (* Other block types that should close a paragraph *) ) ) ->
      process { blocks = close { blocks; next }; next = Rempty } s
    | Rparagraph lines, Lsetext_heading { level; _ } ->
      let text = concat (List.map trim_left lines) in
      let defs, text = link_reference_definitions text in
      link_defs := defs @ !link_defs;
      if text = ""
      then
        { blocks; next = Rparagraph [ Str_slice.to_string s ] }
        (* Keep line if heading was empty *)
      else { blocks = Heading ([], level, text) :: blocks; next = Rempty }
    | Rparagraph lines, _ ->
      (* Includes Lhtml_end, Lindented_code, Lparagraph, etc. *)
      { blocks; next = Rparagraph (Str_slice.to_string s :: lines) }
    (* === FENCED CODE BLOCK === *)
    | Rfenced_code (_, num, q, _, _, _), Lfenced_code (_, num', q1, ("", _), _)
      when num' >= num && q = q1 -> { blocks = close { blocks; next }; next = Rempty }
    | Rfenced_code (ind, num, q, info, lines, a), _ ->
      let s_line =
        if ind > 0 then Str_slice.offset (min (Parser.indent s) ind) s else s
      in
      { blocks
      ; next = Rfenced_code (ind, num, q, info, Str_slice.to_string s_line :: lines, a)
      }
    (* === CONTINUING DEF LIST === *)
    | Rdef_list (term, d :: defs), Lparagraph ->
      { blocks; next = Rdef_list (term, (d ^ "\n" ^ Str_slice.to_string s) :: defs) }
    | Rdef_list _, _ -> process { blocks = close { blocks; next }; next = Rempty } s
    (* === TABLE HANDLING === *)
    | Rtable_header (headers, line), Ltable_line items ->
      (match match_table_headers headers items with
       | Some hdrs -> { blocks; next = Rtable (hdrs, []) }
       | None ->
         process
           { blocks = close { blocks; next = Rparagraph [ line ] }; next = Rempty }
           s)
    | Rtable_header (_, line), _ ->
      process { blocks = close { blocks; next = Rparagraph [ line ] }; next = Rempty } s
    | Rtable (header, rows), Ltable_line row_items ->
      let row = match_row_length header row_items in
      { blocks; next = Rtable (header, row :: rows) }
    | Rtable (header, rows), (Lparagraph | Lsetext_heading _) ->
      (* Lazy continuation *)
      let row = match_row_length header [ s ] in
      { blocks; next = Rtable (header, row :: rows) }
    | Rtable _, _ -> process { blocks = close { blocks; next }; next = Rempty } s
    (* === INDENTED CODE BLOCK === *)
    | Rindented_code lines, Lindented_code s' ->
      { blocks; next = Rindented_code (Str_slice.to_string s' :: lines) }
    | Rindented_code lines, Lempty ->
      let n = min (Parser.indent s) 4 in
      let s_line = Str_slice.offset n s in
      { blocks; next = Rindented_code (Str_slice.to_string s_line :: lines) }
    | Rindented_code _, _ -> process { blocks = close { blocks; next }; next = Rempty } s
    (* === HTML BLOCK === *)
    | Rhtml (opening_tag, opening_attr, inner_state), Lhtml_end closing_tag
      when opening_tag = closing_tag ->
      (* Found the matching closing tag *)
      let inner_blocks = finish inner_state in
      { blocks = Html_block (opening_attr, opening_tag, inner_blocks) :: blocks
      ; next = Rempty
      }
    | ( Rhtml (opening_tag, opening_attr, inner_state)
      , Lhtml_start (_nested_tag, _nested_attr) ) ->
      (* Handle a nested HTML start tag: Pass the line to the inner parser state *)
      let new_inner_state = process inner_state s in
      { blocks; next = Rhtml (opening_tag, opening_attr, new_inner_state) }
    | Rhtml (opening_tag, opening_attr, inner_state), _ ->
      (* Any other line (Paragraph, List, etc.): Process it using the inner parser state *)
      let new_inner_state = process inner_state s in
      { blocks; next = Rhtml (opening_tag, opening_attr, new_inner_state) }
    (* === BLOCKQUOTE === *)
    | Rblockquote state, Lblockquote s' ->
      { blocks; next = Rblockquote (process state s') }
    (* === LIST HANDLING === *)
    | Rlist (kind, style, _, ind, items, state), Lempty ->
      (* Pass empty line to inner state, set prev_empty flag *)
      { blocks; next = Rlist (kind, style, true, ind, items, process state s) }
    | Rlist (_, _, true, ind, _, { blocks = []; next = Rempty }), _
      when Parser.indent s < ind ->
      (* Blank line followed by non-indented line: close the list *)
      process { blocks = close { blocks; next }; next = Rempty } s
    | Rlist (kind, style, prev_empty, ind, items, state), _ when Parser.indent s >= ind ->
      (* Line is indented enough to continue the current list item *)
      let s_line = Str_slice.offset ind s in
      let state' = process state s_line in
      (* Process the indented line *)
      (* Determine list spacing (tight/loose) *)
      let style' =
        (* Check if the *newly processed* inner state starts a new block *)
        let starts_new_block = function
          | Rblockquote { blocks = []; next = Rempty } -> true
          | Rlist (_, _, _, _, _, { blocks = []; next = Rempty }) -> true
          | Rhtml (_, _, { blocks = []; next = Rempty }) -> true (* Check inner state *)
          | Rparagraph [ _ ] -> true
          | Rfenced_code (_, _, _, _, [], _) -> true
          | Rindented_code [ _ ] -> true
          | _ -> false
        in
        if prev_empty && starts_new_block state'.next then Loose else style
      in
      { blocks; next = Rlist (kind, style', false, ind, items, state') }
    | Rlist (kind, style, prev_empty, _, items, state), Llist_item (kind', ind, s')
      when same_block_list_kind kind kind' ->
      (* New list item of the same kind *)
      let style' = if prev_empty then Loose else style in
      { blocks
      ; next = Rlist (kind, style', false, ind, finish state :: items, process empty s')
      }
    (* === DEFAULT: Close surrounding block and reprocess === *)
    | (Rlist _ | Rblockquote _), _ ->
      let rec find_innermost_paragraph_continuation = function
        (* Correctly reconstruct Rlist with the updated inner state *)
        | Rlist (kind, style, prev_empty, ind, items, { blocks; next }) ->
          (match find_innermost_paragraph_continuation next with
           | Some next' ->
             Some (Rlist (kind, style, prev_empty, ind, items, { blocks; next = next' }))
           | None -> None)
        (* Correctly reconstruct Rblockquote with the updated inner state *)
        | Rblockquote { blocks; next } ->
          (match find_innermost_paragraph_continuation next with
           | Some next' -> Some (Rblockquote { blocks; next = next' })
           | None -> None)
        (* Correctly reconstruct Rhtml with the updated inner state *)
        | Rhtml (tag, attr, { blocks; next }) ->
          (match find_innermost_paragraph_continuation next with
           | Some next' -> Some (Rhtml (tag, attr, { blocks; next = next' }))
           | None -> None)
        (* Base case: Found a paragraph that can be continued *)
        | Rparagraph (_ :: _ as lines) ->
          (match classify_line s with
           | Parser.Lparagraph | Lindented_code _
           | Lsetext_heading { level = 1; _ }
           | Lhtml_end _ -> Some (Rparagraph (Str_slice.to_string s :: lines))
           | _ -> None)
        (* Cannot continue paragraph *)
        (* Base case: Not a container we can recurse into or a continuable paragraph *)
        | _ -> None
      in
      (match find_innermost_paragraph_continuation next with
       | Some next' ->
         { blocks; next = next' } (* Update the state with the modified container *)
       | None -> process { blocks = close { blocks; next }; next = Rempty } s)
  ;;

  (* Close and reprocess *)

  (* Close current block and reprocess line *)

  let process link_defs state s = process link_defs state (Str_slice.of_string s)

  let of_channel ic =
    let link_defs = ref [] in
    let rec loop state =
      match input_line ic with
      | s -> loop (process link_defs state s)
      | exception End_of_file ->
        let blocks = finish link_defs state in
        blocks, List.rev !link_defs
    in
    loop empty
  ;;

  let read_line s off =
    let buf = Buffer.create 128 in
    let rec loop cr_read off =
      if off >= String.length s
      then Buffer.contents buf, None
      else (
        match s.[off] with
        | '\n' -> Buffer.contents buf, Some (succ off)
        | '\r' ->
          if cr_read then Buffer.add_char buf '\r';
          loop true (succ off)
        | c ->
          if cr_read then Buffer.add_char buf '\r';
          Buffer.add_char buf c;
          loop false (succ off))
    in
    loop false off
  ;;

  let of_string s =
    let link_defs = ref [] in
    let rec loop state = function
      | None ->
        let blocks = finish link_defs state in
        blocks, List.rev !link_defs
      | Some off ->
        let s, off' = read_line s off in
        loop (process link_defs state s) off'
    in
    loop empty (Some 0)
  ;;
end
