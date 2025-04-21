(* A frontmatter parser that runs before the initial block parser *)

(* Parses a single "key: value" line. Returns None for comments, empty lines, or malformed lines. *)
let parse_yaml_line line =
  let trimmed_line = String.trim line in
  if trimmed_line = "" || String.starts_with ~prefix:"#" trimmed_line
  then None (* Skip empty lines and comments *)
  else (
    match String.index_opt trimmed_line ':' with
    | None ->
      (* Handle potential error: line without a colon. Maybe log a warning? *)
      Printf.eprintf
        "[Warning] Omdx: Malformed frontmatter line (missing ':'): %s\n%!"
        line;
      None
    | Some index ->
      let key = String.trim (String.sub trimmed_line 0 index) in
      (* Get substring *after* the colon *)
      let value_part =
        if index + 1 < String.length trimmed_line
        then String.sub trimmed_line (index + 1) (String.length trimmed_line - index - 1)
        else "" (* Key with colon but no value *)
      in
      let value = String.trim value_part in
      if key = ""
      then (
        (* Handle potential error: empty key. Maybe log a warning? *)
        Printf.eprintf
          "[Warning] Omdx: Malformed frontmatter line (empty key): %s\n%!"
          line;
        None)
      else Some (key, value))
;;

(* --- Parse String Implementation --- *)
let parse_string s =
  let frontmatter_ref = ref None in
  let offset = ref 0 in
  (* Will store the offset of the content *after* frontmatter *)
  try
    let line1, off1_opt = Parser.read_line s 0 in
    (* Use Parser.read_line to handle \r\n *)
    if String.trim line1 <> "---"
    then None, s (* No frontmatter start delimiter found *)
    else (
      match off1_opt with
      | None -> None, s (* Only --- found in the whole string *)
      | Some off1 ->
        let fm_lines = ref [] in
        let current_offset = ref off1 in
        let terminated = ref false in
        (* Read lines until EOF or closing --- *)
        while (not !terminated) && !current_offset < String.length s do
          let line, next_off_opt = Parser.read_line s !current_offset in
          (* Update offset for the *next* line or end of string *)
          current_offset := Option.value next_off_opt ~default:(String.length s);
          if String.trim line = "---"
          then (
            offset := !current_offset;
            (* Mark end of frontmatter *)
            terminated := true)
          else fm_lines := line :: !fm_lines
        done;
        (* Process collected lines if frontmatter was properly terminated *)
        if !terminated
        then (
          let fm_list = List.rev !fm_lines |> List.filter_map parse_yaml_line in
          (* Only set frontmatter if it's not empty *)
          if fm_list <> [] then frontmatter_ref := Some fm_list;
          (* Return metadata and the rest of the string *)
          !frontmatter_ref, String.sub s !offset (String.length s - !offset))
        else (
          (* Unterminated frontmatter - treat as if no frontmatter existed *)
          Printf.eprintf "[Warning] Omdx: Unterminated frontmatter block.\n%!";
          None, s))
  with
  (* Handle cases where the string is empty or reading fails unexpectedly *)
  | End_of_file | Failure _ -> None, s
;;

(* --- Parse Channel Implementation --- *)
let parse_channel ic =
  let frontmatter_ref = ref None in
  let first_line_after = ref None in
  try
    let line1 = input_line ic in
    if String.trim line1 <> "---"
    then (
      (* First line is not ---, so it's the first content line *)
      first_line_after := Some line1;
      None, !first_line_after)
    else (
      (* Found opening --- *)
      let fm_lines = ref [] in
      let terminated = ref false in
      try
        while not !terminated do
          let line = input_line ic in
          if String.trim line = "---"
          then terminated := true
          else fm_lines := line :: !fm_lines
        done;
        let fm_list = List.rev !fm_lines |> List.filter_map parse_yaml_line in
        if fm_list <> [] then frontmatter_ref := Some fm_list;
        (* Try reading the first line *after* the frontmatter *)
        (try first_line_after := Some (input_line ic) with
         | End_of_file -> first_line_after := None);
        !frontmatter_ref, !first_line_after
      with
      | End_of_file ->
        (* Unterminated frontmatter *)
        Printf.eprintf "[Warning] Omdx: Unterminated frontmatter block at EOF.\n%!";
        (* No frontmatter parsed, and no lines after *)
        None, None)
  with
  | End_of_file ->
    (* File was empty or just --- *)
    None, None
;;
