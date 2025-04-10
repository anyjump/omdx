(* Use the entities array from the file you found *)
let entities = Data_html.entities 

(* Create a hashtable with exact size needed for the entities *)
let entity_table = Hashtbl.create (Array.length entities)

(* Initialize the hashtable *)
let () =
  Array.iter
    (fun (name, char) -> Hashtbl.add entity_table name char)
    entities

let lookup name =
  match Hashtbl.find_opt entity_table name with
  | Some char ->
      (* Convert Unicode character to list of Uchar.t *)
      let rec collect_code_points i acc =
        if i < 0 then acc
        else 
          let code = Char.code (String.get char i) in
          let uchar = Uchar.of_int code in
          collect_code_points (i-1) (uchar :: acc)
      in
      collect_code_points (String.length char - 1) []
  | None -> []  (* Return empty list if not found *)
