(* Define tables for computation, destination, and jump *)
let comp_table = [
  ("0", "0101010"); ("1", "0111111"); ("-1", "0111010");
  ("D", "0001100"); ("A", "0110000"); ("M", "1110000");
  ("!D", "0001101"); ("!A", "0110001"); ("!M", "1110001");
  ("-D", "0001111"); ("-A", "0110011"); ("-M", "1110011");
  ("D+1", "0011111"); ("A+1", "0110111"); ("M+1", "1110111");
  ("D-1", "0001110"); ("A-1", "0110010"); ("M-1", "1110010");
  ("D+A", "0000010"); ("D+M", "1000010"); ("D-A", "0010011");
  ("D-M", "1010011"); ("A-D", "0000111"); ("M-D", "1000111");
  ("D&A", "0000000"); ("D&M", "1000000"); ("D|A", "0010101");
  ("D|M", "1010101")
]

let dest_table = [
  ("", "000"); ("M", "001"); ("D", "010"); ("MD", "011");
  ("A", "100"); ("AM", "101"); ("AD", "110"); ("AMD", "111")
]

let jump_table = [
  ("", "000"); ("JGT", "001"); ("JEQ", "010");
  ("JGE", "011"); ("JLT", "100"); ("JNE", "101");
  ("JLE", "110"); ("JMP", "111")
]

(* Lookup functions *)
let lookup table key =
  try List.assoc key table
  with Not_found -> failwith ("Invalid assembly code: " ^ key)

(* Convert integer to 15-bit binary string *)
let int_to_binary n =
  let rec to_bin n =
    if n = 0 then ""
    else to_bin (n / 2) ^ string_of_int (n mod 2)
  in
  let bin = to_bin n in
  let padding = String.make (15 - String.length bin) '0' in
  padding ^ bin

(* Convert A-instruction to binary *)
let convert_a_instruction value =
  "0" ^ int_to_binary value

(* Convert C-instruction to binary *)
let convert_c_instruction dest comp jump =
  let comp_bits = lookup comp_table comp in
  let dest_bits = lookup dest_table dest in
  let jump_bits = lookup jump_table jump in
  "111" ^ comp_bits ^ dest_bits ^ jump_bits

(* Parse an assembly instruction *)
let parse_instruction instruction =
  if String.length instruction = 0 || instruction.[0] = '/' then
    (* Skip empty or comment lines *)
    ""
  else if String.get instruction 0 = '@' then
    (* A-instruction: @value *)
    let value = String.sub instruction 1 (String.length instruction - 1) |> int_of_string in
    convert_a_instruction value
  else
    (* C-instruction: dest=comp;jump *)
    let eq_pos = try Some (String.index instruction '=') with Not_found -> None in
    let semi_pos = try Some (String.index instruction ';') with Not_found -> None in
    let dest, comp, jump =
      match eq_pos, semi_pos with
      | Some eq_pos, Some semi_pos ->
        let dest = String.sub instruction 0 eq_pos in
        let comp = String.sub instruction (eq_pos + 1) (semi_pos - eq_pos - 1) in
        let jump = String.sub instruction (semi_pos + 1) (String.length instruction - semi_pos - 1) in
        (dest, comp, jump)
      | Some eq_pos, None ->
        let dest = String.sub instruction 0 eq_pos in
        let comp = String.sub instruction (eq_pos + 1) (String.length instruction - eq_pos - 1) in
        (dest, comp, "")
      | None, Some semi_pos ->
        let comp = String.sub instruction 0 semi_pos in
        let jump = String.sub instruction (semi_pos + 1) (String.length instruction - semi_pos - 1) in
        ("", comp, jump)
      | None, None ->
        ("", instruction, "")
    in
    convert_c_instruction dest comp jump

(* Assemble a list of instructions *)
let assemble instructions =
  List.filter (fun s -> s <> "") (List.map parse_instruction instructions)

(* Main function to assemble Hack code *)
let () =
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let instructions = (* Read input file lines into list of instructions *)
    let ic = open_in input_file in
    let rec read_lines acc =
      try
        let line = input_line ic in
        let line = String.trim line in
        if line = "" || String.sub line 0 2 = "//" then (* Skip comments *)
          read_lines acc
        else
          read_lines (line :: acc)
      with End_of_file ->
        close_in ic; List.rev acc
    in
    read_lines []
  in
  let binary_instructions = assemble instructions in
  (* Write binary instructions to output file *)
  let oc = open_out output_file in
  List.iter (fun binary -> output_string oc (binary ^ "\n")) binary_instructions;
  close_out oc

