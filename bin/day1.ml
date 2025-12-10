let string_to_char_list s = List.of_seq @@  String.to_seq s
let string_of_char_list w = String.of_seq @@ List.to_seq w


let eatLine s = match string_to_char_list s with
  | 'L'::w -> Some (- (int_of_string @@ string_of_char_list w))
  | 'R'::w -> Some (int_of_string @@ string_of_char_list  w)
  | _ -> None

let moves l = List.fold_left (fun (acc,j) ->
  function
  | Some v -> (
    (acc + v) mod 100,
    if (acc + v) mod 100 = 0 then j + 1 else j
    )
  | None -> (acc,j)) (50,0)  l

let aux (a,i) v =
  let v = Option.value v ~default:0 in
  let turns = v / 100 in
  let rest = v mod 100 in
  let n = a + rest mod 100 in
  let x = turns + (a + rest) / 100 in
  (n, i + x)


let moves_day2 l = List.fold_left aux (50,0) l



(*
Source - https://stackoverflow.com/a
Posted by ivg, modified by community. See post 'Timeline' for change history
Retrieved 2025-12-05, License - CC BY-SA 3.0
*)

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let _ =
  let (_, j) = read_lines "./bin/input.txt" |> List.map (eatLine) |> moves_day2 in
  print_int j
