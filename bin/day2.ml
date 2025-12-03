let split_on_comma = String.split_on_char ','

let split_on_dash = String.split_on_char '-'

(*
Source - https://stackoverflow.com/a
Posted by gasche, modified by community. See post 'Timeline' for change history
Retrieved 2025-12-03, License - CC BY-SA 3.0
*)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let (<<) f g x = f(g(x))

let log10_int = int_of_float << log10 << float_of_int

let is_dupe s =
  let l = (String.length s) in
  if l mod 2 = 1 then false
  else String.sub s (l / 2) (l / 2) = String.sub s 0 (l / 2)



let is_exp n =
  let l = log10_int n + 1 in
  let rec aux i m d=
    if i  > l / 2 then None
    else
    if l mod i = 0 then
      let e = (pow 10 i) in
      if m = 0 then Some d
      else
        if m mod e = d then
          aux i (m / e) d
        else
          aux (i + 1) n (n mod (pow 10 (i + 1)))
    else
      aux (i + 1) n (n mod (pow 10 (i + 1))) in
    aux 1 n (n mod 10)



let rec has_dupe lower upper acc =
  if lower > upper then
    acc
  else
    if Option.is_some @@ is_exp lower then has_dupe (lower + 1) upper (lower::acc)
    else has_dupe (lower + 1) upper acc

let sol s =
  let words = split_on_comma s in
  let ids_str = List.map split_on_dash words in
  let ids_ints = List.map (List.map int_of_string) ids_str in
  let bounds = List.map (function
  | a::b::_ -> Some (a,b)
  | _ -> None) ids_ints in
  let dupe_lists = List.map (Option.map (fun (x,y) -> has_dupe x y [])) bounds in
  List.fold_left (+) 0 (List.fold_left (fun acc v -> match v with | None -> acc | Some v -> v @ acc) [] dupe_lists)

let _ = print_int (sol "9191906840-9191941337,7671-13230,2669677096-2669816099,2-12,229599-392092,48403409-48523311,96763-229430,1919163519-1919240770,74928-96389,638049-668065,34781-73835,736781-819688,831765539-831907263,5615884-5749554,14101091-14196519,7134383-7169141,413340-625418,849755289-849920418,7745350-7815119,16717-26267,4396832-4549887,87161544-87241541,4747436629-4747494891,335-549,867623-929630,53-77,1414-3089,940604-1043283,3444659-3500714,3629-7368,79-129,5488908-5597446,97922755-98097602,182-281,8336644992-8336729448,24-47,613-1077")


