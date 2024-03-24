let rec fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun f lst accu ->
  match lst with
  | [] -> accu
  | hd::tl -> f hd (fold_right f tl accu)
  
type value =
  | Int of int 
  | List of value list;;

let s1 = List [Int 1; Int 2; Int 3];;
let s2 = List [Int 4; Int 5; Int 6];;

s1;;

match s1, s2 with
| List s1, List s2 -> List (s1 @ s2)
| _ -> failwith "Invalid input"
