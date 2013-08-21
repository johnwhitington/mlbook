(* Starting Off *)

(* Functions *)
let times_ten x = x * 10

let both_non_zero x y =
  x <> 0 && y <> 0

let rec sum n =
  if n = 1 then 1 else n + sum (n - 1)

let rec power x n =
  if n = 0 then 1 else
    (if n = 1 then x else
       x * power x (n - 1)

let isvowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

let isconsonant c =
  not (isvowel c)

let rec factorial x =
  if x < 0 then 0 else
    if x = 1 then 1 else
      x * factorial (x - 1)

(* Case by Case *)
let not x =
  match x with
    true -> false
  | false -> true

let rec sum_match n =
  match n with
    1 -> 1
  | _ -> n + sum_match (n - 1)

let power_match x n =
  match n with
    0 -> 1
  | 1 -> x
  | _ -> x * power_match x (n - 1)

let isupper c =
  match c with
    'A'..'Z' -> true
  | _ -> false

let islower c =
  match c with
    'a'..'z' -> true
  | _ -> false

let islower c =
  not (isupper c)

(* Listing things *)
let rec even_elements l =
  match l with
    [] -> []
  | [a] -> []
  | _::b::t -> b::even_elements t

let rec even_elements l =
  match l with
    _::b::t -> b::even_elements t
  | l -> []

let rec count_true l =
  match l with
    [] -> 0
  | true::t -> 1 + count_true t
  | false::t -> count_true t

let rec count_true_inner n l =
  match l with
    [] -> n
  | true::t -> count_true_inner (n + 1) t
  | false::t -> count_true_inner n t

let count_true l =
  count_true_inner 0 l

let mk_palindrome l = l @ rev l

let is_palindrome l = (l = rev l)

let rec droplast l =
  match l with
    [] -> []
  | [x] -> []
  | h::t -> h :: droplast t

let rec droplast_inner a l =
  match l with
    [] -> rev a
  | [x] -> rev a
  | h::t -> droplast_inner (h :: a) t

let droplast l =
  droplast_inner [] l

let rec exists e l =
  match l with
    [] -> false
  | h::t -> h = e || exists e t

let rec make_set l =
  match l with
    [] -> []
  | h::t -> if exists h t then make_set t else h :: make_set t

let rev_inner a l =
  match l with
    [] -> a
  | h::t -> rev_inner (h :: a) t

let rev l = rev_inner [] l

(* Sorting *)
let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
      let x = length l / 2 in
        let left = take x l
        and right = drop x l in
          merge (msort left) (msort right)

let rec insert x l =
  match l with
    [] -> [x]
  | h::t ->
      if x >= h
        then x :: h :: t
        else h :: insert x t

let rec sort l = match l with
     [] -> []
   | h::t -> insert h (sort t)

let rec is_sorted l =
  match l with
    [] -> true
  | [x] -> true
  | a::b::t -> a <= b && is_sorted (b :: t)
 
let rec is_sorted l =
  match l with
    a::b::t -> a <= b && is_sorted (b :: t)
  | _ -> true

let rec sort l =
  let rec insert x s =
    match s with
      [] -> [x]
    | h::t ->
        if x <= h
          then x :: h :: t
          else h :: insert x t
  in
    match l with
      [] -> []
    | h::t -> insert h (sort t)

(* Chapter 6: Functions upon Functions upon Functions *)
let rec calm l =
  match l with
    [] -> []
  | '!'::t -> '.'::calm t
  | h::t -> h::calm t

let calm_char x =
  match x with '!' -> '.' | _ -> x

let calm l =
  map calm_char l

let clip x =
  if x < 0 then 0 else
    if x > 10 then 10 else x

let cliplist =
  map clip l

let cliplist l =
  map
    (fun x ->
       if x < 0 then 0 else
         if x > 10 then 10 else x)
    l

let rec apply f n x =
  if n = 0
    then x
    else f (apply f (n - 1) x)

let power a b =
  apply (fun x -> x * a) b 1

let rec insert f x l =
  match l with
    [] -> [x]
  | h::t ->
      if f x h
        then x::h::t
        else h::insert f x t

let rec sort f l =
  match l with
    [] -> []
  | h::t -> insert f h (sort f t)

let rec filter f l =
  match l with
    [] -> []
  | h::t ->
     if f h
       then h :: filter f t
       else filter f t

let rec for_all f l = 
  match l with 
    [] -> true
  | h::t -> f h && for_all f t

let rec mapl f l =
  match l with
    [] -> []
  | h::t -> map f h :: mapl f t

(* Chapter 7 *)
let rec smallest_inner current found l =
  match l with
    [] ->
      if found then current else raise Not_found 
  | h::t ->
      if h > 0 && h < current
        then smallest_inner h true t
        else smallest_inner current found t

let smallest l =
  smallest_inner max_int false l

let smallest_or_zero l =
  try smallest l with Not_found -> 0

let rec sqrt_inner x n =
  if x * x > n then x - 1 else sqrt_inner (x + 1) n

exception Complex

let sqrt n =
  if n < 0 then raise Complex else
    sqrt_inner 1 n

let safe_sqrt n =
  try sqrt n with Complex -> 0
  
(* Chapter 8 *)
let dictlength l = length l

let rec replace k v l =
  match l with
    [] -> raise (Invalid_argument "replace")
  | (k', v')::t ->
      if k = k'
        then (k, v) :: t
        else (k', v') :: replace k v t

let rec mkdict keys values =
  match keys, values with
    [], [] -> []
  | _, [] -> raise (Invalid_argument "mkdict")
  | [], _ -> raise (Invalid_argument "mkdict")
  | k::ks, v::vs -> (k, v) :: mkdict ks vs

let rec mklists l =
  match l with
    [] -> ([], [])
  | (k, v)::more ->
      match mklists more with
        (ks, vs) -> (k :: ks, v :: vs)

let rec mklists l =
  match l with
    [] -> ([], [])
  | (k, v)::more ->
      let (ks, vs) = mklists more in
        (k :: ks, v :: vs)

let rec union a b =
  match a with
    [] -> b
  | (k, v)::t -> add k v (union t b)

let rec member x l =
  match l with
    [] -> false
  | h::t -> x = h || member x t

let rec dictionary_of_pairs_inner keys_seen l =
  match l with
    [] -> []
  | (k, v)::t ->
      if member k keys_seen
        then dictionary_of_pairs_inner keys_seen t
        else (k, v) :: dictionary_of_pairs_inner (k :: keys_seen) t

let dictionary_of_pairs l =
  dictionary_of_pairs_inner [] l

(* Chapter 9. More with functions *)
let member_all x ls =
  not (member false (map (member x) ls))

let reverse_subtract a b = b - a

let mapll f l = map (map (map f)) l

let mapll f = map (map (map f))

let truncate_l n l =
  if length l >= n then take n l else l

let truncate_l n l =
  try take n l with Invalid_argument "take" -> l 

let truncate n ll =
  map (truncate_l n) ll

let firstelt n l =
  match l with
    [] -> n
  | h::_ -> h

let firstelts n l =
  map (firstelt n) l

(* New Kinds of Data *)
type rect =
    Square of int
  | Rectangle of int * int (* width, height *)

let area r =
  match r with
    Square s -> s * s
  | Rectangle (w, h) -> w * h

let rotate r =
  match r with
    Rectangle (w, h) ->
      if w > h then Rectangle (h, w) else Rectangle (w, h)
  | _ -> r

let width_of_rect r =
  match r with
    Square s -> s
  | Rectangle (w, _) -> w

let rect_compare a b =
  width_of_rect a < width_of_rect b

let pack rs =
  sort rect_compare (map rotate rs)

let take n l =
  if n = 0 then Nil else
    match l with
      Nil -> raise (Invalid_argument "take")
    | Cons (h, t) -> Cons (h, take (n - 1) t)

let drop n l =
  if n = 0 then l else
    match l with
      Nil -> raise (Invalid_argument "drop")
    | Cons (_, l) -> drop (n - 1) l

let map f l =
  match l with
    Nil -> Nil
  | Cons (h, t) -> Cons (f h, map f t)

type expr =
    Num of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Multiply of expr * expr
  | Divide of expr * expr
  | Power of expr * expr

let rec evaluate e =
  match e with
    Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Multiply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e'
  | Power (e, e') -> pow (evaluate e) (evaluate e')

let evaluate_opt e =
  try Some (evaluate e) with Division_by_zero -> None

(* Trees *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree 

let rec member x tr =
  match tr with
    Lf -> false
  | Br (y, l, r) -> x = y || member x l || member x r

let rec equal_shape tr tr2 =
  match tr, tr2 with
    Lf, Lf -> true
  | Br (_, l, r), Br (_, l2, r2) -> equal_shape l l2 && equal_shape r r2
  | _, _ -> false

let rec equal_shape tr tr2 =
  map_tree (fun _ -> 0) tr = map_tree (fun _ -> 0) tr2

let rec flip_tree tr =
  match tr with
    Lf -> Lf
  | Br (x, l, r) -> Br (x, flip_tree r, flip_tree l)

type 'a mtree = Branch of 'a * 'a mtree list

let rec size tr =
  match tr with
    Branch (e, l) -> 1 + sum (map size l)

let rec total tr =
  match tr with
    Branch (e, l) -> e + sum (map total l)

let rec map_mtree f tr =
  match tr with
    Branch (e, l) -> Branch (f e, map (map_mtree f) l)

let rec size (Branch (e, l)) =
  1 + sum (map size l)

let rec total (Branch (e, l)) =
  e + sum (map total l)

let rec map_mtree f (Branch (e, l)) =
  Branch (f e, map (map_mtree f) l)

let rec tree_of_list l =
  match l with
    [] -> Lf
  | (k, v)::t -> insert (tree_of_list t) k v

let tree_union t t' =
  tree_of_list (list_of_tree t' @ list_of_tree t)

(* 12. In and out *)
let print_integers l =
  print_string "[";
  iter (fun i -> print_int i; print_string "; ") l;
  print_string "]"

let rec print_integers_inner l =
  match l with
    [] -> ()
  | [i] -> print_int i
  | h::t -> print_int h; print_string "; "; print_integers_inner t

let print_integers l =
  print_string "[";
  print_integers_inner l;
  print_string "]"

let rec read_three () =
  try
    print_string "Please type in three integers, pressing enter after each\n";
    let x = read_int () in
      let y = read_int () in
        let z = read_int () in
          (x, y, z)
  with
    Failure "int_of_string" ->
      print_string "failed to read integers; please try again\n";
      read_three ()

let rec read_dict_number n =
  if n = 0 then [] else
    try
      let i = read_int () in
        let name = read_line () in
          (i, name)::read_dict_number (n - 1)
    with
      Failure "int_of_string" ->
        print_string "This is not a valid integer -- please enter integer and name again";
        print_newline ();
        read_dict_number n

exception BadNumber

let rec read_dict () =
  print_string "How many dictionary entries to input?\n";
  try
    let n = read_int () in
      if n < 0 then raise BadNumber else read_dict_number n
  with
    Failure "int_of_string" ->
      print_string "Not a number. Try again\n"; read_dict ()
  | BadNumber ->
      print_string "Number is negative. Try again\n"; read_dict ()

(* Disc full, file name not suitable, etc. *)
let rec numlist n =
  match n with
    0 -> []
  | _ -> (numlist (n - 1)) @ [n]  

let write_table_channel ch n =
  iter
    (fun x ->
       iter
          (fun i -> output_string ch (string_of_int i); output_string ch "\t")
          (map (( * ) x) (numlist n));
       output_string ch "\n")
    (numlist n)

exception FileProblem

let table filename n =
  if n < 0 then raise (Invalid_argument "table") else
    try
      let ch = open_out filename in
        write_table_channel ch n;
        close_out ch
    with
      _ -> raise FileProblem

let rec countlines_channel ch =
  try
    let _ = input_line ch in
      1 + countlines_channel ch
  with
    End_of_file -> 0

let countlines file =
  try
    let ch = open_in file in
      let result = countlines_channel ch in
        close_in ch;
        result
  with
    _ -> raise (Failure "countlines")

let rec copy_file_ch from_ch to_ch =
  try
    output_string to_ch (input_line from_ch);
    output_string to_ch "\n";
    copy_file_ch from_ch to_ch
  with
    End_of_file -> ()

exception CopyFailed

let copy_file from_name to_name =
  try
    let from_ch = open_in from_name in
      let to_ch = open_out to_name in
        copy_file_ch from_ch to_ch
  with
    _ -> raise CopyFailed

(* Putting things in boxes *)
let rec forloop f n m =
  if n <= m then
    begin
      f n;
      forloop f (n + 1) m
    end

let array_sum a =
  let sum = ref 0 in
    for x = 0 to Array.length a - 1 do
      sum := !sum + a.(x)
    done;
    !sum

let array_rev a =
  for x = 0 to Array.length a / 2 do
    let t = a.(x) in
      a.(x) <- a.(Array.length a - 1 - x);
      a.(Array.length a - 1 - x) <- t
  done

let table n =
  let a = Array.make n [||] in
    for x = 0 to n - 1 do
      a.(x) <- Array.make n 0
    done;
    for y = 0 to n - 1 do
      for x = 0 to n - 1 do
        a.(x).(y) <- (x + 1) * (y + 1)
      done
    done;
    a

let uppercase x =
  if int_of_char x >= 97 && int_of_char x <= 122
    then char_of_int (int_of_char x - 32)
    else x

let lowercase x =
  if int_of_char x >= 65 && int_of_char x <= 90
    then char_of_int (int_of_char x + 32)
    else x

(* Getting real. *)
let round x =
  let c = ceil x in
    let f = floor x in
      if c -. x <= x -. f then c else f

let between (x, y) (x', y') =
  (x +. x') /. 2., (y +. y') /. 2.

let rec parts x =
  if x < 0. then
    let a, b = parts (~-. x) in
      (~-.a, b)
  else
    (floor x, x -. floor x)

let star x =
  let i = int_of_float (floor (x *. 50.)) in
    let i' = if i = 50 then 49 else i in
      for x = 1 to i' - 1 do print_char ' ' done;
      print_char '*';
      print_newline ()

let plot f a b dy =
  let pos = ref a in
    while !pos < b do
      star (f !pos);
      pos := !pos +. dy 
    done

(* The OCaml Standard Library *)
let rec concat l =
  match l with
    [] -> []
  | h::t -> h @ concat t

let rec concat_tail a l =
  match l with
    [] -> List.rev a
  | h::t -> concat_tail (List.rev h @ a) t

let concat l =
  concat_tail [] l

let all_contain_true l =
  match l with
    [] -> false
  | x -> List.mem true (List.map (List.mem true) x)

let count_exclamations s =
  let n = ref 0 in
    String.iter (function '!' -> n := !n + 1 | _ -> ()) s;
    !n

let calm =
  String.map (function '!' -> '.' | x -> x)

let concat =
  String.concat ""

let concat ss =
  let b = Buffer.create 100 in
    List.iter (Buffer.add_string b) ss;
    Buffer.contents b

let occurrences ss s =
  if ss = "" then 0 else
    let num = ref 0 in
      let str = ref s in
        while String.length ss <= String.length !str && !str <> "" do
          if String.sub !str 0 (String.length ss) = ss then num := !num + 1;
          str := String.sub !str 1 (String.length !str - 1)
        done;
        !num


