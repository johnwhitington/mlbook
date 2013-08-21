(* Chapter 2 *)
let x = 200

let x = 200 in x * x * x

let x = 200 in (let y = x * x in x + y)

let cube x = x * x * x

let neg x = if x < 0 then true else false

let neg x = x < 0

let isvowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

let addtoten a b = (a + b = 10)

let rec factorial a =
  if a = 1 then 1 else a * factorial (a - 1)

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let not x =
  if x then false else true

(* Chapter 3 *)
let rec factorial a =
  match a with
    1 -> a
  | _ -> a * factorial (a - 1)

let isvowel c =
  match c with
    'a' -> true
  | 'e' -> true
  | 'i' -> true
  | 'o' -> true
  | 'u' -> true
  | _ -> false

let isvowel c =
  match c with
   'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false

let rec gcd a b =
  match b with
   '0' -> a
  | _ -> gcd b (a mod b)

(* Chapter 4 *)
let isnil l =
  match l with
    [] -> true
  | _ -> false

let rec length l =
  match l with
    [] -> 0
  | h::t -> 1 + length t

let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t

let rec sum l =
  match l with
    [] -> 0
  | h::t -> h + sum t

let rec length' l n =
  match l with
    [] -> n
  | h::t -> length' t (n + 1)

let length l = length' l 0

let rec odd_elements l =
  match l with
    [] -> []
  | [a] -> [a]
  | a::_::t -> a :: odd_elements t

let rec odd_elements l =
  match l with
    a::_::t -> a :: odd_elements t
  | _ -> l

let rec append a b =
  match a with
    [] -> b
  | h::t -> h :: append t b
 
let rec rev l =
  match l with
    [] -> []
  | h::t -> rev t @ [h]

let rec take n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t

(* Chapter 5 *)
let rec insert x l =
  match l with
    [] -> [x]
  | h::t ->
      if x <= h
        then x::h::t
        else h::insert x t

let rec sort l =
  match l with
    [] -> []
  | h::t -> insert h (sort t)

let rec merge x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
      if hx < hy
        then hx::merge tx (hy::ty)
        else hy::merge (hx::tx) ty

(* from earlier *)
let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h::take (n - 1) t

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t

let rec length x =
  match x with
    [] -> 0
  | _::t -> 1 + length t
(* end from earlier *)

let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
      let left = take (length l / 2) l
      and right = drop (length l / 2) l in
        merge (msort left) (msort right)

(* Chapter 6 *)
let rec double l =
  match l with
    [] -> []
  | h::t -> (h * 2) :: double t

let rec evens l =
  match l with
    [] -> []
  | h::t -> (h mod 2 = 0) :: double t

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let is_even x =
  x mod 2 = 0

let evens l =
  map (fun x -> x mod 2 = 0) l

let greater a b =
  a >= b

let rec merge cmp x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
      if cmp hx hy
        then hx :: merge cmp tx (hy :: ty)
        else hy :: merge xmp (hx :: tx) ty

let rec msort cmp l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
      let left = take (length l / 2) l in
        let right = take (length l / 2) l in
          merge cmp (msort cmp left) (msort cmp right)

(* Chapter 7 *)
let rec take n l =
  match l with
    [] ->
      if n = 0
        then []
        else raise (Invalid_argument "take")
  | h::t ->
      if n < 0 then raise (Invalid_argument "take") else
        if n = 0 then [] else h :: take (n - 1) t

let rec drop n l =
  match l with
    [] ->
      if n = 0
       then []
       else raise (Invalid_argument "drop")
  | h::t ->
      if n < 0 then raise (Invalid_argument "drop") else
        if n = 0 then l else drop (n - 1) t

let safe_divide x y =
  try x / y with
    Division_by_zero -> 0

let last l =
  match l with
    [x] -> x
  | _::t -> last t

let last l =
  match l with
    [] -> raise Not_found
  | [x] -> x
  | _::t -> last t

(* Chapter 8 *)
let p = (1, 4)

let q = (1, '1')

let fst p = match p with (x, y) -> x

let snd p = match p with (x, y) -> y

let fst (x, y) = x

let snd (x, y) = y

let census = [(1, 4); (2, 2); (3, 2); (4, 3); (5, 1); (6, 2)]

let y = (1, [2; 3; 4])

let rec lookup x l =
  match l with
    [] -> raise Not_found
  | (k, v)::t ->
      if k = x then v else lookup x t

let rec add k v l =
  match l with
    [] -> []
  | (k', v')::t ->
      if k = k'
        then t
        else (k', v') :: remove k t

let rec key_exists k l
  try
    let v = lookup k l in true
  with
    Not_found -> false

(* Chapter 9 *)
let add x y = x + y

let rec mapl f l =
  match l with
    [] -> []
  | h::t -> map f h :: mapl f t

let mapl f l = map (map f) l

let mapl f = map (map f)

let add = fun x -> fun y -> x + y

(* Chapter 10 *)
type colour = Red | Green | Blue | Yellow

let col = Blue

let cols = [Red; Red; Green; Yellow]

let colpair = ('R', Red)

type colour =
  Red
| Green
| Blue
| Yellow
| RGB of int * int * int

let cols = [Red; Red; Green; Yellow; RGB (150, 0, 255)]

let components c =
  match c with
    Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)
  | RGB (r, g, b) -> (r, g, b)

let nothing = None

let number = Some 50

let numbers = [Some 12; None; None; Some 2]

let word = Some ['c'; 'a'; 'k'; 'e']

let rec lookup_opt x l =
  match l with
    [] -> None
  | (k, v)::t -> if x = k then Some v else lookup_opt x t

let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t

let rec append a b =
  match a with
    [] -> b
  | h::t -> h :: append t b

let rec length s =
  match s with
    Nil -> 0
  | Cons (_, t) -> 1 + length t

let rec append a b =
  match a with
    Nil -> b
  | Cons (h, t) -> Cons (h, append t b)

type expr =
  Num of int
| Add of expr * expr
| Subtract of expr * expr
| Multiply of expr * expr
| Divide of expr * expr

let rec evaluate e =
  match e with
    Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Multiply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e'

(* Chapter 11. Growing Trees *)
type 'a tree =
    Br of 'a * 'a tree * 'a tree
  | Lf

let rec size tr =
  match tr with
    Br (_, l, r) -> 1 + size l + size r
  | Lf -> 0

let total tr =
  match tr with
    Br (x, l, r) -> x + total l + total r
  | Lf -> 0

let max x y =
  if x > y then x else y

let maxdepth tr =
  match tr with
    Br (_, l, r) -> 1 + max (maxdepth l, maxdepth r)
  | Lf -> 0

let rec list_of_tree tr =
  match tr with
    Br (x, l, r) -> list_of_tree l @ [x] @ list_of_tree r
  | Lf -> []

let tree_map f tr =
  match tr with
    Br (x, l, r) -> Br (f x, tree_map f l, tree_map f r)
  | Lf -> Lf

let rec lookup tr k =
  match tr with
    Lf -> raise Not_found
  | Br ((k', v), l, r) ->
      if k = k' then v
      else if k < k' then lookup l k
      else lookup r k

let rec lookup tr k =
  match tr with
    Lf -> None
  | Br ((k', v), l, r) ->
      if k = k' then Some v
      else if k < k' lookup l k
      else lookup r k

let rec insert tr k v =
  match tr with
    Lf -> Br ((k, v), Lf, Lf)
  | Br ((k', v'), l, r) ->
      if k = k' then Br ((k, v), l, r)
      else if k < k' then Br ((k', v'), insert l k v, r)
      else Br ((k', v'), l, insert r k v)

(* Chapter 12. In and Out *)
(* Write a dictionary to standard output *)
let print_dict_entry (k, v) =
  print_int k;
  print_newline ();
  print_string v;
  print_newline ()

let print_dict l =
  match l with
    [] -> ()
  | h::t -> print_dict_entry h; print_dict t

let rec iter f l =
  match l with
    [] -> ()
  | h::t -> f h; iter f t

let print_dict d =
  iter print_dict_entry d

let print_dict =
  iter print_dict_entry

(* Interactively read a dictionary from standard input *)
let rec read_dict () =
  let i = read_int () in
    if i = 0 then [] else
      let name = read_line () in
        (i, name) :: read_dict ()

let rec read_dict () =
  try
    let i = read_int () in
      if i = 0 then [] else
        let name = read_line () in
          (i, name)::read_dict ()
  with
    Failure "int_of_string" ->
      print_string "This is not a valid integer -- please enter integer and name again";
      print_newline ();
      read_dict ()

(* Write a dictionary to a given channel *)
let entry_to_channel ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'

let dictionary_to_channel ch d =
  iter (entry_to_channel ch) d

(* Write a dictionary to a given file *)
let dictionary_to_file filename dict =
  let ch = open_out filename in
    dictionary_to_channel ch dict;
    close_out ch

(* Read a dictionary from a given channel *)
let entry_of_channel ch =
  let number = input_line ch in
    let name = input_line ch in
      (int_of_string number, name)

let rec dictionary_of_channel ch =
  try
    let e = entry_of_channel ch in
      e :: dictionary_of_channel ch
  with
    End_of_file -> []

(* Read a dictionary from a given file *)
let dictionary_of_file filename =
  let ch = open_in filename in
    let dict = dictionary_of_channel ch in
      close_in ch;
      dict

(* Chapter 13 *)
let swap a b =
  let t = !a in
    a := !b; b := t

let smallest_pow2 x =
  let t = ref 1 in
    while !t < x do
      t := !t 8 2
    done;
    !t

(* Final version of word counter *)
let print_histogram arr =
  print_string "Word frequencies:";
  print_newline ();
  for x = 0 to 255 do
    if arr.(x) > 0 then
      begin
        print_string "For character '";
        print_char (char_of_int x);
        print_string "' (character number ";
        print_int x;
        print_string ") the count is ";
        print_int arr.(x);
        print_string ".";
        print_newline ()
      end
  done

let channel_statistics in_channel =
  let lines = ref 0
  and characters = ref 0
  and words = ref 0
  and sentences = ref 0
  and histogram = Array.make 256 0 in
    try
      while true do
        let line = input_line in_channel in
          lines := !lines + 1;
          characters := !characters + String.length line;
          String.iter
            (fun c ->
               match c with
               '.' | '?' | '!' -> sentences := !sentences + 1
               | ' ' -> words := !words + 1
               | _ -> ())
            line;
          String.iter
            (fun c ->
              let i = int_of_char c in
                histogram.(i) <- histogram.(i) + 1)
            line
      done
    with
      End_of_file ->
        print_string "There were ";
        print_int !lines;
        print_string " lines, making up ";
        print_int !characters;
        print_string " characters with ";
        print_int !words;
        print_string " words in ";
        print_int !sentences;
        print_string " sentences.";
        print_newline ();
        print_histogram histogram

let file_statistics name =
  let channel = open_in name in
    try
      channel_statistics channel;
      close_in channel
    with
      _ -> close_in channel

(* Chapter 14 *)
let make_vector (x0, y0) (x1, y1) =
  (x1 -. x0, y1 -. y0)

let vector_length (x, y) =
  sqrt (x *. x, y *. y)

let offset_point (x, y) (px, py) =
  (px +. x, py +. y)

let scale_to_length l (a, b) =
  let currentlength = vector_length (a, b) in
    if currentlength = 0. then (a, b) else
      let factor = l /. currentlength in
        (a *. factor, b *. factor)


