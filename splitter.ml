(* Split pandoc HTML output into chapters *)
open Soup

let soup = read_file "ocamlfromtheverybeginning.html" |> parse

let body_contents = soup $$ "body" |> R.first |> children

let _ = Printf.printf "Found %i things inside the body\n" (count body_contents)

let filename = ref 0

(*let _ = Printf.printf (to_string (first body_nodes))*)
