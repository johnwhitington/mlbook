(* Split pandoc HTML output into chapters using lambdasoup/markup.ml *)
open Soup

let soup = read_file "ocamlfromtheverybeginning.html" |> parse

let _ =
  match soup $ "generator" |> leaf_text with
  | Some s -> print_endline s
  | _ -> ()
