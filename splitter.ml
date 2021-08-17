(* Split pandoc HTML output into chapters *)
open Soup

let soup = read_file "ocamlfromtheverybeginning.html" |> parse

let body_contents = soup $$ "body" |> R.first |> children

let filenum = ref 0

let open_a_file () =
  let name = Printf.sprintf "html/split%02i.html" !filenum in
    (*Printf.printf "Writing %s\n%!" name;*)
    open_out_bin name

let the_open_file = ref (open_a_file ())

let contents_of_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let header = contents_of_file "html/header.html"
let footer = contents_of_file "html/footer.html"

let copy s = output_string !the_open_file s

let () =
  copy header;
  iter
    (fun node ->
       if 
         match element node with
         | Some e -> name e = "h1"
         | _  -> false
       then
         begin
           filenum := !filenum + 1;
           copy footer;
           close_out !the_open_file;
           the_open_file := open_a_file ();
           copy header
         end;
       copy (to_string node))
    body_contents;
    copy footer;
    close_out !the_open_file
