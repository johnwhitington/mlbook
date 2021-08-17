(* Split pandoc HTML output into chapters *)
open Soup

let soup = read_file "ocamlfromtheverybeginning.html" |> parse

let body_contents = soup $$ "body" |> R.first |> children

let filenum = ref 0

let open_a_file () =
  Printf.printf "Writing html/%02i.html\n%!" !filenum;
  open_out_bin (Printf.sprintf "html/%02i.html" !filenum)

let the_open_file = ref (open_a_file ())

let () =
  iter
    (fun node ->
       if 
         match element node with
         | Some e -> name e = "h1"
         | _  -> false
       then
         begin
           filenum := !filenum + 1;
           close_out !the_open_file;
           the_open_file := open_a_file ()
         end;
       output_string !the_open_file (to_string node))
    body_contents
