open Core
open Caesar

let () =
  let args = Sys.get_argv () |> Array.to_list in
  let file = List.nth_exn args 1 in
  let contents = In_channel.read_all file in
  let command = List.nth args 2 in
  let shift =
    match List.nth args 3 with Some x -> Int.of_string x | None -> 0
  in
  let result =
    match command with
    | Some "encrypt" -> Cipher.encrypt ~shift contents
    | Some "decrypt" -> Cipher.decrypt ~shift contents
    | _ -> "Invalid input the only two commands are encrypt and decrypt"
  in
  let flag =
    match List.nth args 4 with
    | Some "apply" -> Out_channel.write_all file ~data:result
    | Some x ->
        let error_msg = "Unknown command " ^ x ^ " did you mean apply?\n" in
        print_string error_msg
    | None -> ()
  in
  flag;
  print_string result
