open Core

let encrypt msg ~shift:x =
  let arg = String.to_list msg in
  let chrs_to_ints = List.map ~f:Char.to_int arg in
  let shifter1 x n = ((n - 97 + x) mod 26) + 97 in
  let shifter2 x n = ((n - 65 + x) mod 26) + 65 in
  let shift1_or_shift2 x n =
    match n with
    | n when n >= 97 && n <= 122 -> shifter1 x n
    | n when n >= 65 && n <= 90 -> shifter2 x n
    | _ -> n
  in
  let shifted = List.map ~f:(shift1_or_shift2 x) chrs_to_ints in
  let ints_to_chrs = List.map ~f:Char.of_int_exn shifted in
  String.of_list ints_to_chrs

let decrypt msg ~shift:x =
  let arg = String.to_list msg in
  let chrs_to_ints = List.map ~f:Char.to_int arg in
  let shifter1 x n = ((n - 97 - x + 26) mod 26) + 97 in
  let shifter2 x n = ((n - 65 - x + 26) mod 26) + 65 in
  let shift1_or_shift2 x n =
    match n with
    | n when n >= 97 && n <= 122 -> shifter1 x n
    | n when n >= 65 && n <= 90 -> shifter2 x n
    | _ -> n
  in
  let shifted = List.map ~f:(shift1_or_shift2 x) chrs_to_ints in
  let ints_to_chrs = List.map ~f:Char.of_int_exn shifted in
  String.of_list ints_to_chrs
