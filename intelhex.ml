(* Intelhex *)

type intelhex_line =
| Data of int * string
| EOF
| ELAR of int
| ESAR of int
| SSAR of int32
| Unknown of int

let sf = Printf.sprintf

let hex_to_digit = function
  | '0'..'9' as c -> (Char.code c) land 15
  | 'a'..'f' as c -> (Char.code c) - 87
  | 'A'..'F' as c -> (Char.code c) - 55
  | _ -> invalid_arg "Bad hex digit"

let decode_bytes u =
  let m = String.length u in
  let n = (m - 1) / 2 in
  let v = String.create n in
  for i = 0 to n - 1 do
    v.[i] <-
      Char.chr
        (((hex_to_digit u.[2 * i + 1]) lsl 4) lor
          (hex_to_digit u.[2 * i + 2]))
  done;
  v

let ( !!! ) = Int32.of_int
let ( +++ ) = Int32.add
let ( ||| ) = Int32.logor
let ( &&& ) = Int32.logand
let ( <<< ) = Int32.shift_left

let decode_intelhex_line u =
  let m = String.length u in
  if m < 11 or u.[0] <> ':' then
    invalid_arg "Bad intelhex line"
  else
    begin
      let v = decode_bytes u in
      let n = String.length v in
      let g = Char.code in
      let h x = Int32.of_int (g x) in
      let count = g v.[0] in
      if count + 5 <> n then invalid_arg "Bad count";
      let offset = ((g v.[1]) lsl 8) + (g v.[2]) in
      let rectyp = g v.[3] in
      let cksum = ref 0 in
      for i = 0 to n - 1 do
        cksum := !cksum + g v.[i];
      done;
      if (!cksum land 255) <> 0 then invalid_arg (sf "Bad checksum, got 0x%02x" !cksum);
      match rectyp with
      | 0x00 -> Data(offset, String.sub v 4 (n - 5))
      | 0x01 -> EOF
      | 0x02 -> ESAR(((g v.[4]) lsl 8) lor (g v.[5]))
      | 0x03 -> SSAR(((h v.[4]) <<< 24) ||| ((h v.[5]) <<< 16) ||| ((h v.[6]) <<<  8) ||| (h v.[7]))
      | 0x04 -> ELAR(((g v.[4]) lsl 8) lor (g v.[5]))
      | _ -> Unknown rectyp
    end

let iter_over_intelhex_file fn f =
  let ic = open_in_bin fn in
  try
    let base = ref 0l in
    while true do
      let l = input_line ic in
      match decode_intelhex_line l with
      | Data(offset, d) ->
          let address = !base +++ (!!! offset) in
          f address d
      | EOF -> raise End_of_file
      | SSAR a ->
          Printf.printf "Warning: ignoring SSAR 0x%08lx\n%!" a
      | ELAR a -> base := ((!!! a) <<< 16) ||| (!base &&& 0xffffl)
      | ESAR o -> base := (!!! o) <<< 4
      | Unknown rt ->
          Printf.printf "Warning: unknown record type 0x%02x\n%!" rt
    done
  with
  | End_of_file -> close_in ic
