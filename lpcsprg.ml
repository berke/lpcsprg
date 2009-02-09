(* Lpcsprg *)
(* Utility for programming Philips LPC21xx-type ARM CPUs using the serial protocol. *)
(* By Berke Durak - http://lambda-diode.com/contact *)
(* Released under the GNU General Public Licence, v3 *)

open Unix

module Opt =
  struct
    open Arg;;

    let sao x y = x := Some y;;
    let set_int32 x y = x := (Int32.of_string y);;

    let modem_device = ref "/dev/ttyS1";;
    let baud = ref 38400;;
    let clock = ref 14.7456e6;;
    let start = ref 0x00000000l;;
    let sector_buffer = ref 0x40001000l;;
    let write_size = ref 4096;;
    let length = ref 256;;
    let synchronize = ref true;;
    let checksum = ref true;;
    let toggle_dtr = ref false;;
    let cache : string option ref = ref None;;

    let commands :
      [`Dump of string * int32 * int | `Program of string | `Info | `Unlock |`Jump_ARM of int32|`Erase of int * int]
      list ref
      =
      ref [];;

    let cmd c = commands := c :: !commands;;

    let spec =
      align [
        "-device",        String( (:=) modem_device ), "<device> set modem device";
        "-baud",          Int( (:=) baud ),            "<rate> baud rate";
        "-start",         String(set_int32 start),     "<address> set start address";
        "-jump-arm",      String(fun x -> cmd (`Jump_ARM(Int32.of_string x))),
                                                       "<address> jump to address in ARM mode";
        "-sector-buffer", String(set_int32 sector_buffer),
                                                       "<address> set sector buffer address in SRAM";
        "-length",        Set_int length,              "<length> set data length, in bytes";
        "-nosyn",         Clear synchronize,           " don't synchronize";
        "-nock",          Clear checksum,              " don't control checksum";
        "-dump",          String(fun fn -> cmd (`Dump(fn, !start, !length))),
                                                  "<file> dump memory contents to file";
        "-program",       String(fun fn -> cmd (`Program fn)),
                                                  "<file> program flash memory with contents of file";
        "-info",          Unit(fun () -> cmd `Info),   " Show device information";
        "-erase",         begin
                            let ss = ref 0 in
                            Tuple[
                              Int(fun x -> ss := x);
                              Int(fun y -> cmd (`Erase(!ss, y)))]
                          end,
                          "<start> <end> Erase sectors";
        "-clock",         Set_float clock,             "<Hz> set device clock";
        "-unlock",        Unit(fun () -> cmd `Unlock), " Unlock device";

        "-cache",         String(fun f -> cache := Some f), "<file> Set cache file";

        "-toggle",        Set toggle_dtr,              " Toggle DTR";
      ]
    ;;
  end
;;

let pf = Printf.printf;;
let fp = Printf.fprintf;;
let sf = Printf.sprintf;;

exception Timeout;;
exception Match_failure;;

type return_code =
  [ `COMMAND_SUCCESS
  | `INVALID_COMMAND
  | `SRC_ADDR_ERROR
  | `DST_ADDR_ERROR
  | `SRC_ADDR_NOT_MAPPED
  | `DST_ADDR_NOT_MAPPED
  | `COUNT_ERROR
  | `INVALID_SECTOR
  | `SECTOR_NOT_BLANK
  | `SECTOR_NOT_PREPARED_FOR_WRITE_OPERATION
  | `COMPARE_ERROR
  | `BUSY
  | `PARAM_ERROR
  | `ADDR_ERROR
  | `ADDR_NOT_MAPPED
  | `CMD_LOCKED
  | `INVALID_CODE
  | `INVALID_BAUD_RATE
  | `INVALID_STOP_BIT
  | `CODE_READ_PROTECTION_ENABLED
  | `RETURN_CODE of int ]
;;

exception Command_failure of return_code;;

let input_char_with_timeout ?(timeout=2.0) fd =
  let w = String.make 1 '\000' in
  match select [fd] [] [] timeout with
  | [fd],[],[] ->
    let n = read fd w 0 1 in
    if n = 0 then
      raise End_of_file
    else
      w.[0]
  | _ -> raise Timeout
;;

let match_string ?(ignore=[]) ?timeout u fd =
  let m = String.length u in
  let rec loop i =
    if i = m then
      ()
    else
      let c = input_char_with_timeout ?timeout fd in
      if List.mem c ignore then
        loop i
      else
        if c = u.[i] then
          loop (i + 1)
        else
          raise Match_failure
  in
  loop 0
;;

exception Synchronization_failure;;

let print_return_code oc = function
| `COMMAND_SUCCESS -> fp oc "COMMAND_SUCCESS"
| `INVALID_COMMAND -> fp oc "INVALID_COMMAND"
| `SRC_ADDR_ERROR -> fp oc "SRC_ADDR_ERROR"
| `DST_ADDR_ERROR -> fp oc "DST_ADDR_ERROR"
| `SRC_ADDR_NOT_MAPPED -> fp oc "SRC_ADDR_NOT_MAPPED"
| `DST_ADDR_NOT_MAPPED -> fp oc "DST_ADDR_NOT_MAPPED"
| `COUNT_ERROR -> fp oc "COUNT_ERROR"
| `INVALID_SECTOR -> fp oc "INVALID_SECTOR"
| `SECTOR_NOT_BLANK -> fp oc "SECTOR_NOT_BLANK"
| `SECTOR_NOT_PREPARED_FOR_WRITE_OPERATION -> fp oc "SECTOR_NOT_PREPARED_FOR_WRITE_OPERATION"
| `COMPARE_ERROR -> fp oc "COMPARE_ERROR"
| `BUSY -> fp oc "BUSY"
| `PARAM_ERROR -> fp oc "PARAM_ERROR"
| `ADDR_ERROR -> fp oc "ADDR_ERROR"
| `ADDR_NOT_MAPPED -> fp oc "ADDR_NOT_MAPPED"
| `CMD_LOCKED -> fp oc "CMD_LOCKED"
| `INVALID_CODE -> fp oc "INVALID_CODE"
| `INVALID_BAUD_RATE -> fp oc "INVALID_BAUD_RATE"
| `INVALID_STOP_BIT -> fp oc "INVALID_STOP_BIT"
| `CODE_READ_PROTECTION_ENABLED -> fp oc "CODE_READ_PROTECTION_ENABLED"
| `RETURN_CODE x -> fp oc "RETURN_CODE(%d)" x
;;

let uu_encode_byte = function
  | 0 -> '`'
  | b -> Char.chr (b + 0x20)
;;

let uu_decode_byte = function
  | '`' -> 0
  | '!'..'_' as c -> (Char.code c) - 0x20
  | _ -> raise Match_failure
;;

let uu_is_valid u =
  let m = String.length u in
  m > 0 &&
    begin
      try
        let n = uu_decode_byte u.[0] in
        let m' = 1 + 4 * ((n + 2) / 3) in
        m = m'
      with
      | Match_failure -> false
    end
;;

(* Encodes one line of data from [u] starting at offset [i] with length [m] into the buffer [b].
 * Returns the number of bytes from [u] encoded. *)

let uu_encode_to_buffer b u i m =
  let f x = Buffer.add_char b (uu_encode_byte x) in
  let o = min 45 m in (* Number of bytes that will be encoded *)
  let p = (o + 2) / 3 in (* Number of groups *)
  let g j =
    if j < m then
      Char.code u.[i + j]
    else
      0
  in
  f o;
  for j = 0 to p - 1 do
    let a0 = g (3 * j + 0) in
    let a1 = g (3 * j + 1) in
    let a2 = g (3 * j + 2) in
    (* 00000011 11112222 22333333 *)
    f (a0 lsr 2);
    f (((a0 land 3) lsl 4) lor (a1 lsr 4));
    f (((a1 land 15) lsl 2) lor (a2 lsr 6));
    f (a2 land 63);
  done;
  o
;;

let uu_decode_to_buffer b u =
  let m = String.length u in
  let f x = Buffer.add_char b (Char.chr x) in
  if m = 0 then
    raise Match_failure
  else
    let n = uu_decode_byte u.[0] in
    let p = (n + 2) / 3 in
    for j = 0 to p - 1 do
      let b0 = uu_decode_byte u.[4 * j + 1]
      and b1 = uu_decode_byte u.[4 * j + 2]
      and b2 = uu_decode_byte u.[4 * j + 3]
      and b3 = uu_decode_byte u.[4 * j + 4]
      in
      (* 00000011 11112222 22333333 *)
      let a1 () =
        f ((b0 lsl 2) lor (b1 lsr 4));
      in
      let a2 () =
        a1 ();
        f (((b1 land 15) lsl 4) lor (b2 lsr 2));
      in
      let a3 () =
        a2 ();
        f (((b2 land 3) lsl 6) lor b3);
      in
      if j = p - 1 then
        begin
          match n mod 3 with
          | 0 -> a3 ()
          | 1 -> a1 ()
          | _ -> a2 ()
        end
      else
        a3 ()
    done;
    n
;;

let uu_test_coding u =
  let b1 = Buffer.create 256 in
  let n = uu_encode_to_buffer b1 u 0 (String.length u) in
  (Buffer.contents b1, n);;

let uu_test_decoding v =
  let b2 = Buffer.create 256 in
  let o = uu_decode_to_buffer b2 v in
  let u = Buffer.contents b2 in
  (u,o)
;;

let compute_checksum u i0 i1 =
  let rec loop q i =
    if i = i1 then
      q
    else
      loop (q + (Char.code u.[i])) (i + 1)
  in
  loop 0 i0
;;

module IM = Map.Make(struct type t = int let compare = compare end);;

let ( !!! ) = Int32.of_int;;
let ( !!? ) = Int32.to_int;;
let ( +++ ) = Int32.add;;
let ( --- ) = Int32.sub;;
let ( ||| ) = Int32.logor;;
let ( &&& ) = Int32.logand;;
let ( <<< ) = Int32.shift_left;;
let ( >>> ) = Int32.shift_right_logical;;

let flash_sector_to_address = function
  | s when 0 <= s && s <=  7 -> !!!(s lsl 12)
  | s when           s <= 21 -> !!!(((s -  8) lsl 15) + 0x0000_8000)
  | s when           s <= 26 -> !!!(((s - 26) lsl 12) + 0x0007_8000)
  | _ -> invalid_arg "Bad sector number"
;;

let sector_size = function
  | x when 8 <= x && x <= 21 -> 32768
  | x when 0 <= x && x <= 26 -> 4096
  | _ -> invalid_arg "Bad sector number"
;;

let address_to_flash_sector a =
  let x = !!? (a >>> 12) in
  match x with
  | _ when 0x00 <= x && x < 0x08 -> x
  | _ when 0x08 <= x && x < 0x78 -> 8 + ((x - 0x08) lsr 3)
  | _ when 0x78 <= x && x < 0x7d -> 26 + (x - 0x78)
  | _ -> invalid_arg (sf "Bad flash address 0x%08lx" a)
;;

let main () =
  Arg.parse Opt.spec (fun x -> Printf.eprintf "Extraneous argument %S ignored.\n%!" x) 
    "Will output data from serial port to stdout.\n";
  let fd = openfile !Opt.modem_device [O_RDWR] 0 in
  let ta = tcgetattr fd in
  let toggle_dtr () =
    let ta = tcgetattr fd in
    let ta' = { ta with c_obaud = 0; c_ibaud = 0 } in
    tcsetattr fd TCSANOW ta';
    ignore (select [] [] [] 0.2);
    tcsetattr fd TCSANOW ta;
    ignore (select [] [] [] 0.2);
  in
  let ta' = { ta with
	      c_istrip = false;
	      c_inlcr = false;
	      c_igncr = false;
	      c_icrnl = false;
	      c_ixoff = false;
	      c_ixon = false;
	      c_obaud = !Opt.baud;
	      c_ibaud = !Opt.baud;
	      c_csize = 8;
	      c_cstopb = 1;
	      c_cread = true;
	      c_parenb = false;
	      c_hupcl = false;
	      c_clocal = true;
	      c_isig = false;
	      c_icanon = false;
	      c_noflsh = true;
	      c_echo = false;
	      c_echoe = false;
	      c_echok = false;
	      c_echonl = false;
	      c_vmin = 1;
	      c_vtime = 0;
	    } 
  in
  Sys.catch_break true;
  tcsetattr fd TCSANOW ta';
  try
    let _ic = in_channel_of_descr fd
    and oc = out_channel_of_descr fd
    in
    let drain ?(duration=0.2) () =
      let t0 = gettimeofday () in
      let t1 = t0 +. duration in
      let rec loop () =
        let t = gettimeofday () in
        if t >= t1 then
          ()
        else
          begin
            ignore (input_char_with_timeout ~timeout:(t1 -. t) fd);
            loop ()
          end
      in
      try
        loop ()
      with
      | Timeout -> ()
    in
    let get_char () = input_char_with_timeout fd in
    let get_ok () = match_string "OK\r\n" fd in
    let send_ok () = fp oc "OK\r\n%!" in
    let put_and_get u =
      let u = u ^ "\r\n" in
      fp oc "%s%!" u;
      match_string u fd
    in
    let match_char c =
      let c' = get_char () in
      if c <> c' then raise Match_failure
    in
    let read_line () =
      let b = Buffer.create 80 in
      let rec loop () =
        let c = get_char () in
        if c = '\r' then
          begin
            match_char '\n';
            let u = Buffer.contents b in
            u
          end
        else
          begin
            Buffer.add_char b c;
            loop ()
          end
      in
      loop ()
    in
    let read_int () =
      let rec loop q = 
        match get_char () with
        | '0'..'9' as c -> loop ((10 * q) + (Char.code c) land 15)
        | '\r' ->
            begin
              match_char '\n';
              q
            end
        | _ -> raise Match_failure
      in
      loop 0
    in
    let read_return_code () =
      match read_int () with
      | 0  -> `COMMAND_SUCCESS
      | 1  -> `INVALID_COMMAND
      | 2  -> `SRC_ADDR_ERROR
      | 3  -> `DST_ADDR_ERROR
      | 4  -> `SRC_ADDR_NOT_MAPPED
      | 5  -> `DST_ADDR_NOT_MAPPED
      | 6  -> `COUNT_ERROR
      | 7  -> `INVALID_SECTOR
      | 8  -> `SECTOR_NOT_BLANK
      | 9  -> `SECTOR_NOT_PREPARED_FOR_WRITE_OPERATION
      | 10 -> `COMPARE_ERROR
      | 11 -> `BUSY
      | 12 -> `PARAM_ERROR
      | 13 -> `ADDR_ERROR
      | 14 -> `ADDR_NOT_MAPPED
      | 15 -> `CMD_LOCKED
      | 16 -> `INVALID_CODE
      | 17 -> `INVALID_BAUD_RATE
      | 18 -> `INVALID_STOP_BIT
      | 19 -> `CODE_READ_PROTECTION_ENABLED
      | x  -> `RETURN_CODE x
    in
    let check_command_rc () =
      match read_return_code () with
      | `COMMAND_SUCCESS -> ()
      | x -> raise (Command_failure x)
    in
    let send_command cmd args =
      fp oc "%s" cmd;
      List.iter
        begin function
          | `Int x     -> fp oc " %d" x
          | `Address x -> fp oc " %lu" x
          | `String x  -> fp oc " %s" x
        end
        args;
      fp oc "\n%!";
      check_command_rc ()
    in
    let uuread b n =
      let rec loop n =
        let u = read_line () in
        if n = 0 or not (uu_is_valid u) then
          begin
            let m = String.length u in
            for i = 0 to m - 1 do
              match u.[i] with
              | '0'..'9' -> ()
              | _ -> raise Match_failure
            done;
            let z = int_of_string u in
            (z, n)
          end
        else
          begin
            let m = String.length u in
            if m = 0 then
              raise Match_failure
            else
              let n' = uu_decode_to_buffer b u in
              loop (n - n')
          end
      in
      loop n
    in
    let f_clock_khz = int_of_float (!Opt.clock /. 1e3) in
    let rec synchronize ?(attempts=30) () =
      if attempts = 0 then
        raise Synchronization_failure
      else
        begin
          fp oc "?%!";
          try
            match_string ~ignore:['?'] ~timeout:0.1 "Synchronized\r\n" fd;
            put_and_get "Synchronized";
            get_ok ();
            put_and_get (sf "%d" f_clock_khz);
            get_ok ();
            put_and_get "A 0";
            check_command_rc ();
          with
          | Timeout -> continue "timeout" attempts
          | Match_failure -> continue "match failure" attempts
        end
    and continue reason attempts =
      pf "Synchronization attempt failed (%s), %d to go\n%!" reason attempts;
      drain ();
      synchronize ~attempts:(attempts - 1) ()
    in

    let dump_memory a n =
      let b = Buffer.create 1024 in
      let b' = Buffer.create 1024 in
      send_command "R" [`Address a; `Int n];
      let rec loop remaining =
        if remaining = 0 then
          Buffer.contents b
        else
          begin
            Buffer.clear b';
            let (cksum, remaining) = uuread b' remaining in
            let v = Buffer.contents b' in
            let cksum' = compute_checksum v 0 (String.length v) in
            if not (!Opt.checksum) or cksum = cksum' then
              begin
                send_ok ();
                Buffer.add_string b v;
                loop remaining
              end
            else
              begin
                Printf.printf "Checksum mismatch %x <> %x, retrying.\n%!" cksum cksum';
                fp oc "RESEND\r\n%!";
                loop n
              end
          end
      in
      loop n
    in

    let send_data data i0 i1 =
      let b = Buffer.create 80 in
      (*Printf.printf "Sending %d bytes from %d to %d\n%!" m i0 i1;*)
      let rec loop i0 line_count i =
        if line_count = 20 or i = i1 then
          begin
            let cksum = compute_checksum data i0 i in
            (*Printf.printf "  Checksum for bytes %d to %d is %d\n%!" i0 i cksum;*)
            fp oc "%d\r\n%!" cksum;
            match read_line () with
            | "RESEND" ->
                Printf.printf "Target requested resend.\n%!";
                loop i0 0 i0
            | "OK" ->
                if i = i1 then
                  ()
                else
                  loop i 0 i
            | _ -> raise Match_failure
          end
        else
          begin
            Buffer.clear b;
            let n = uu_encode_to_buffer b data i (i1 - i) in
            Buffer.add_char b '\n';
            Buffer.output_buffer oc b;
            flush oc;
            loop i0 (line_count + 1) (i + n)
          end
      in
      loop i0 0 i0
    in

    let cache = ref IM.empty in

    let load_cache () =
      match !Opt.cache with
      | None -> ()
      | Some fn ->
          if Sys.file_exists fn then
            begin
              Printf.printf "Loading cache from file %S\n%!" fn;
              let sb = Scanf.Scanning.from_file fn in
              try
                while true do
                  Scanf.bscanf sb "%d %S\n"
                    (fun s digest ->
                      cache := IM.add s digest !cache)
                done;
                assert false
              with
              | End_of_file -> ()
            end
          else
            Printf.printf "Cache file %S does not exist.\n%!" fn;
    in

    let save_cache () =
      match !Opt.cache with
      | None -> ()
      | Some fn ->
          Printf.printf "Saving cache to file %S.\n%!" fn;
          let oc = open_out fn in
          IM.iter
            begin fun s digest ->
              Printf.fprintf oc "%d %S\n" s digest
            end
            !cache;
          close_out oc
    in

    let program_sector s data =
      let m = String.length data in
      let s_a = flash_sector_to_address s in
      let unchanged =
        let digest = Digest.string data in
        if
          try
            let digest' = IM.find s !cache in
            digest = digest'
          with
          | Not_found -> false
        then
          true
        else
          begin
            cache := IM.add s digest !cache;
            false
          end
      in
      if unchanged then
        Printf.printf "Sector %d unchanged.\n%!" s
      else
        begin
          Printf.printf "Preparing sector %d for erasure\n%!" s;
          send_command "P" [`Int s; `Int s];
          Printf.printf "Erasing sector %d\n%!" s;
          send_command "E" [`Int s; `Int s; `Int f_clock_khz];
          let rec loop i =
            if i = m then
              Printf.printf "Done writing sector %d\n%!" s
            else
              begin
                let n = !Opt.write_size in
                Printf.printf "Sector %d offset %d, sending %d bytes to 0x%08lx\n%!" s i n !Opt.sector_buffer;
                send_command "W" [`Address !Opt.sector_buffer; `Int n];
                send_data data i (i + n);
                let f_a = s_a +++ !!! i in
                Printf.printf "Preparing sector %d\n%!" s;
                send_command "P" [`Int s; `Int s];
                Printf.printf "Copying buffer to 0x%08lx\n%!" f_a;
                send_command "C" [`Address f_a; `Address !Opt.sector_buffer; `Int n];
                loop (i + n)
              end
          in
          loop 0
        end
    in

    let program fn =
      Printf.printf "Loading program from file %S\n%!" fn;
      let sectors = ref IM.empty in
      Intelhex.iter_over_intelhex_file fn
        begin fun address data ->
          let rec loop address i =
            if i = String.length data then
              ()
            else
              begin
                let sector = address_to_flash_sector address in
                let sector_start = flash_sector_to_address sector in
                let size = sector_size sector in
                let offset = !!? (address --- sector_start) in
                let sector_data =
                  try
                    IM.find sector !sectors
                  with
                  | Not_found ->
                      let u = String.make size '\000' in
                      sectors := IM.add sector u !sectors;
                      u
                in
                let length = min (String.length data - i) (size - offset) in
                String.blit data i sector_data offset length;
                loop (address +++ !!! length) (i + length)
              end
          in
          loop address 0
        end;
      Printf.printf "Loaded sectors";
      IM.iter
        begin fun s _ ->
          Printf.printf " %d" s
        end
        !sectors;
      Printf.printf "\n%!";

      (* Fix vector checksum *)
      let _ =
        let s = address_to_flash_sector 0x00000000l in
        try
          let data = IM.find s !sectors in
          let f i = !!! (Char.code data.[i]) in
          let g i = ((f (i + 3)) <<< 24) ||| ((f (i + 2)) <<< 16) ||| ((f (i + 1)) <<< 8) ||| (f (i + 0)) in
          let h i x = data.[i] <- Char.chr (!!? (x &&& 255l)) in
          let s i x =
            (h (i + 3) (x >>> 24));
            (h (i + 2) (x >>> 16));
            (h (i + 1) (x >>> 8));
            (h (i + 0)) x
          in
          let sum =
            g 0x00 +++
            g 0x04 +++
            g 0x08 +++
            g 0x0c +++
            g 0x10 +++
            g 0x18 +++
            g 0x1c
          in
          let sum = 0l --- sum in
          Printf.printf "Setting interrupt vector checksum to 0x%08lx\n" sum;
          s 0x14 sum;
        with
        | Not_found ->
          Printf.printf "WARNING: No sector 0, can't fix interrupt vector checksum.\n%!"
      in

      (* Checksum sectors *)
      let _ =
        let sum = ref 0l in
        IM.iter
          begin fun _ data ->
            let f i = !!! (Char.code data.[i]) in
            let g i = ((f (i + 3)) <<< 24) ||| ((f (i + 2)) <<< 16) ||| ((f (i + 1)) <<< 8) ||| (f (i + 0)) in
            let m = String.length data in
            for i = 0 to (m / 4) - 1 do
              sum := !sum +++ (g (4 * i))
            done
          end
          !sectors;
        Printf.printf "Flash checksum is 0x%08lx\n%!" !sum
      in

      IM.iter
        begin fun s data ->
          program_sector s data
        end
        !sectors
    in

    let info () =
      List.iter
        begin function
          | `Unlock ->
              pf "Unlocking device\n%!";
              send_command "U" [`Int 23130]
          | `Dump(fn, start, length) ->
              let u = dump_memory start length in
              let oc = open_out_bin fn in
              output_string oc u;
              close_out oc
          | `Program fn -> load_cache (); program fn; save_cache ()
          | `Jump_ARM a -> send_command "G" [`Address a; `String "A"]
          | `Info ->
              send_command "J" [];
              let device = read_int () in
              pf "Device 0x%08x\n" device;
              send_command "K" [];
              let bootloader_1 = read_int () in
              let bootloader_2 = read_int () in
              pf "Bootloader %d.%d\n" bootloader_1 bootloader_2;
          | `Erase(x,y) ->
              Printf.printf "Prepaing sectors %d to %d\n%!" x y;
              send_command "P" [`Int x; `Int y];
              Printf.printf "Erasing sectors %d to %d\n%!" x y;
              send_command "E" [`Int x; `Int y; `Int f_clock_khz]
        end
        (List.rev !Opt.commands)
    in
    try
      begin
        try
          if !Opt.toggle_dtr then
            begin
              pf "Toggling DTR\n%!";
              toggle_dtr ()
            end;
          if !Opt.synchronize then
            begin
              pf "Synchronizing\n%!";
              synchronize ();
              pf "Synchronized\n"
            end;
          Printf.printf "Setting XON/XOFF flow control.\n%!";
          let ta' =
            { ta' with
              c_ixoff = true;
              c_ixon = true }
          in
          tcsetattr fd TCSANOW ta';
          info ()
        with
        | Synchronization_failure -> pf "Cannot synchronize with LPC.\n"
        | Command_failure rc -> pf "Command failed with code %a\n" print_return_code rc
      end;
      flush oc
    with
    | x -> flush oc; raise x
  with
  | Sys.Break|End_of_file ->
    tcsetattr fd TCSANOW ta;
    close fd
;;

let _ = main ();;
