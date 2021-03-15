module Sexp = Sexplib0.Sexp
module Csexp = Csexp.Make (Sexp)
module Ocf = Ocamlformat_rpc_lib
open Util.Result.Infix

let current_client = ref None

let supported_versions = [ "v1" ]

let start () =
  let prog = "/home/gpe/ocamlformat-rpc.exe" in
  let argv = [ "ocamlformat-rpc" ] in
  let stdin, in_ = Unix.pipe () in
  let out_, stdout = Unix.pipe () in
  Unix.set_close_on_exec in_;
  let pid =
    Spawn.spawn ~prog ~argv ~stdin ~stdout
      ~stderr:(Unix.descr_of_out_channel stderr)
      ()
  in
  let input = Unix.in_channel_of_descr out_ in
  let output = Unix.out_channel_of_descr in_ in
  Ocf.pick_client ~pid input output supported_versions >>| fun client ->
  current_client := Some client;
  client

let try_format_as_list ?(toplevel = false) l cl =
  let split_lines = Astring.String.cuts ~sep:"\n" ~empty:true in
  let chop_semisemi x =
    match Astring.String.cut ~sep:"\n" x with Some (";;", x) -> x | _ -> x
  in
  let whole = Astring.String.concat ~sep:"\n" l in
  if toplevel then
    match Ocf.format whole cl with
    | exception _ -> l
    | Error _ -> l
    | Ok fmted -> (
        match List.rev (split_lines (chop_semisemi fmted)) with
        | "" :: x :: r | x :: r -> List.rev ((x ^ ";;") :: r)
        | [] -> failwith "command are not empty")
  else
    match Astring.String.cuts ~sep:";;" whole with
    | [] -> []
    | [ x ] -> (
        match Ocf.format x cl with
        | exception _ -> l
        | Error _ -> l
        | Ok fmted -> (
            match List.rev (split_lines fmted) with
            | "" :: r -> List.rev r
            | x :: r -> List.rev (x :: r)
            | [] -> failwith "command are not empty"))
    | phrases ->
        let fmted =
          List.map
            (fun phrase ->
              if Astring.String.(is_empty (trim phrase)) then []
              else
                match Ocf.format phrase cl with
                | exception _ ->
                    Astring.String.cuts ~sep:"\n" ~empty:true (phrase ^ ";;")
                | Error _ ->
                    Astring.String.cuts ~sep:"\n" ~empty:true (phrase ^ ";;")
                | Ok fmted -> (
                    match List.rev (split_lines (chop_semisemi fmted)) with
                    | [ ""; x1 ] -> [ x1 ^ ";;" ]
                    | "" :: x :: r -> List.rev ((x ^ ";;") :: r)
                    | x :: r -> List.rev ((x ^ ";;") :: r)
                    | [] -> failwith "command are not empty"))
            phrases
        in
        List.concat fmted

let get_client () =
  match !current_client with
  | None -> start ()
  | Some cl ->
      let i, _ = Unix.waitpid [ WNOHANG ] (Ocf.pid cl) in
      if i = 0 then Ok cl else start ()

let halt () =
  match get_client () >>= Ocf.halt with (exception _) | Error _ | Ok () -> ()

let try_config x =
  match get_client () >>= Ocf.config x with
  | (exception _) | Error _ | Ok () -> ()

let try_format_as_list ?toplevel x =
  match get_client () with
  | (exception _) | Error _ -> x
  | Ok cl -> try_format_as_list ?toplevel x cl
