module Sexp = Sexplib0.Sexp
module Csexp = Csexp.Make (Sexp)
module Ocf = Ocamlformat_rpc_lib
open Util.Result.Infix

type process = {
  pid : int;
  input : in_channel;
  output : out_channel;
  impl : (module Ocf.V);
}

let running_process : process option ref = ref None

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
  let process =
    {
      pid;
      input = Unix.in_channel_of_descr out_;
      output = Unix.out_channel_of_descr in_;
      impl = (module Ocf.V1);
    }
  in
  Ocf.pick_impl process.input process.output supported_versions >>| fun impl ->
  let process = { process with impl } in
  running_process := Some process;
  process

let get_process () =
  match !running_process with
  | None -> start ()
  | Some p ->
      (* Is the process still running ? *)
      let i, _ = Unix.waitpid [ WNOHANG ] p.pid in
      if i = 0 then Ok p else start ()

module Client = struct
  module type S = sig
    val try_config : (string * string) list -> unit

    val try_format : string -> string

    val try_format_as_list : ?toplevel:bool -> string list -> string list

    val halt : unit -> unit
  end

  module V1 = struct
    let query t =
      get_process () >>| fun p ->
      Ocf.V1.output p.output t;
      Ocf.V1.read_input p.input

    let config c =
      match query (`Config c) with
      | Ok (`Config _) -> Ok ()
      | Ok (`Error msg) -> Error (`Msg msg)
      | Ok _ -> Error (`Msg "Unknown error")
      | Error msg -> Error msg

    let format x =
      match query (`Format x) with
      | Ok (`Format x) -> Ok x
      | Ok (`Error msg) -> Error (`Msg msg)
      | Ok _ -> Error (`Msg "Unknown error")
      | Error msg -> Error msg

    let try_format_as_list ?(toplevel = false) l =
      let split_lines = Astring.String.cuts ~sep:"\n" ~empty:true in
      let chop_semisemi x =
        match Astring.String.cut ~sep:"\n" x with Some (";;", x) -> x | _ -> x
      in
      let whole = Astring.String.concat ~sep:"\n" l in
      if toplevel then
        match format whole with
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
            match format x with
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
                    match format phrase with
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

    let halt () =
      match get_process () with
      | exception _ -> running_process := None
      | Ok p ->
          Ocf.V1.output p.output `Halt;
          close_in p.input;
          close_out p.output;
          running_process := None
      | Error _ -> running_process := None

    let try_config c = match config c with exception _ -> () | _ -> ()

    let try_format x =
      match format x with exception _ -> x | Ok x -> x | Error _ -> x
  end
end

let get_client () : (module Client.S) =
  match get_process () with
  | Ok p -> ( match p.impl with (module V1) -> (module Client.V1))
  | Error (`Msg msg) -> failwith msg

let halt () =
  match get_client () with exception _ -> () | (module Impl) -> Impl.halt ()

let try_config x =
  match get_client () with
  | exception _ -> ()
  | (module Impl) -> Impl.try_config x

let try_format x =
  match get_client () with
  | exception _ -> x
  | (module Impl) -> Impl.try_format x

let try_format_as_list ?toplevel x =
  match get_client () with
  | exception _ -> x
  | (module Impl) -> Impl.try_format_as_list ?toplevel x
