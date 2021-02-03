module Sexp = Sexplib0.Sexp
module Csexp = Csexp.Make (Sexp)
module Ocf = Ocamlformat_rpc_lib
open Util.Result.Infix

module V = struct
  type t = Unknown | V1

  let to_string = function Unknown -> "?" | V1 -> "v1"

  let is_handled = function "v1" | "V1" -> Some V1 | _ -> None

  (* Supported versions by decreasing order *)
  let supported_versions = [ V1 ]
end

type process = {
  pid : int;
  input : in_channel;
  output : out_channel;
  version : V.t;
}

let running_process : process option ref = ref None

let log = Printf.printf

let negociate_version { input; output; _ } =
  let rec aux = function
    | [] -> Error "Version negociation failed"
    | latest :: others -> (
        let latest = V.to_string latest in
        let version = `Version latest in
        log "[ocf] proposed version %s\n%!" latest;
        Csexp.to_channel output (Ocf.Init.to_sexp version);
        flush output;
        match Ocf.Init.read_input input with
        | `Version v when v = latest -> (
            match V.is_handled v with
            | Some v ->
                log "[ocf] server accepted version %s\n%!" latest;
                Ok v
            | None -> failwith "impossible")
        | `Version vstr -> (
            log "Server proposed version: %s\n%!" vstr;
            match V.is_handled vstr with
            | Some v ->
                log "Version %s is supported\n%!" vstr;
                if List.mem v others then Ok v else aux others
            | None ->
                log "Version %s is not supported\n%!" vstr;
                aux others)
        | `Unknown ->
            log "Server answered unknown\n%!";
            Error "Version negociation failed"
        | `Halt ->
            log "Server did not answer with a version\n%!";
            Error "Version negociation failed")
  in
  aux V.supported_versions

let start () =
  let prog = "ocamlformat-rpc" in
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
      version = V.Unknown;
    }
  in
  negociate_version process >>| fun version ->
  let process = { process with version } in
  running_process := Some process;
  process

let get_process () =
  match !running_process with
  | None -> start ()
  | Some p ->
      (* Is the process still running ? *)
      let i, _ = Unix.waitpid [ WNOHANG ] p.pid in
      if i = 0 then Ok p else start ()

module V1 = struct
  let query t =
    get_process () >>| fun p ->
    Ocf.V1.output p.output t;
    log "Sent %s\n%!" (Ocf.V1.to_sexp t |> Sexp.to_string);
    Ocf.V1.read_input p.input

  let format x =
    log "Format '%s'\n%!" x;
    match query (`Format (x, None)) with
    | Ok (`Format (x, _)) -> Ok x
    | Ok (`Error msg) -> Error msg
    | _ -> Error "Unknown error"

  let halt () =
    match get_process () with
    | Ok p ->
      Ocf.V1.output p.output `Halt;
      log "Sent %s\n%!" (Ocf.V1.to_sexp `Halt |> Sexp.to_string);
      close_in p.input;
      close_out p.output;
      running_process := None
    | Error _ -> ()

  let try_format x = match format x with Ok x -> x | Error _ -> x
end
