open Cmdliner
open Result

let named wrapper = Term.(app (const wrapper))

let non_deterministic =
  let doc = "Run non-deterministic tests." in
  let env = Arg.env_var ~doc "MDX_RUN_NON_DETERMINISTIC" in
  named
    (fun x -> `Non_deterministic x)
    Arg.(value & flag & info [ "non-deterministic"; "n" ] ~env ~doc)

let r_syntax = ref None

let syntax =
  let parse s =
    match Mdx.Syntax.of_string s with
    | Some syntax ->
        r_syntax := Some syntax;
        `Ok syntax
    | None -> `Error (Format.sprintf "unrecognized syntax %S" s)
  in
  let syntax = (parse, Mdx.Syntax.pp) in
  let doc =
    "Which syntax to use. Either 'markdown' (also 'normal'), 'cram', or 'mli'."
  in
  named
    (fun x -> `Syntax x)
    Arg.(value & opt (some syntax) None & info [ "syntax" ] ~doc ~docv:"SYNTAX")

let file_docv = "FILE"

let r_file = ref []

let file_conv =
  let check_file, fmt = Arg.non_dir_file in
  let check_file x =
    match check_file x with
    | `Ok s -> (
        let syntax = Option.value !r_syntax ~default:Normal in
        let file_contents, lexbuf = Mdx.Misc.init x in
        match Mdx.parse_lexbuf file_contents syntax lexbuf with
        | Ok items ->
            r_file := items;
            `Ok s
        | Error (`Msg e) -> `Error e )
    | `Error e -> `Error e
  in
  (check_file, fmt)

let file =
  let doc = "The file to use." in
  named
    (fun x -> `File x)
    Arg.(required & pos 0 (some file_conv) None & info [] ~doc ~docv:file_docv)

let section_conv =
  let check_section, fmt = Arg.string in
  let check_section x =
    match check_section x with
    | `Ok s ->
        let items = !r_file in
        let section_re = Re.Perl.compile_pat x in
        if
          List.exists
            (function
              | Mdx.Document.Text _ -> false
              | Mdx.Document.Section (_, s) -> Re.execp section_re s
              | Mdx.Document.Block _ -> false)
            items
        then `Ok s
        else `Error (Format.sprintf "%s is not a valid section" x)
    | `Error e -> `Error e
  in
  (check_section, fmt)

let section =
  let doc =
    "Select file sub-sections. Will be interpreted as a Perl regular \
     expression."
  in
  named
    (fun x -> `Section x)
    Arg.(
      value
      & opt (some section_conv) None
      & info [ "section"; "s" ] ~doc ~docv:"PAT")

let silent_eval =
  let doc = "Do not show the result of evaluating toplevel phrases." in
  named
    (fun x -> `Silent_eval x)
    Arg.(value & flag & info [ "silent-eval" ] ~doc)

let record_backtrace =
  let doc = "Print backtraces when evaluating toplevel phrases." in
  named
    (fun x -> `Record_backtrace x)
    Arg.(value & flag & info [ "record-backtrace" ] ~doc)

let silent =
  let doc = "Do not show any (phrases and findlib directives) results." in
  named (fun x -> `Silent x) Arg.(value & flag & info [ "silent" ] ~doc)

let verbose_findlib =
  let doc =
    "Show the result of evaluating findlib directives in toplevel fragments."
  in
  named
    (fun x -> `Verbose_findlib x)
    Arg.(value & flag & info [ "verbose-findlib" ] ~doc)

let prelude =
  let parse s =
    let _, file = Mdx.Prelude.env_and_file s in
    let parse, _ = Arg.non_dir_file in
    match parse file with `Ok _ -> `Ok s | `Error e -> `Error e
  in
  let prelude = (parse, Fmt.string) in
  let doc =
    "A file to load as prelude. Can be prefixed with $(i,env:) to specify a \
     specific environment to load the prelude in. Multiple prelude files can \
     be provided:they will be evaluated in the order they are provided on the \
     command-line."
  in
  named
    (fun x -> `Prelude x)
    Arg.(value & opt_all prelude [] & info [ "prelude" ] ~doc ~docv:"FILE")

let prelude_str =
  let doc =
    "A string to load as prelude. Can be prefixed with $(i,env:) to specify a \
     specific environment to load the prelude in (the environment name should \
     not contain any spaces. Multiple prelude strings can be provided: they \
     will be evaluated in the order they are provided on the command-line."
  in
  named
    (fun x -> `Prelude_str x)
    Arg.(value & opt_all string [] & info [ "prelude-str" ] ~doc ~docv:"STR")

let root =
  let doc = "The directory to run the tests from." in
  named
    (fun x -> `Root x)
    Arg.(value & opt (some dir) None & info [ "root" ] ~doc ~docv:"DIR")

let force_output =
  let doc = "Force generation of corrected file (even if there was no diff)" in
  named
    (fun x -> `Force_output x)
    Arg.(value & flag & info [ "force-output" ] ~doc)

type output = File of string | Stdout

let output_conv =
  let sparse, sprint = Arg.string in
  let parse s =
    match sparse s with
    | `Ok "-" -> Ok Stdout
    | `Ok s -> Ok (File s)
    | `Error msg -> Error (`Msg msg)
  in
  let print fmt = function
    | Stdout -> sprint fmt "-"
    | File s -> sprint fmt s
  in
  Arg.conv ~docv:"OUTPUT" (parse, print)

let output =
  let docv = "OUTPUT" in
  let doc =
    Printf.sprintf
      "Specify where to write the command output. $(docv) should be $(b,-) for \
       stdout or a filename. Defaults to $(i,%s).corrected. Note that setting \
       this option implies $(b,--force-output)."
      file_docv
  in
  named
    (fun x -> `Output x)
    Arg.(
      value & opt (some output_conv) None & info ~doc ~docv [ "o"; "output" ])

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup =
  named
    (fun x -> `Setup x)
    Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
