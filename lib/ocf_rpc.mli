module V1 : sig
  val halt : unit -> unit

  val try_config : (string * string) list -> unit

  val try_format : string -> string

  val try_format_as_list : ?toplevel:bool -> string list -> string list
end
