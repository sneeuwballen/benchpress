type flag = {
  names: string list;
      (** cmdliner-style names: single-char names become short options ([t]),
          multi-char names become long options ([timeout]) *)
  has_value: bool;
}

type cmd = {
  name: string;
  doc: string;
  flags: flag list;
  positional: string option;
  subcmds: cmd list;
}

let flag_of_param names ?(has_value = false) () = { names; has_value }

let flag_str f =
  f.names
  |> List.map (fun n ->
         if String.length n = 1 then
           "-" ^ n
         else
           "--" ^ n)
  |> String.concat " "

let emit_line buf fmt =
  Printf.ksprintf
    (fun s ->
      Buffer.add_string buf s;
      Buffer.add_char buf '\n')
    fmt

let gen_bash cmds =
  let buf = Buffer.create 512 in
  emit_line buf "_benchpress() {";
  emit_line buf "  local cur prev words cword";
  emit_line buf "  _init_completion || return";
  emit_line buf "";
  let names = String.concat " " (List.map (fun c -> c.name) cmds) in
  emit_line buf "  if [[ $cword -eq 1 ]]; then";
  emit_line buf "    COMPREPLY=($(compgen -W \"%s\" -- \"$cur\"))" names;
  emit_line buf "    return";
  emit_line buf "  fi";
  emit_line buf "";
  emit_line buf "  case ${words[1]} in";
  let go_cmd ~indent buf cmd =
    let flags_s = String.concat " " (List.map flag_str cmd.flags) in
    if cmd.subcmds <> [] then (
      let subs_s = String.concat " " (List.map (fun c -> c.name) cmd.subcmds) in
      emit_line buf "%s%s)" indent cmd.name;
      emit_line buf "%s  if [[ $cword -eq 2 ]]; then" indent;
      emit_line buf "%s    COMPREPLY=($(compgen -W \"%s\" -- \"$cur\"))" indent
        subs_s;
      emit_line buf "%s  else" indent;
      emit_line buf "%s    case ${words[2]} in" indent;
      List.iter
        (fun sub ->
          let sub_f = String.concat " " (List.map flag_str sub.flags) in
          emit_line buf
            "%s      %s) COMPREPLY=($(compgen -W \"%s\" -- \"$cur\")) ;;" indent
            sub.name sub_f)
        cmd.subcmds;
      emit_line buf "%s    esac" indent;
      emit_line buf "%s  fi" indent;
      emit_line buf "%s  ;;" indent
    ) else
      emit_line buf "%s%s) COMPREPLY=($(compgen -W \"%s\" -- \"$cur\")) ;;"
        indent cmd.name flags_s
  in
  List.iter (go_cmd ~indent:"  " buf) cmds;
  emit_line buf "  esac";
  emit_line buf "}";
  Buffer.contents buf

let gen_fish cmds =
  let buf = Buffer.create 512 in
  (* Ancestor path (e.g. ["meta"; "get"]) rendered as a fish condition that
     requires every ancestor to have been seen on the command line. *)
  let cond_str path =
    if path = [] then
      ""
    else (
      let conds =
        List.map
          (fun c -> Printf.sprintf "__fish_seen_subcommand_from %s" c)
          path
      in
      Printf.sprintf "-n '%s' " (String.concat "; and " conds)
    )
  in
  let rec go path cmds =
    List.iter
      (fun (c : cmd) ->
        (* Command name: available once the parent path has been seen *)
        let name_cond = cond_str path in
        emit_line buf "complete -c benchpress %s-a \"%s\" -d \"%s\"" name_cond
          c.name c.doc;
        (* Flags: available once this command (and its ancestors) have been
           seen *)
        let flag_cond = cond_str (path @ [ c.name ]) in
        List.iter
          (fun (f : flag) ->
            let r =
              if f.has_value then
                " -r"
              else
                ""
            in
            List.iter
              (fun n ->
                if String.length n = 1 then
                  emit_line buf "complete -c benchpress %s-s %s%s" flag_cond n r
                else
                  emit_line buf "complete -c benchpress %s-l \"%s\"%s" flag_cond
                    n r)
              f.names)
          c.flags;
        go (path @ [ c.name ]) c.subcmds)
      cmds
  in
  go [] cmds;
  Buffer.contents buf
