open Common
open Misc
open Test
module Gp = Gnuplot

type t = { lines: (string * Prover.name * float list) list }

let of_db ?provers db =
  Error.guard (Error.wrap "producting cactus plot") @@ fun () ->
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "cactus-plot.of-db" in
  let provers =
    match provers with
    | Some provers -> provers
    | None -> list_provers db
  in
  Logs.debug (fun k -> k "provers: [%s]" (String.concat ";" provers));
  let has_custom_tags =
    try
      ignore (Db.exec0_exn db "select 1 from custom_tags;");
      true
    with _ -> false
  in
  let get_prover prover =
    Db.exec db
      (Printf.sprintf
         {| select rtime from prover_res
            where prover=? and
            (res in ('sat','unsat')
             %s
             )
            order by rtime|}
         (if has_custom_tags then
            "or exists (select 1 from custom_tags where tag=res)"
          else
            ""))
      prover
      ~ty:Db.Ty.(p1 text, p1 float, id)
      ~f:Db.Cursor.to_list
    |> Misc.unwrap_db (fun () -> spf "obtaining values for prover '%s'" prover)
  in
  let lines = CCList.map (fun p -> "", p, get_prover p) provers in
  { lines }

let combine (l : (_ * t) list) : t =
  {
    lines =
      CCList.flat_map
        (fun (pre1, plot) ->
          List.map
            (fun (pre2, prover, line) -> pre1 ^ pre2, prover, line)
            plot.lines)
        l;
  }

let of_file ?provers file : t =
  try Db.with_db ~timeout:500 ~mode:`READONLY file (of_db ?provers)
  with e -> Error.(raise @@ of_exn e)

let to_gp ~output self =
  Error.guard (Error.wrap "plot.gnuplot") @@ fun () ->
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "cactus-plot.gnuplot" in
  Gp.with_ (fun gp ->
      let series =
        self.lines
        |> CCList.map (fun (pre, prover, l) ->
               let l =
                 let sum = ref 0. in
                 CCList.mapi
                   (fun i rtime ->
                     sum := !sum +. rtime;
                     !sum, float i)
                   l
               in
               let title =
                 if pre = "" then
                   prover
                 else
                   pre ^ "." ^ prover
               in
               Gp.Series.linespoints_xy ~title l)
      in
      Gp.plot_many
        ~labels:
          (Gp.Labels.create ~x:"time (s)" ~y:"problems solved (accumulated)" ())
        ~title:"cumulative time for n° of problems solved" gp series ~output);
  ()

let show (self : t) = to_gp self ~output:(Gp.Output.create `X11)

let save_to_file (self : t) file =
  to_gp self ~output:(Gp.Output.create ~size:(1800, 1024) @@ `Png file)

let to_echarts_json (self : t) : string =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "plot.to-echarts-json" in
  (* Build series: for each prover, compute cumulative time vs. problem count.
     x = cumulative time (sum of rtimes up to problem i)
     y = i+1 (number of problems solved so far) *)
  let series =
    List.map
      (fun (pre, prover, rtimes) ->
        let title =
          if pre = "" then
            prover
          else
            pre ^ "." ^ prover
        in
        (* Build list of [cumtime, count] pairs *)
        let _sum, rev_data =
          List.fold_left
            (fun (cum, acc) rtime ->
              let cum' = cum +. rtime in
              cum', [ `Float cum'; `Int (List.length acc + 1) ] :: acc)
            (0., [])
            rtimes
        in
        let data = List.rev rev_data in
        `Assoc
          [
            "name", `String title;
            "type", `String "line";
            "showSymbol", `Bool false;
            "data", `List (List.map (fun pt -> `List pt) data);
          ])
      self.lines
  in
  let legend_data =
    List.map
      (fun (pre, prover, _) ->
        let title =
          if pre = "" then
            prover
          else
            pre ^ "." ^ prover
        in
        `String title)
      self.lines
  in
  let option =
    `Assoc
      [
        "title",
        `Assoc
          [
            "text", `String "Cactus plot";
            "subtext", `String "cumulative time for n\xc2\xb0 of problems solved";
          ];
        "tooltip", `Assoc [ "trigger", `String "axis" ];
        "legend", `Assoc [ "data", `List legend_data ];
        "xAxis",
        `Assoc
          [
            "name", `String "time (s)";
            "type", `String "value";
            "nameLocation", `String "middle";
            "nameGap", `Int 25;
          ];
        "yAxis",
        `Assoc
          [
            "name", `String "problems solved (accumulated)";
            "type", `String "value";
            "nameLocation", `String "middle";
            "nameGap", `Int 50;
          ];
        "series", `List series;
      ]
  in
  Yojson.Basic.to_string option

let to_png (self : t) : string =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "plot.to-png" in
  CCIO.File.with_temp ~prefix:"benchpress_plot" ~suffix:".png" (fun file ->
      Logs.debug (fun k -> k "plot into file %s" file);
      save_to_file self file;
      let s = CCIO.with_in file CCIO.read_all in
      Logs.debug (fun k -> k "read %d bytes from file" (String.length s));
      s)
