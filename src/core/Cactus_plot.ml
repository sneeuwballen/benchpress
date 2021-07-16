
open Misc
open Test
module Gp = Gnuplot

type t = {
  lines: (string * Prover.name * float list) list;
}

let of_db db =
  Profile.with_ "plot.of-db" @@ fun () ->
  Misc.err_with
    ~map_err:(Printf.sprintf "while plotting DB: %s")
    (fun scope ->
       let provers = list_provers db |> scope.unwrap in
       Logs.debug (fun k->k "provers: [%s]" (String.concat ";" provers));
       let has_custom_tags =
         try ignore (Db.exec0_exn db "select 1 from custom_tags;"); true
         with _ -> false
       in
       let get_prover prover =
         Db.exec db
           (Printf.sprintf {| select rtime from prover_res
            where prover=? and
            (res in ('sat','unsat')
             %s
             )
            order by rtime|}
              (if has_custom_tags then "or exists (select 1 from custom_tags where tag=res)" else ""))
           prover
           ~ty:Db.Ty.(p1 text, p1 float, id) ~f:Db.Cursor.to_list
         |> scope.unwrap_with Db.Rc.to_string
       in
       let lines = CCList.map (fun p -> "", p, get_prover p) provers in
       { lines }
    )

let combine (l:(_*t) list) : t =
  {lines=
     CCList.flat_map (fun (pre1,plot) ->
       List.map (fun (pre2,prover,line) -> pre1^pre2,prover,line) plot.lines) l
  }

let of_file file : t or_error =
  try
    Db.with_db ~timeout:500 ~mode:`READONLY file of_db
  with e -> E.of_exn_trace e

let to_gp ~output self =
  Profile.with_ "plot.gnuplot" @@ fun () ->
  Gp.with_ (fun gp ->
      let series =
        self.lines
        |> CCList.map
          (fun (pre,prover,l) ->
             let l =
               let sum = ref 0. in
               CCList.mapi
                 (fun i rtime ->
                    sum := !sum +. rtime;
                    (!sum, float i))
                 l
             in
             let title = if pre="" then prover else pre^"."^prover in
             Gp.Series.linespoints_xy ~title l)
      in
      Gp.plot_many
        ~labels:(Gp.Labels.create ~x:"time (s)" ~y:"problems solved (accumulated)" ())
        ~title:"cumulative time for n° of problems solved" gp series ~output);
  ()

let show (self:t) =
  to_gp self ~output:(Gp.Output.create `X11)

let save_to_file (self:t) file =
  to_gp self ~output:(Gp.Output.create ~size:(1800,1024) @@ `Png file)

let to_png (self:t) : string =
  Profile.with_ "plot.to-png" @@ fun () ->
  CCIO.File.with_temp ~prefix:"benchpress_plot" ~suffix:".png"
    (fun file ->
       Logs.debug (fun k->k "plot into file %s" file);
       save_to_file self file;
       let s = CCIO.with_in file CCIO.read_all in
       Logs.debug (fun k->k "read %d bytes from file" (String.length s));
       s)
