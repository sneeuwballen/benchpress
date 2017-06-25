
(* This file is free software. See file "license" for more details. *)

(** {1 Junit wrapper} *)

let test_analyze (t:Test.Analyze.t) : Junit.Testsuite.t =
  let open Test in
  let module J = Junit in
  let l =
    MStr.fold
      (fun _ r acc ->
         let prover = r.Event.program in
         let res = Event.analyze_p r in
         let name =
           Printf.sprintf "prover `%s` on problem `%s`"
             prover.Prover.name
             r.Event.problem.Problem.name
         and message =
           Printf.sprintf "result: `%s`, expected: `%s`"
             (Res.to_string res)
             (Res.to_string r.Event.problem.Problem.expected)
         and classname = ""
         and typ = ""
         and time = r.Event.raw.Event.rtime
         in
         let case =
           match Problem.compare_res r.Event.problem res with
             | `Error
             | `Mismatch ->
               J.Testcase.error
                 ~typ ~classname ~time
                 ~name
                 ~message:""
                 message
             | `Disappoint
             | `Same
             | `Improvement ->
               J.Testcase.pass
                 ~classname ~time
                 ~name
         in
         case :: acc)
      t.Analyze.raw
      []
  in
  let suite =
    J.Testsuite.make
      ?package:None
      ?timestamp:None
      ?hostname:None
      ?system_out:None
      ?system_err:None
      ~name:"frogtest"
  in
  J.Testsuite.add_testcases l suite

let junit_to_file suites file =
  let report = Junit.make suites in
  let xml_report = Junit.to_xml report in
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "@[%a@]@." (Tyxml.Xml.pp ()) xml_report;
  close_out oc;
  ()
