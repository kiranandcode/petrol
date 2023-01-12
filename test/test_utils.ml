let () = Printexc.register_printer (function (Failure msg) -> Some msg | _ -> None)

let run expr =
  match Lwt_main.run expr with
  | Ok _ -> ()
  | Error (`Newer_version_than_supported _) ->
    failwith ("Attempted to use a newer database than supported")
  | Error (#Caqti_error.t as err) -> failwith ((Caqti_error.show err))
  | Error (`Msg m) -> failwith m

let main f =
  if Array.length Sys.argv < 2 then begin
    print_endline "USAGE: test_runner.exe CMD [options...]";
    exit 255
  end;
  let open Lwt_result.Syntax in
  let fname = Sys.argv.(2) in
  run begin
    let* conn = Caqti_lwt.connect (Uri.of_string ("sqlite3://:" ^ fname)) in
    let args = List.init (Array.length Sys.argv - 3) (fun ind -> Sys.argv.(3 + ind)) in
    match f conn (Sys.argv.(1), args) with
    | computation ->
      computation
    | exception (Match_failure _) -> failwith "unknown query command"
  end
