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

let main_postgres f =
  let (let*) x f = Lwt.bind x (function Error err -> failwith (Caqti_error.show err) | Ok v -> f v) in
  run @@ begin
    let args = List.init (Array.length Sys.argv - 1) (fun ind -> Sys.argv.(1 + ind)) in
    match[@warning "-8"] args with
    | ["createdb"; name] ->
      let* (module DB) = Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432") in
      Lwt.bind (DB.exec (Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit)
                           (Format.sprintf {sql| CREATE DATABASE %s |sql} name)) ()) @@ 
      fun err -> Lwt.bind (DB.disconnect ()) @@ fun () -> Lwt.return err
    | ["dropdb"; name] ->
      let* (module DB) = Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432") in
      Lwt.bind (DB.exec (Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit)
                           (Format.sprintf {sql| DROP DATABASE IF EXISTS %s WITH (FORCE) |sql} name)) ()) @@
      fun err -> Lwt.bind (DB.disconnect ()) @@ fun () -> Lwt.return err
    | name :: args ->
      let* ((module DB) as conn) = Caqti_lwt.connect (Uri.of_string (Format.sprintf "postgresql://localhost:5432/%s" name)) in
      try
        Lwt.bind begin match f conn args with
          | computation -> computation
          | exception (Match_failure _)  -> failwith "unknown query command"
        end @@ fun res ->
        Lwt.return res
      with
       e ->
       Lwt.bind (DB.disconnect ()) @@ fun () -> Lwt.return (Error (`Msg (Printexc.to_string e)))
  end  
