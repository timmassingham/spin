open Jingoo

let jg_string_fn ?kwargs:_ ?defaults:_ fn value =
  let value = Jg_runtime.string_of_tvalue value in
  let slug = fn value in
  Jg_types.Tstr slug

let filters =
  [ "slugify", Helpers.slugify |> jg_string_fn |> Jg_types.func_arg1_no_kw
  ; "snake_case", Helpers.snake_case |> jg_string_fn |> Jg_types.func_arg1_no_kw
  ; "camel_case", Helpers.camel_case |> jg_string_fn |> Jg_types.func_arg1_no_kw
  ]

let jg_models_of_context context =
  Hashtbl.to_alist context |> List.map ~f:(fun (k, v) -> k, Jg_types.Tstr v)

let generate_string ~context s =
  Jg_template.from_string
    s
    ~models:(jg_models_of_context context)
    ~env:{ Jg_types.std_env with filters = Jg_types.std_env.filters @ filters }

let generate ~context ~content path =
  let open Lwt_result.Syntax in
  let* content =
    (try Ok (generate_string content ~context) with
    | _ ->
      Error
        (Spin_error.failed_to_generate
           (Printf.sprintf
              "Error while running the template engine on content of %S."
              path)))
    |> Lwt.return
  in
  (* Need to normalize the file separation because "\\" will escape the
     expressions to evaluate in the template engine *)
  let normalized_path =
    String.substr_replace_all path ~pattern:"\\" ~with_:"/"
  in
  let* normalized_path =
    (try Ok (generate_string normalized_path ~context) with
    | _ ->
      Error
        (Spin_error.failed_to_generate
           (Printf.sprintf
              "Error while running the template engine on the path of %S"
              path)))
    |> Lwt.return
  in
  let* () = Logs_lwt.debug (fun m -> m "Generating %s" path) |> Lwt_result.ok in
  Filename.dirname normalized_path |> Spin_unix.mkdir_p;
  Lwt_io.with_file
    normalized_path
    (fun oc -> Lwt_io.write oc content |> Lwt_result.ok)
    ~mode:Lwt_io.Output
