let () =
    match Sys.argv with
    | [| _; "day1" |] -> Day1.solve
    | _ -> Printf.eprintf "Usage %s day1\n" Sys.argv.(0)
