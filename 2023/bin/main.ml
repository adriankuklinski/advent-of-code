let () =
    match Sys.argv with
    | [| _; "day1" |] -> Day1.solve
    | [| _; "day2" |] -> Day2.solve
    | _ -> Printf.eprintf "Usage %s day1|day2\n" Sys.argv.(0)
