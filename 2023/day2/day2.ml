let filename = "day2/input.txt"

type color = Red | Green | Blue
type game_round = (color * int) list
type game = { id: int; rounds: game_round list }

let read_lines filename =
    let channel = open_in filename in
    let rec read_acc acc =
        try
            let line = input_line channel in
            read_acc (line :: acc)
        with End_of_file -> acc
    in
    let lines = read_acc [] in
    close_in channel;
    List.rev lines

let string_to_color color =
    match color with
    | "red" -> Red
    | "green" -> Green
    | "blue" -> Blue
    | _ -> failwith ("Not a valid color: " ^ color)

let parse_game_round round =
    let pairs = String.split_on_char ',' (String.trim round) in
    List.map (fun pair ->
        match String.split_on_char ' ' (String.trim pair) with
            | [count; color] -> (string_to_color color, int_of_string count)
            | _ -> failwith ("Invalid [count; color] pair")
    ) pairs

let parse_game line =
    if String.trim line = "" then None
    else
        let game_parts = String.split_on_char ':' line in
        match game_parts with
        | [game_id; game_rounds] ->
            let id = int_of_string (String.trim (String.sub game_id 5 (String.length game_id -5))) in
            let rounds = String.split_on_char ';' (String.trim game_rounds) in
            let parsed_rounds = List.map parse_game_round rounds in
            Some { id; rounds = parsed_rounds }
        | _ -> failwith ("Invalid game format: " ^ line)

let is_round_valid round =
    List.fold_left (fun acc (color, count) ->
        acc && (match color with
            | Red -> count <= 12
            | Green -> count <= 13
            | Blue -> count <= 14)
    ) true round

let is_game_valid game =
    List.for_all is_round_valid game.rounds

let sum_valid_game_ids games =
    List.fold_left (fun acc game ->
        if is_game_valid game then acc + game.id else acc
    ) 0 games

let get_cube_power game =
    let max_cubes = List.fold_left (fun (max_red, max_green, max_blue) round ->
        List.fold_left (fun (mr, mg, mb) (color, count) ->
            match color with
            | Red -> (max mr count, mg, mb)
            | Green -> (mr, max mg count, mb)
            | Blue -> (mr, mg, max mb count)
        ) (max_red, max_green, max_blue) round
    ) (0, 0, 0) game.rounds in
    let (max_red, max_green, max_blue) = max_cubes in
    max_red * max_green * max_blue

let sum_cube_power games =
    List.fold_left (fun acc game ->
        acc + get_cube_power game
    ) 0 games

let solution_part_1 lines =
    let parsed_games = List.filter_map parse_game lines in
    sum_valid_game_ids parsed_games

let solution_part_2 lines =
    let parsed_games = List.filter_map parse_game lines in
    sum_cube_power parsed_games

let solve =
    let lines = read_lines filename in
    let result_part_1 = solution_part_1 lines in
    let result_part_2 = solution_part_2 lines in
    Printf.printf "Result Part 1: %d \nResult Part 2: %d \n" result_part_1 result_part_2

