let filename = "day3/test.txt"

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

let is_digit c = c >= '0' && c <= '9'

let find_numbers_in_line line =
    let rec aux idx current_start current_length numbers =
        Printf.printf "Current index: %d, Character: '%c'\n" idx (if idx < String.length line then line.[idx] else ' ');
        if idx = String.length line then
            if current_length > 0 then (current_start, current_length) :: numbers else numbers
        else
            let c = line.[idx] in
            if is_digit c then
                aux (idx + 1) (if current_length = 0 then idx else current_start) (current_length + 1) numbers
            else
                if current_length > 0 then
                    aux (idx + 1) 0 0 ((current_start, current_length) :: numbers)
                else
                    aux (idx + 1) 0 0 numbers
    in
    List.rev (aux 0 0 0 [])

[@@@warning "-26"]
let process_lines lines =
    let rec aux prev_line current_line next_line remaining_lines =
        match current_line, next_line, remaining_lines with
        | None, _, _ -> ()
        | Some current, next, rest ->
            let number_positions = find_numbers_in_line current in
            let next_line_str = match next with Some line -> line | None -> "None" in
            let prev_line_str = match prev_line with Some line -> line | None -> "None" in
            Printf.printf "Previous Line: %s, Current Line: %s, Next Line: %s\n" prev_line_str current next_line_str;
            let new_next_line, new_remaining_lines = match rest with
                | next_line :: remaining_lines -> (Some next_line, remaining_lines)
                | [] -> (None, [])
            in
            aux (Some current) next new_next_line new_remaining_lines
    in
    match lines with
    | [] -> ()
    | [single_line] ->
        Printf.printf "Previous Line: None, Current Line: %s, Next Line: None\n" single_line
    | first_line :: second_line :: rest_of_lines ->
        aux None (Some first_line) (Some second_line) rest_of_lines

[@@@warning "-26"]
let solve =
    let lines = read_lines filename in
    let total_sum = process_lines lines in
    failwith "TODO"

