let filename = "day1/input.txt"

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

let parse_line line =
    let is_digit c = c >= '0' && c <= '9' in
    let n = String.length line in
    let rec find_first_last i first last  =
        if i >= n then (first, last)
        else
            let c = line.[i] in
            if is_digit c then
                match first with
                | None -> find_first_last (i + 1) (Some c) (Some c)
                | Some _ -> find_first_last (i + 1) first (Some c)
            else find_first_last (i + 1) first last
    in
    match find_first_last 0 None None with
    | (Some first_digit, Some last_digit) -> int_of_string (String.make 1 first_digit ^ String.make 1 last_digit)
    | _ -> 0

let sum list =
    List.fold_left (fun acc x -> acc + x) 0 list

let replace_digit_words line =
    let number_words = [("nine", "9"); ("eight", "8"); ("seven", "7"); ("six", "6"); ("five", "5"); ("four", "4"); ("three", "3"); ("two", "2"); ("one", "1")] in
    let is_digit c = c >= '0' && c <= '9' in
    let rec find_numbers i acc =
        if i >= String.length line then List.rev acc
        else if is_digit line.[i] then
            find_numbers (i + 1) ((String.make 1 line.[i], i) :: acc)
        else
            let found = List.fold_left (fun (found, word_acc) (word, num) ->
                if not found && i + String.length word <= String.length line && String.sub line i (String.length word) = word then
                    (true, (num, i) :: word_acc)
                else (found, word_acc)
            ) (false, acc) number_words in
            let next_i = if fst found then i + 1 else i + 1 in
            find_numbers next_i (snd found)
    in
    let number_indices = find_numbers 0 [] in
    let sorted_number_indices = List.sort (fun (_, i1) (_, i2) -> compare i1 i2) number_indices in
    String.concat "" (List.map fst sorted_number_indices)

let solution_part_1 lines =
    let transformed_lines = List.map parse_line lines in
    sum transformed_lines

let solution_part_2 lines =
    let sanitized_lines = List.map replace_digit_words lines in
    let transformed_lines = List.map parse_line sanitized_lines in
    sum transformed_lines

let solve =
    let filename = "day1/input.txt" in
    let lines = read_lines filename in
    let result_part_1 = solution_part_1 lines in
    let result_part_2 = solution_part_2 lines in
    Printf.printf "Result Part 1: %d \nResult Part 2: %d \n" result_part_1 result_part_2
