open Core_kernel

let take_input () =
    let lines = In_channel.input_lines In_channel.stdin in
    List.map ~f:int_of_string lines

let task1 () =
    let numbers = take_input () in
    let processed_numbers = List.map ~f:(fun x -> x / 3 - 2) numbers in
    let sum = List.fold_left ~f:(+) ~init:0 processed_numbers in
    printf "%d\n" sum

let take_fuel_recursively (x : int) : int =
    let x = ref (x / 3 - 2) in
    let sum = ref 0 in
    while !x > 0 do
        sum := !sum + !x;
        x := !x / 3 - 2
    done;
    !sum

let task2 () =
    let numbers = take_input () in
    let processed_numbers = List.map ~f:take_fuel_recursively numbers in
    printf "%d\n" (List.fold_left ~f:(+) ~init:0 processed_numbers)


let () = task2 ()