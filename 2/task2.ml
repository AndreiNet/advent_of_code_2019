open Core_kernel

let get_input () =
    In_channel.input_all In_channel.stdin |>
    String.split ~on:',' |>
    List.map ~f:String.strip |>
    List.filter ~f:(fun x -> String.is_empty x |> not) |>
    List.map ~f:int_of_string

let evaluate (numbers : int array) : int option =
    let pos = ref 0 in
    while 0 <= !pos && !pos + 3 < Array.length numbers && numbers.(!pos) <> 99 do
        let opcode = numbers.(!pos) in
        let a = numbers.(!pos + 1) in
        let b = numbers.(!pos + 2) in
        let c = numbers.(!pos + 3) in
        if b >= 0 && b < Array.length numbers && c >= 0 && c < Array.length numbers &&
            a >= 0 && a < Array.length numbers then (
            numbers.(c) <- (if opcode = 1 then (+) else ( * )) numbers.(a) numbers.(b);
            pos := !pos + 4
        ) else
            pos := -1
    done;
    if !pos < 0 || !pos + 3 >= Array.length numbers then None else Some(numbers.(0))


let task1 () =
    let numbers = get_input() in
    let numbers = (List.nth numbers 0 |> Option.value ~default:0) :: 12 :: 2 :: (List.drop numbers 3) in
    let numbers = Array.of_list numbers in
    let ans = evaluate numbers |> Option.value ~default:0 in
    printf "%d\n" ans

let task2 () =
    let numbers = get_input() |> Array.of_list in
    for pos1 = 1 to 1000 do
        for pos2 = 1 to 1000 do
            let numbers = Array.copy numbers in
            numbers.(1) <- pos1;
            numbers.(2) <- pos2;
            let ans = evaluate numbers in
            if Option.value ~default:(-1) ans = 19690720 then printf "%d %d\n" pos1 pos2
        done
    done

let () = task2 ()