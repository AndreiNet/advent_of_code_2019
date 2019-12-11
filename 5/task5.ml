open Core_kernel

let get_input () =
    let inp = In_channel.input_line In_channel.stdin
        |> Option.value ~default:""
        |> String.split ~on:','
        |> List.map ~f:int_of_string
        |> Array.of_list in
    inp

type env = int array
type prog_input = int list ref

let get_task_input () : prog_input = ref [5]
let pop_input (inp : prog_input) : int =
    match !inp with
    | [] -> raise (Failure "EOF")
    | hd :: tl -> inp := tl;
                  hd

let valid_pos (env : env) (pos : int) : bool = 
    0 <= pos && pos < Array.length env

let get_value_both_modes (env : env) (pos : int) (mode : int) : int =
    if mode <> 0 && mode <> 1
    then raise (Failure "Invalid mode")
    else if not (valid_pos env pos)
    then raise (Failure "Invalid position")
    else if mode = 1 then env.(pos)
    else (
        if not (valid_pos env env.(pos))
        then raise (Failure "Invalid reference")
        else env.(env.(pos))
    )

let get_value_only_ref (env : env) (pos : int) (mode : int) : int =
    if mode <> 0 && mode <> 1
    then raise (Failure "Invalid mode")
    else if mode = 1
    then raise (Failure "Only ref command")
    else if not (valid_pos env env.(pos))
    then raise (Printf.sprintf "Invalid reference %d" env.(pos) |> Failure)
    else env.(pos)

let get_3_values (env : env) (instr : int) (pos : int) : int * int * int =
    let a = get_value_both_modes env pos (instr / 100 % 10) in
    let b = get_value_both_modes env (pos + 1) (instr / 1000 % 10) in
    let c = get_value_only_ref env (pos + 2) (instr / 10000 % 10) in
    a, b, c

let get_2_values (env : env) (instr : int) (pos : int) : int * int =
    let a = get_value_both_modes env pos (instr / 100 % 10) in
    let b = get_value_both_modes env (pos + 1) (instr / 1000 % 10) in
    a, b

let make_move (env : env) (inp : prog_input) (pos : int ref) : int =
    if !pos < 0 || !pos >= Array.length env
    then raise (Failure "Instruction out of bounds");
    if env.(!pos) < 0
    then raise (Failure "Negative instruction");
    let instr = env.(!pos) in
    match instr mod 100 with
    | 1 -> let a, b, c = get_3_values env instr (!pos + 1) in
           env.(c) <- a + b;
           !pos + 4
    | 2 -> let a, b, c = get_3_values env instr (!pos + 1) in
           env.(c) <- a * b;
           !pos + 4
    | 3 -> let npos = get_value_only_ref env (!pos + 1) (instr / 100 % 10) in 
           env.(npos) <- pop_input inp;
           !pos + 2
    | 4 -> let value = get_value_both_modes env (!pos + 1) (instr / 100 % 10) in
           printf "%d\n" value;
           !pos + 2
    | 5 -> let a, b = get_2_values env instr (!pos + 1) in
           if a <> 0 then b else !pos + 3
    | 6 -> let a, b = get_2_values env instr (!pos + 1) in
           if a = 0 then b else !pos + 3
    | 7 -> let a, b, c = get_3_values env instr (!pos + 1) in
           env.(c) <- if a < b then 1 else 0;
           !pos + 4
    | 8 -> let a, b, c = get_3_values env instr (!pos + 1) in
           env.(c) <- if a = b then 1 else 0;
           !pos + 4
    | 99 -> -1
    | x -> printf "Instruction code : %d\n" x; raise (Failure "Invalid instruction")

let task () = 
    let env = get_input () in
    printf "%d\n" (Array.length env);
    let inp = get_task_input () in
    let pos = ref 0 in
    try
        while !pos <> -1 do 
            pos := make_move env inp pos
        done
    with
    | Failure s ->
        raise (Failure (Printf.sprintf "At position %d\n%s" !pos s))

let () = task ()