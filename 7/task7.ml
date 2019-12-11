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

type program_state = { env : env; pos: int ref }

let pop_input (inp : prog_input) : int option =
    match !inp with
    | [] -> None
    | hd :: tl -> inp := tl;
                  Some(hd)

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

let make_move (env : env) (inp : prog_input) (pos : int ref) (output : int list ref) : int =
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
           (match pop_input inp with
           | None -> !pos
           | Some v -> env.(npos) <- v; !pos + 2)
    | 4 -> let value = get_value_both_modes env (!pos + 1) (instr / 100 % 10) in
           output := value :: !output;
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

let run (env : env) (inp : prog_input) (pos : int ref) : int list * bool =
    let prev_pos = ref (-1) in
    let output = ref [] in
    try
        while !pos <> -1 && !pos <> !prev_pos do
            prev_pos := !pos;
            pos := make_move env inp pos output
        done;
        output := List.rev !output;
        (!output, !pos = -1)
    with
    | Failure s -> raise (Failure (Printf.sprintf "At position %d\n%s" !pos s))

let run_with_phases (env : env) (phases : int list) : int = 
    let outputs = ref [0] in
    let phases = Array.of_list phases in
    for i = 0 to (Array.length phases - 1) do
        outputs := run (Array.copy env) (ref (phases.(i) :: !outputs)) (ref 0) |> fst;
        if List.length !outputs <> 1 then raise (Failure "Number of outputs different from 1")
    done;
    match !outputs with
    | [] -> raise (Failure "No final output")
    | hd :: _ -> hd

let has_unique_elems (l : int list) : bool = 
    List.length l = List.length (List.sort l ~compare |> List.remove_consecutive_duplicates ~equal)

let rec all_phases ?(val_min : int = 0) ~(val_max : int) (n : int) : int list list =
    match n with
    | 0 -> [[]]
    | n -> let following = all_phases (n - 1) ~val_min ~val_max in
           List.init (val_max - val_min) ~f:(fun x -> x + val_min)
           |> List.map ~f:(fun x -> List.map ~f:(fun phases -> x :: phases) following)
           |> List.concat
           |> List.filter ~f:has_unique_elems

let task1 () = 
    let env = get_input () in
    let confs = all_phases 5 ~val_max:5 in
    let results = List.map ~f:(run_with_phases env) confs in
    let ans = List.max_elt results ~compare |> Option.value ~default:0 in
    printf "%d\n" ans

let test () =
    let confs = all_phases ~val_min:5 ~val_max:8 3 in
    List.iter confs ~f:(fun conf -> List.iter conf ~f:(printf "%d ");
                                    printf "\n")

let loop_with_phases (env : env) (phases : int list) : int =
    let give_phase = ref true in
    let outputs = ref [0] in
    let states = List.map phases ~f:(fun _ -> { env = Array.copy env; pos = ref 0 })
                |> Array.of_list in
    let phases = Array.of_list phases in
    let halt = ref false in
    while not !halt do
        for i = 0 to (Array.length phases - 1) do
            let curr_output = match !give_phase with
                | false -> !outputs
                | true -> phases.(i) :: !outputs in
            let o = run states.(i).env (ref curr_output) states.(i).pos in
            outputs := fst o;
            halt := snd o;
            if List.length !outputs < 1 then raise (Failure "Number of outputs different from 1")
        done;
        give_phase := false
    done;
    match !outputs with
    | [] -> raise (Failure "No final output")
    | hd :: _ -> hd

let task2 () = 
    let env = get_input () in
    let confs = all_phases ~val_min:5 ~val_max:10 5 in
    let results = List.map ~f:(loop_with_phases env) confs in
    let ans = List.max_elt results ~compare |> Option.value ~default:0 in
    printf "%d\n" ans

let () = task2 ()