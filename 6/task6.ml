open Core_kernel

let rec index_of (l : Base.string list) (elem : string) : int = 
    match l with
    | [] -> -1
    | hd :: tl -> if String.equal hd elem then 0 else 1 + index_of tl elem

let get_input () = 
    let string_edges = In_channel.input_lines In_channel.stdin
    |> List.map ~f:(String.split ~on:')') in
    let string_nodes = List.concat string_edges
                       |> List.sort ~compare:String.compare
                       |> List.remove_consecutive_duplicates ~equal:String.equal in
    let int_edges = List.map ~f:(fun l ->
                                    match l with
                                    | [x; y] -> index_of string_nodes x,
                                                 index_of string_nodes y
                                    | _ -> raise (Failure "Edge failure")) string_edges in
    let n = List.length string_nodes in
    (n, string_nodes, int_edges)

let edges_of_node (edges : (int * int) list) (node : int) : int list = 
    List.filter ~f:(fun (x, _) -> x = node) edges
    |> List.map ~f:snd

let get_parent (edges : (int * int) list) (node : int ) : int =
    let edg = List.filter ~f:(fun (_, y) -> y = node) edges in
    match edg with
    | [] -> -1
    | hd :: _ -> fst hd

let rec get_sum (edges : (int * int) list) (node : int) (depth : int) : int =
    let sons = edges_of_node edges node in
    let sons_sum = List.map ~f:(fun son -> get_sum edges son (depth + 1)) sons
        |> List.fold ~f:(+) ~init:0 in
    depth + sons_sum

let task1 () = 
    let (_n, string_nodes, edges) = get_input () in
    let root = index_of string_nodes "COM" in
    printf "%d\n" (get_sum edges root 0)

let path_to_root (edges : (int * int) list) (node : int) : int list = 
    let nodes = ref [] in
    let curr = ref node in
    while !curr <> -1 do
        nodes := !curr :: !nodes;
        curr := get_parent edges !curr
    done;
    !nodes

let task2 () =
    let (_n, string_nodes, edges) = get_input () in
    let you = index_of string_nodes "YOU" in
    let san = index_of string_nodes "SAN" in
    let path1 = path_to_root edges you |> Array.of_list in
    let path2 = path_to_root edges san |> Array.of_list in
    let pos = ref 0 in
    while path1.(!pos) = path2.(!pos) do
        if path1.(!pos) = you || path1.(!pos) = san
        then raise (Failure "Ancestor case");
        pos := !pos + 1
    done;
    let ans = Array.length path1 + Array.length path2 - 2 * !pos - 2 in
    printf "%d\n" ans


let () = task2 ()