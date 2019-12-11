open Core_kernel

let get_input () = 
    let first = In_channel.input_line In_channel.stdin |> Option.value ~default:"" in
    let second = In_channel.input_line In_channel.stdin |> Option.value ~default:"" in
    let process (line : string) : (char * int) list =
        String.split ~on:',' line |>
        List.map ~f:(fun tok -> (String.get tok 0, String.drop_prefix tok 1 |> int_of_string)) in
    (process first, process second)

type segment =
    | Vertical of { x1 : int; x2 : int; y : int }
    | Horizontal of { x: int; y1: int; y2: int }

let set_right (seg : segment) : segment = 
    match seg with
    | Vertical { x1; x2; y } -> Vertical { x1 = min x1 x2; x2 = max x1 x2; y }
    | Horizontal { x; y1; y2 } -> Horizontal { x; y1 = min y1 y2; y2 = max y1 y2 }

let add_segment (acc : segment list) (move : char * int) : segment list =
    let (lastx, lasty) = match acc with
                        | [] -> (0, 0)
                        | (Vertical { x1 = _; x2; y }) :: _tail -> (x2, y)
                        | (Horizontal { x; y1 = _; y2 }) :: _tail -> (x, y2) in
    let (incx, incy) = match (fst move) with
                       | 'L' -> (0, -1)
                       | 'R' -> (0, 1)
                       | 'U' -> (-1, 0)
                       | 'D' -> (1, 0)
                       | _ -> raise (Failure "Wrong move") in
    let steps = snd move in
    let (newx, newy) = (lastx + incx * steps, lasty + incy * steps) in
    (* printf "%d %d\n" incx incy;
    printf "%d %d %d %d\n" lastx lasty newx newy; *)
    let seg = if newx = lastx then Horizontal { x = newx; y1 = lasty; y2 = newy }
              else if newy = lasty then Vertical { x1 = lastx; x2 = newx; y = newy }
              else raise (Failure "Wrong segment") in
    seg :: acc

let compute_segments (moves : (char * int) list): segment list =
    List.fold_left ~init:[] ~f:add_segment moves |> List.rev

let closest_point_line (a : int) (b : int) (c : int) (d : int) : int option =
    let beg = max a c in
    let en = min b d in
    if beg <= en then
        let x = if beg > 0 then beg else if en < 0 then en else 0 in
        (* printf "%d\n" x; *)
        Some(x)
    else None

let intersection x1 x2 y x y1 y2 = 
    if y1 <= y && y <= y2 && x1 <= x && x <= x2 then Some (x, y)
    else None

let segment_intersection (seg1 : segment) (seg2 : segment) : (int * int) option =
    let seg1 = set_right seg1 in
    let seg2 = set_right seg2 in 
    match seg1 with
    | Vertical { x1 = s1x1; x2 = s1x2; y = s1y } -> (
        match seg2 with
        | Vertical { x1 = s2x1; x2 = s2x2; y = s2y } -> if s1y = s2y then
                                                            match (closest_point_line s1x1 s1x2 s2x1 s2x2) with
                                                            | Some x -> Some(x, s1y)
                                                            | None -> None
                                                        else None
        | Horizontal { x = s2x; y1 = s2y1; y2 = s2y2 } -> intersection s1x1 s1x2 s1y s2x s2y1 s2y2
    )
    | Horizontal { x = s1x; y1 = s1y1; y2 = s1y2 } -> (
        match seg2 with
        | Vertical { x1 = s2x1; x2 = s2x2; y = s2y } -> intersection s2x1 s2x2 s2y s1x s1y1 s1y2
        | Horizontal { x = s2x; y1 = s2y1; y2 = s2y2 } -> if s1x = s2x then
                                                            match (closest_point_line s1y1 s1y2 s2y1 s2y2) with
                                                            | Some y -> Some (s1x, y)
                                                            | None -> None
                                                          else None
    )

let print_segment segment =
    match segment with
    | Vertical { x1; x2; y } -> printf "Vertical x1=%d, x2=%d, y=%d\n" x1 x2 y
    | Horizontal { x; y1; y2 } -> printf "Horizontal x=%d, y1=%d, y2=%d\n" x y1 y2

let print_point (x, y) = printf "(%d, %d)\n" x y

let task1 () = 
    let (first, second) = get_input() in
    let first_segments = compute_segments first in
    let second_segments = compute_segments second in
    let ans = List.map ~f:(fun seg -> List.map ~f:(segment_intersection seg) second_segments) first_segments |>
    List.concat |>
    List.filter ~f:is_some |>
    List.map ~f:(Option.value ~default:(0, 0)) |>
    List.map ~f:(fun (x, y) -> abs(x) + abs(y)) |>
    List.filter ~f:(fun x -> x > 0) |>
    List.min_elt ~compare |>
    Option.value ~default:(-1) in
    printf "%d\n" ans

let on_segment (seg : segment) ((px : int), (py : int)) : bool =
    let seg = set_right seg in
    match seg with
    | Vertical { x1; x2; y } -> y = py && x1 <= px && px <= x2
    | Horizontal { x; y1; y2 } -> x = px && y1 <= py && py <= y2

let segment_length (seg : segment) : int =
    match seg with
    | Vertical { x1; x2; y = _ } -> abs(x2 - x1)
    | Horizontal { x = _; y1; y2 } -> abs(y2 - y1)

let distance_on_segment (seg : segment) ((px : int), (py : int)) : int =
    match seg with
    | Vertical { x1; x2 = _; y = _ } -> abs(px - x1)
    | Horizontal { x = _; y1; y2 = _ } -> abs(py - y1)

let reach_dest (segments : segment list) (p : int * int) : int =
    let lengths = List.take_while ~f:(fun segment -> on_segment segment p |> not) segments |>
        List.map ~f:segment_length in
    let sum = List.fold_left ~f:(+) ~init:0 lengths in
    let last = List.nth segments (List.length lengths)
        |> Option.value ~default:(Vertical { x1 = 0; x2 = 0; y = 0})
        |> (fun seg -> distance_on_segment seg p) in
    sum + last


let task2 () =
    let (first, second) = get_input() in
    let first_segments = compute_segments first in
    let second_segments = compute_segments second in
    let inters = List.map ~f:(fun seg -> List.map ~f:(segment_intersection seg) second_segments) first_segments |>
    List.concat |>
    List.filter ~f:is_some |>
    List.map ~f:(Option.value ~default:(0, 0)) |>
    List.filter ~f:(fun (x, y) -> x <> 0 || y <> 0) in
    List.iter ~f:print_point inters;
    let ans = List.map ~f:(fun p -> reach_dest first_segments p + reach_dest second_segments p) inters |>
    List.min_elt ~compare |>
    Option.value ~default:(-1) in
    printf "%d\n" ans

let () = task2 ()