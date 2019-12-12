open Core_kernel

type point = { x : int; y : int }

let new_point ~(x: int) ~(y: int) : point = { x; y }

let equal_points (p : point) (q : point) : bool =
    p.x = q.x && p.y = q.y

let norm (p : point) : int = 
    p.x * p.x + p.y * p.y

let quarter (p : point) : int =
    if p.x = 0 && p.y = 0 then raise (Failure "Quarter")
    else if p.x >= 0 && p.y > 0 then 0
    else if p.x > 0 && p.y <= 0 then 1
    else if p.x <= 0 && p.y < 0 then 2
    else if p.x < 0 && p.y >= 0 then 3
    else raise (Failure "Bad code")

let sub_points (p : point) (q : point) : point =
    new_point ~x:(p.x - q.x) ~y:(p.y - q.y)

let cross_product (o : point) (a : point) (b : point) : int =
    (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)

let compare_angle (p : point) (q : point) : int =
    if quarter p <> quarter q then compare (quarter p) (quarter q)
    else cross_product (new_point ~x:0 ~y:0) p q

let compare_points (p : point) (q : point) : int =
    let angl = compare_angle p q in
    if angl <> 0 then angl
    else norm p - norm q

let get_input () =
    let lines = In_channel.stdin |> In_channel.input_lines in
    let points = List.mapi lines ~f:(fun x line -> String.to_list line |> List.mapi ~f:(fun y c ->
        if Char.(=) c '#' then Some(new_point ~x ~y) else None
    ))
    |> List.concat
    |> List.filter ~f:Option.is_some
    |> List.map ~f:(Option.value ~default:(new_point ~x:0 ~y:0)) in 
    points

let on_segment (a : point) (b : point) (o : point) : bool =
    if o.x < min a.x b.x ||
       o.x > max a.x b.x ||
       o.y < min a.y b.y || 
       o.y > max a.y b.y then false
    else if (o.x = a.x && o.y = a.y) || (o.x = b.x && o.y = b.y) then false
    else cross_product o a b = 0

let number_of_visible_points (points : point list) (o : point) : int =
    List.filter points ~f:(fun a ->
        List.filter points ~f:(on_segment o a)
            |> List.length |> ((=) 0)
    ) |> List.length

let task1 () =
    let points = get_input () in
    let ans = List.map points ~f:(number_of_visible_points points)
        |> List.max_elt ~compare |> Option.value ~default:0 in
    let ans = ans - 1 in
    printf "%d\n" ans

let task2 () =
    let points = get_input () in
    let val_max = List.map points ~f:(number_of_visible_points points)
        |> List.max_elt ~compare |> Option.value ~default:0 in
    let good_points = List.filter points
        ~f:(fun o -> number_of_visible_points points o = val_max) in
    let origin : point = match good_points with
    | [] -> raise (Failure "Bad code")
    | hd :: [] -> hd
    | _ :: _ :: _ -> raise (Failure "Multiple good points") in
    (* let origin = new_point ~x:3 ~y:8 in *)
    printf "%d %d\n" origin.x origin.y;
    let points = List.filter points ~f:(fun p -> equal_points origin p |> not)
        |> List.map ~f:(fun p -> sub_points p origin)
        |> List.map ~f:(fun { x; y} -> new_point ~x:y ~y:(-x))
        |> List.sort ~compare:compare_points in
    let processing = ref points in
    let follows = ref [] in
    let prev_point = ref (new_point ~x:(-1) ~y:(-1)) in
    let step = ref 0 in
    while not (List.is_empty !processing) do
        prev_point := new_point ~x:(-3234) ~y:(-3480);
        while not (List.is_empty !processing) do
            match !processing with
            | curr :: rem ->
                if compare_angle curr !prev_point = 0 then (
                    follows := curr :: !follows;
                ) else (
                    (* if !step <= 10 then (printf "%02d %02d -> %d %d\n" curr.x curr.y (origin.x - curr.y) (origin.y + curr.x)); *)
                    step := !step + 1;
                    if !step = 200 then (printf "Good: %d %d\n" (origin.x - curr.y) (origin.y + curr.x));
                    prev_point := curr
                );
                processing := rem
            | [] -> raise (Failure "Bad code")
        done;
        processing := List.rev !follows;
        follows := []
    done

let test () =
    printf "%d\n" (compare_angle (new_point ~x:1 ~y:8) (new_point ~x:0 ~y:3))

let () = task2 ()