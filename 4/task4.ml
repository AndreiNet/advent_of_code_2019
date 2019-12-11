open Core_kernel

let get_input () = (245182, 790572)

let digits (x : int) : int list =
    let digs = ref [] in
    let x = ref x in
    while !x > 0 do
        digs := (!x mod 10) :: !digs;
        x := !x / 10
    done;
    !digs

let good_digits1 (digs : int list) : bool =
    let rec asc (digs : int list) : bool =
        match digs with
        | [] | _ :: [] -> true
        | a :: b :: tl -> a <= b && asc (b :: tl) in
    let rec has_two_eq (digs : int list) : bool =
        match digs with
        | [] | _ :: [] -> false
        | a :: b :: tl -> a = b || has_two_eq (b :: tl) in
    asc digs && has_two_eq digs

let good_digits2 (digs : int list) : bool =
    let rec has_two_eq (digs : int list) (before_eq : bool): bool =
        match digs with
        | [] | _ :: [] -> false
        | a :: b :: [] -> not before_eq && a = b
        | a :: b :: c :: tl -> if not before_eq && a = b && b <> c
                               then true
                               else has_two_eq (b :: c :: tl) (a = b) in
    good_digits1 digs && has_two_eq digs false

let task (good_digits : int list -> bool) =
    let (low, high) = get_input() in
    let numbers = List.init 10_000_000 ~f:ident
        |> List.filter ~f:(fun x -> low <= x && x <= high) in
    let numbers = List.filter ~f:(fun x -> digits x |> good_digits) numbers in
    printf "%d\n" (List.length numbers)


let () = task good_digits2