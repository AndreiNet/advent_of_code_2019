open Core_kernel

let m, n = 25,6

let unconcat (s : string) ~(length : int) : string list =
    List.init (String.length s / length)
        ~f:(fun i -> String.slice s (i * length) ((i + 1) * length))

let get_input (m : int) (n : int) = 
    let line = In_channel.stdin |> In_channel.input_line |> Option.value ~default:"" in
    let layers = unconcat line ~length:(n * m) in
    layers

let count (s : string) ~(c : char) : int =
    String.filter ~f:(Char.(=) c) s |> String.length

let task1 () = 
    let layers = get_input m n in
    let fewest_zeroes =
        List.map layers ~f:(count ~c:'0')
        |> List.min_elt ~compare
        |> Option.value ~default:0 in
    let good_layers =
        List.filter ~f:(fun layer -> count layer ~c:'0' = fewest_zeroes) layers in
    let layer = match good_layers with
    | [] -> raise (Failure "Bad code")
    | hd :: [] -> hd
    | _ :: _ :: _ -> raise (Failure "More than one layer") in
    let ans = count layer ~c:'1' * count layer ~c:'2' in
    printf "%d\n" ans

let overlap_pixels (up : char) (down : char) : char =
    match up with
    | '2' -> down
    | _ -> up

let task2 () =
    let layer_size = m * n in
    let layers = get_input m n in
    let final_layer = String.init layer_size ~f:(fun _ -> '2')
        |> String.to_list |> ref in
    List.iter layers ~f:(fun layer ->
        final_layer := List.map2_exn !final_layer (String.to_list layer) ~f:overlap_pixels);
    let final_layer = String.of_char_list !final_layer in
    let final_layer_image = unconcat ~length:m final_layer in
    List.iter final_layer_image ~f:(fun image_line -> printf "%s\n" image_line)

let () = task2 ()