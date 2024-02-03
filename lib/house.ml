type t = { floors : int; rooms : int; has_garage : bool }

let house_builder () = { floors = 0; rooms = 0; has_garage = false }
let floors n h = { h with floors = n }
let rooms n h = { h with rooms = n }
let has_garage n h = { h with has_garage = n }

let garage_chars height row has_garage =
  match (height - row, has_garage) with
  | _, false -> ""
  | 3, _ -> " __ "
  | 2, _ -> "/__\\"
  | 1, _ -> "|🚘|"
  | _, _ -> "    "

let rec dup_str s = function 0 -> "" | n -> s ^ dup_str s (n - 1)

let get_house model =
  let width =
    (model.rooms / model.floors)
    + if model.rooms mod model.floors <> 0 then 1 else 0
  in
  let char_width = (width * 2) + 1 in
  let height = model.floors + width + 1 in
  let get_row_chars n =
    let gchars = garage_chars height n model.has_garage in
    let house_chars =
      match n with
      | 0 -> String.make width ' ' ^ "_"
      (* (2*width + 1) - (2 * (width -n) + 2) = 2n - 1  *)
      | n when n < width ->
          String.make (width - n) ' '
          ^ "/"
          ^ String.make ((2 * n) - 1) ' '
          ^ "\\"
      | n when n = width -> "/" ^ String.make (char_width - 2) '_' ^ "\\"
      | n when height - n > model.floors - (model.rooms mod model.floors) ->
          "|" ^ dup_str "_|" width
      | _ -> "|" ^ dup_str "_|" (width - 1) ^ "#|"
    in
    gchars ^ house_chars
  in

  let row_chars = List.init height get_row_chars in
  String.concat "\n" row_chars
