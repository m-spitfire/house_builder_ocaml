open Minttea
open Leaves

type model = {
  floors_num : int;
  rooms_num : int;
  has_garage : bool;
  answering_stage : [ `Floors | `Rooms | `Garage | `Done ];
  text : Text_input.t;
}

let cursor = Cursor.make ()

let get_text_input stage =
  let prompt =
    match stage with
    | `Floors -> "Number of floors? "
    | `Rooms -> "Number of rooms? "
    | _ -> failwith "Shouldn't need prompt"
  in
  Text_input.make "" ~cursor ~prompt ()

let initial_answering_stage = `Floors

let initial_model =
  {
    floors_num = 0;
    rooms_num = 0;
    has_garage = false;
    answering_stage = initial_answering_stage;
    text = get_text_input initial_answering_stage;
  }

let init _model = Command.Noop

let update event model =
  match event with
  | Event.KeyDown Enter -> (
      match model.answering_stage with
      | `Floors ->
          ( {
              model with
              floors_num = Text_input.current_text model.text |> int_of_string;
              text = get_text_input `Rooms;
              answering_stage = `Rooms;
            },
            Command.Noop )
      | `Rooms ->
          ( {
              model with
              rooms_num = Text_input.current_text model.text |> int_of_string;
              answering_stage = `Garage;
            },
            Command.Noop )
      | `Garage -> ({ model with answering_stage = `Done }, Command.Quit)
      | _ -> failwith "shouldn't be here")
  | Event.KeyDown Left when model.answering_stage = `Garage ->
      ({ model with has_garage = false }, Command.Noop)
  | Event.KeyDown Right when model.answering_stage = `Garage ->
      ({ model with has_garage = true }, Command.Noop)
  | e ->
      let text = Text_input.update model.text e in
      ({ model with text }, Command.Noop)

let view model =
  match model.answering_stage with
  | `Done ->
      let open House_builder.House in
      house_builder () |> floors model.floors_num |> rooms model.rooms_num
      |> has_garage model.has_garage
      |> get_house
  | `Floors -> Text_input.view model.text
  | `Rooms ->
      Format.sprintf "Floors: %i\n%s" model.floors_num
      @@ Text_input.view model.text
  | `Garage ->
      Format.sprintf "Floors: %i\nRooms: %i\nHas garage: %s %s\n"
        model.floors_num model.rooms_num
        (Spices.(default |> bold (not model.has_garage) |> build) "false")
        (Spices.(default |> bold model.has_garage |> build) "true")

let app = Minttea.app ~init ~update ~view ()
let () = Minttea.start ~initial_model app
