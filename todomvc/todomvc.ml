
module Storage = struct

  let storage =
    match Js_browser.Window.local_storage Js_browser.window with
    | None -> failwith "Storage is not supported by this browser"
    | Some v -> v

  let key = "ocaml-vdom-todo-state"

  let find () =
    Js_browser.Storage.get_item storage key

  let set v = Js_browser.Storage.set_item storage key v

  let init default =
    match find () with
    | None -> set default ; default
    | Some v -> v

end

module Model = struct

  type visibility =
      Completed | Active | All
  [@@deriving json]

  type task = {
    description : string ;
    (* backup field keep the previous description to restore it when ESC key is pressed *)
    backup : string ;
    completed : bool ;
    editing : bool ;
    id : int ;
  } [@@deriving json]

  type t = {
    tasks : task list ;
    field : string ;
    uid : int ;
    visibility : visibility ;
  } [@@deriving json] (* to save/restore the state in JSON *)

  let empty = {
    tasks = [] ;
    field = "" ;
    uid = 0 ;
    visibility = All ;
  }

  let new_task desc id = {
    description = desc ;
    backup = desc ;
    completed = false ;
    editing = false ;
    id = id
  }

  let string_of_visibility v =
    match v with
    | Completed -> "Completed"
    | Active -> "Active"
    | All -> "All"

  let from_json s =
    Deriving_Json.from_string [%json: t] s

  let to_json m =
    Deriving_Json.to_string [%json: t] m

end

module Action = struct

  type action =
    | Update_field of string
    | Editing_task of (int * bool)
    | Update_task of (int * string)
    | Add
    | Delete of int
    | Delete_complete
    | Check of (int * bool)
    | Check_all of bool
    | Change_visibility of Model.visibility
    | Escape of int
    | Nop

end

module Controller = struct

  let update m a =
    let open Action in
    let open Model in
    let m =
      match a with
      | Add ->
        let uid = m.uid + 1 in
        let tasks =
          let v = String.trim m.field in
          if v = "" then m.tasks
          else (new_task v m.uid) :: m.tasks
        in
        { m with uid = uid; field = "" ; tasks = tasks }
      | Update_field field ->
        { m with field }
      | Editing_task (id, is_edit) ->
        let update_task t =
          if (t.id = id) then
            let v = String.trim t.description in
            if is_edit then
              { t with editing = is_edit ; backup = v }
            else
              { t with editing = is_edit ; backup = "** should never be displayed **" }
          else
            { t with editing = false }
        in
        let l = List.map update_task m.tasks in
        let l = List.filter (fun e -> e.description <> "") l in
        { m with tasks = l }
      | Update_task (id, task) ->
        let update_task t =
          if (t.id = id) then { t with description = task }
          else t
        in
        { m with tasks = List.map update_task m.tasks }
      | Delete id ->
        { m with tasks = List.filter (fun e -> e.id <> id) m.tasks }
      | Delete_complete ->
        { m with tasks = List.filter (fun e -> not e.completed) m.tasks }
      | Check (id, is_compl) ->
        let update_task t =
          if (t.id = id) then { t with completed = is_compl }
          else t
        in
        { m with tasks = List.map update_task m.tasks }
      | Check_all is_compl ->
        let update_task t =
          { t with completed = is_compl }
        in
        { m with tasks = List.map update_task m.tasks }
      | Change_visibility visibility ->
        { m with visibility = visibility }
      | Escape id ->
        (* let _ = Js_browser.Console.log Js_browser.console (Ojs.string_to_js (Printf.sprintf "Escape: id=%u" id)) in *)
        let unedit_task t =
          if (t.id = id) then
            (* let _ = Js_browser.Console.log Js_browser.console (Ojs.string_to_js (Printf.sprintf "Escape: backup=%s" t.backup)) in *)
            { t with editing = false ; description = t.backup }
          else t
        in
        { m with tasks = List.map unedit_task m.tasks }
      | Nop -> m
    in
    Storage.set @@ Model.to_json m ;
    m

end

module View = struct

  open Model
  open Action

  (* New task input field *)
  let task_input m =
    Vdom.(input ~a:[
        class_ "new-todo" ;
        str_prop "placeholder" "What needs to be done?" ;
        autofocus ;
        value m.field ;
        oninput (fun s -> Update_field s) ;
        onkeydown
          (fun e ->
             match e.which with
             | 13 -> Add
             | _ -> Nop)
      ] [])

  let task_entry m =
    Vdom.(elt "header" ~a:[class_ "header"] [
        elt "h1" [ text "todos" ];
        task_input m
      ])

  (* One item in the tasks list *)
  let todo_item m (todo:Model.task) =
    let input_check =
      Vdom.(elt "input" ~a:(
          let l = [
            str_prop "type" "checkbox" ;
            class_ "toggle" ;
            onclick (fun _ ->
              Check (todo.id, (not todo.completed))
            )]
          in if todo.completed then str_prop "checked" "checked" :: l else l
        ) [])
    in

    let key_handler e =
      let _ = Js_browser.Console.log Js_browser.console (Ojs.int_to_js e.Vdom.which) in
      if e.Vdom.which = 13 then (
        Editing_task (todo.id, false)
      )
      else if e.Vdom.which = 27 then (
        (* /!\ bug with Firefox 50 (works ok with Chromium 54) /!\
           The esc key is detected (which = 27) but the value is not reset to backup (??!!!)
           If the esc key is replaced by the "a" key for instance, it works ok with Firefox.
        *)
        Action.Escape todo.id
      )
      else
        Action.Nop
    in

    let input_edit m =
      Vdom.(input ~a:[
          class_ "edit" ;
          value todo.description ;
          str_prop "id" (Printf.sprintf "todo-%u" todo.id) ;
          onblur (Editing_task (todo.id, false)) ;
          oninput (fun s -> Update_task (todo.id, s)) ;
          onkeydown (fun e -> key_handler e) ;
        ] [])
    in

    let css_class =
      let l = if todo.completed then ["completed"] else [] in
      let l = if todo.editing then "editing"::l else l in
      String.concat " " l
    in

    Vdom.(elt "li" ~a:[class_ css_class] [
        div ~a:[class_ "view"] [
          input_check;
          elt "label" ~a:[ondblclick (fun _ -> Editing_task (todo.id, true)) ;
                         ] [text todo.description] ;
          elt "button" ~a:[class_ "destroy"; onclick (fun _ ->
              Delete todo.id ;
            ) ] []
        ];
        input_edit m;
      ])

  (* Build the tasks list *)
  let task_list m =
    let css_visibility tasks =
      match tasks with
      | [] -> "hidden"
      | _ -> "visible"
    in
    let toggle_input_checked tasks =
      List.for_all (fun e -> e.Model.completed) tasks
    in
    let list_of_visible_tasks m =
      let is_visible todo =
        match m.Model.visibility with
        | Model.Completed -> todo.Model.completed
        | Active -> not todo.completed
        | All -> true
      in
      List.filter is_visible m.Model.tasks
    in
    Vdom.(elt "section" ~a:[class_ "main"; style "visibility" (css_visibility m.Model.tasks)] [
        elt "input"
          ~a:( (if toggle_input_checked m.Model.tasks then [str_prop "checked" "checked"] else []) @ [
              type_ "checkbox" ;
              class_ "toggle-all" ;
              onclick (fun _ -> Check_all (not (toggle_input_checked m.Model.tasks))) ;
            ]) [] ;
        (* /!\ need "attr" function for the "label" attribute "for" /!\ *)
        elt "label" ~a:[attr "for" "toggle-all"] [text "Mark all as complete"] ;
        elt "ul" ~a:[class_ "todo-list"] (List.map (todo_item m) (list_of_visible_tasks m))
      ])

  let visibility_swap m acc (uri, visibility) =
    let css =
      if visibility = m.Model.visibility then "selected" else ""
    in
    Vdom.(elt "li" [
        elt "a" ~a:[str_prop "href" uri; class_ css; onclick (fun _ ->
            Change_visibility visibility)]
          [text (Model.string_of_visibility visibility)]
      ]) :: acc

  let controls m =
    let footer_hidden tasks =
      match tasks with
      | [] -> true
      | _ -> false
    in
    let button = Vdom.([class_ "clear-completed"; onclick (fun _ ->
        Delete_complete
      )]) in
    let button_hidden tasks =
      let tasks_completed, _ = List.partition (fun e -> e.Model.completed) tasks in
      match tasks_completed with
      | [] -> true
      | _ -> false
    in
    let nb_left tasks =
      let _, tasks_left = List.partition (fun e -> e.Model.completed) tasks in
      string_of_int (List.length tasks_left)
    in
    let item_left tasks =
      let _, tasks_left = List.partition (fun e -> e.Model.completed) tasks in
      if (List.length tasks_left = 1) then " item left" else " items left"
    in
    let vswap m =
      List.rev(List.fold_left (visibility_swap m) []
                 [("#/", Model.All); ("#/active", Model.Active); ("#/completed", Model.Completed)])
    in
    let html =
      Vdom.(elt "footer" ~a:((class_ "footer") ::
                             (if footer_hidden m.Model.tasks then [str_prop "hidden" "hidden"] else [])) [
              elt "span" ~a:[class_ "todo-count"] [
                elt "strong" [text (nb_left m.Model.tasks)] ;
                text (item_left m.Model.tasks)
              ];
              elt "ul" ~a:[class_ "filters"] (vswap m) ;
              elt "button"
                ~a:((if button_hidden m.Model.tasks then [str_prop "hidden" "hidden"] else []) @ button) [
                text "Clear completed"
              ];
            ])
    in
    html

  let info_footer =
    Vdom.(elt "footer" ~a:[class_ "info"] [
        elt "p" [text "Double-click to edit a todo"] ;
        elt "p" [text "TodoMVC with ocaml-vdom"] ;
      ])

  (* Build the HTML for the application *)
  let view m =
    Vdom.(
      div ~a:[class_ "todomvc-wrapper"] [
        elt "section" ~a:[class_ "todoapp"] [
          task_entry m ;
          task_list m ;
          controls m
        ] ;
        info_footer
      ])

end

let app =
  (* restore the saved state or empty state if not found *)
  let m = Model.from_json @@ Storage.init @@ Model.to_json Model.empty in
  (* set the visibility by looking at the current url *)
  let m =
    match Js_browser.Location.get_hash () with
    | "" -> m
    | hash ->
      match hash with
      | "#/" -> { m with Model.visibility = Model.All }
      | "#/active" -> { m with Model.visibility = Model.Active }
      | "#/completed" -> { m with Model.visibility = Model.Completed }
      | _ -> m
  in
  Vdom.simple_app ~init:m ~view:View.view ~update:Controller.update ()

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Js_browser.Element.append_child (Js_browser.Document.body Js_browser.document)

let () = Js_browser.Window.set_onload Js_browser.window run
