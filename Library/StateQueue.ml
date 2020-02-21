(******************************* state library *******************************)

	 
effect GetQueue: unit -> ((string * ctx) list) ;;
effect SetQueue: ((string * ctx) list) -> unit;;

let stateQueue = handler
  | val y -> (fun _ -> y)
  | #GetQueue () k -> (fun s -> k s s)
  | #SetQueue s' k -> (fun _ -> k () s')
  | finally g -> g []
;;
(******************************* state library *******************************)
