(******************************* state library *******************************)

	 
effect Get: unit -> int;;
effect Set: int -> unit;;
effect Count: unit -> unit;;

let state = handler
  | val y -> (fun _ -> y)
  | #Get () k -> (fun s -> k s s)
  | #Set s' k -> (fun _ -> k () s')
  | finally g -> g 0
;;
(******************************* state library *******************************)
