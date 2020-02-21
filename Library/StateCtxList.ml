(******************************* state library *******************************)

	 
effect Get: unit -> (ctx * ctx list );;
effect Set: (ctx *  ctx list) -> unit;;

let state = handler
  | val y -> (fun _ -> y)
  | #Get () k -> (fun s -> k s s)
  | #Set s' k -> (fun _ -> k () s')
  | finally g -> g (Lit true,[])
;;
(******************************* state library *******************************)
