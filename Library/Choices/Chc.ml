let selectV (Lit true) v = v;; 
let rec selectV s v = 
	match s with
	(Lit true) -> v
	| _ ->
		match v with 
			(Chc (ctx,l,r)) -> 
				if unsatCtx (And (ctx,s)) then
					selectV s r
				else
					selectV s l
			| _ -> v
;;


(******************************* choices library *******************************)
	 
effect Choice : ctx -> bool;;

let choose_all = handler
	| #Choice s k -> 
         
	(* calculate the value and remove the choice if it exist *)
		
		let l = select (s) (k true) in  
   		let r = select (Not s) (k false) in
		prepend s l @ prepend (Not s) r (* add a choice *)
	
        
    | val x -> [(Lit true,x)]
;;

let chc d l r = if #Choice d then l else r;;

let private d l r = if #Choice d then (Just l) else Nothing;;

(*******************************************************************************)


let rec makeChc v = match v with
      (Hole) ->  ""
      | (One a) ->  a
      | (Chc (a,l,r)) ->  (chc a (makeChc l) (makeChc r))
  ;;
  
  
  
  (******************************* choices library *******************************)
	 