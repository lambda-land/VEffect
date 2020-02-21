(* ****************************** queue ************************************************ *)


type 'a queue = ('a opt ) list;; 

(* -- | Simplify a queue by filtering out the irrelevent values and simplifying
--   each presence condition.
simplifyQueue :: 'a queue -> 'a queue *)
let rec simplifyQueue l = match l with 
            [] -> []
            | ((a,c)::s) -> 
                if unsatCtx c then simplifyQueue s
                else (a, simplifyCtx c) :: simplifyQueue s
            ;;
			
(* -- | Enqueue a variational value onto the queue.
enqueue :: Ctx -> V a -> 'a queue -> 'a queue *)
(* val enqueue : a' v -> 'a queue -> 'a queue;; *)
let enqueue c v q = simplifyQueue (q ++ toOpts (Chc (c, v, Hole)))
;;

(* -- | Helper function for dequeue, takes a context representing the holes we still
--   have to fill, the queue, and the return value constructed so far.
dequeue_ :: Eq a => Ctx -> 'a queue-> V a -> (V a, 'a queue) *)
let rec dequeue_ cIn q v = 
	if unsatCtx cIn 
		then (v, q)
	else 
	    match q with 
	    [] -> (v, [])
	    | ((a,cElem)::oq) -> 
	        let cDeq =   simplifyCtx (And (cIn, cElem)) in (* part of elem to Deq  *)
	        let cRem =  simplifyCtx (And (Not cIn, cElem)) in  (* part of elem to remain on queue *)
	        let cToDo =   simplifyCtx (And ( cIn, Not cElem)) in  (* holes still to fill *)
	        let  (v', q') = dequeue_ cToDo oq  (Chc (cDeq, (One a), v) ) in    
	        (* let  (v', s') = dequeue_ cToDo os (simpAll' (Chc (cPop, (One a), v) )) in              *)
	        (v', (a,cRem)::q')
;;
    

(* -- | Dequeue a variational value from the queue.
dequeue :: Eq a => Ctx -> 'a queue -> (V a, 'a queue) *)
let dequeue c q = let (v', q') = dequeue_ c q Hole in (getVChc (maskV c v') c, simplifyQueue q')
;;

let emptyQueue = [];;


let rec printQueue q =
	match q with
	[] -> #Print("\n")
	| ((v,c)::xs) -> #Print("(" ^ v ^ "," ^ printCtx c ^ "); "); printQueue xs
;;
