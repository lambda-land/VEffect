(******************************* help funcitons ********************************)

let rec prepend s l =  
     match l with
            [] -> []
            | (chc, ival)::ls -> 
                ( simplifyCtx (And (chc,s)), ival) :: (prepend s ls)    
;;
  

let selectChoice f cs = 
	match cs with 
	(ctx, v) -> if unsatCtx (And (ctx,f)) then [] else [cs]
		
;;
	
let rec select f l = match l with 
        	     [] -> []
        	     | x::xs -> (selectChoice f x) ++ (select f xs)
;;



(*
let rec prepend_ s l added =
     match l with
            [] -> if added then ([],true) else (l,true)
            | (chc, ival)::ls ->
				if unsatCtx (And (s,chc))
							then prepend_ s ls added
							else
								let (rest,b) = (prepend_ s ls true) in
								(((simplifyCtx (And (chc,s)), ival) :: rest) , true)
;;


let rec prepend2 s l  = let (l,_) = prepend_ s l false in l;; *)
