(******************************* read files help functions *******************************)

type fileCtx = L | R;;
    
let x = char " ";;

let is_keyword k str =
    let k' = ith_ele (split str x) 0 in k' = k
;;

let is_choice str = is_keyword "#if" str;;

let is_else str = is_keyword "#else" str ;;

let is_endif str = is_keyword "#endif" str;;

	
let rec getCh lineArr left = 
	
	let rec go xss left' = 
			 match xss with 
		 		[] -> ((Lit false),[])
		 		| (x::xs) -> 
					if (x =  "") then go xs left'
					else if (x =  ")") then (left',xs)					
					else let (c',a') = (getCh xss left') in go a' c'
				
	in 
	match lineArr with 
	[] -> (left,[])
	| (e::arr) -> 		
		if (e =  "") then getCh arr left
		else if (e =  "(") then go arr left
		else if (e = "~") then let (l',arr') = getCh arr left in (Not l',arr')
		else if (e = "&") then 
							let (r',arr') =  (getCh arr left) in 
							(And (left, r'), arr')
		else if (e = "|") then 
							let (r',arr') =  (getCh arr left) in 
							 (Or (left, r'),arr') 
		else 
			((Ref e),arr) 
    ;;
			 


let rec printlist l = 
	match l with 
	[] -> #Print("\n") 
	| (x::xs) -> #Print(x ^ ",") ; printlist xs
;;
		
let rec get_choice lineArr left = 

	(* printlist lineArr; *)
	let (c,l) =  getCh lineArr left in
	match l with 
		[] -> c
		| _ -> get_choice l c
	
    (* getCh lineArr 1 (length lineArr) *)
	
;;


let get_ctx s = 
	let xss = (split  (trim (s)) x) in 
	match xss with 
		[] -> (Lit false)
		| (x::xs)-> get_choice xs (Lit true);; 


let is_semicolon str =
    
    let col = char ";" in    
    if (string_length str) <= 0 then false
    else
    if  col =  (get str ((string_length str) -1)) then true else false

;; 


                                          
let delete_last str = ith_ele (split str (char ";" )) 0;;


                      
     
(******************************* read files help functions *******************************)




