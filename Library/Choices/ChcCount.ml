let rec sum l = 
	match l with 
		[] -> 0
	    | (x::xs) -> #Set(#Get()+1); x + sum xs
;;



#Print "***************************************************************\n";;

with state handle (*  EX1: no choices *)
	with choose_all handle (*  EX1: no choices *)

		#Print "**** no choices **** \n";
		let l = [5;9;7;6;8 ] in 
		(sum l ,#Get())
;;
#Print "***************************************************************\n";;

with state handle (*  EX1: no choices *)
	with choose_all handle (*  EX1: no choices *)

		#Print "**** 1 choice **** \n";
		let l = [5;9;7;chc (Ref "B") 5 6; chc (Ref "A") 11 10 ] in 
		(sum l ,#Get())
;;
#Print "***************************************************************\n";;
