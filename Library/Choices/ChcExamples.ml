
let x = 
	with state handle
		with choose_all handle
		    let ok = chc (Ref "A") true false in
	    	#Set 5;
			if ok then
	        	#Set (#Get() * 2)
	    	else
	        	()
			; #Get()
	;;


(********************** nested choices *********************)

(**************** choices addition ****************)

let ex1 = with choose_all handle
  chc (Ref "A") 10 (chc (Ref "A")  20 30) + chc (Ref "B")  1 (chc (Ref "A")  2 3)
;;


(********************** Variational lists ******************)

(**************** view a variational list *********)

let ex2 = with choose_all handle
let l = 
[Just 5; 
Nothing; 
Just 6; 
chc (Ref "A") Nothing (Just 10) ;
chc (Ref "B") Nothing (Just 30); 
chc (Ref "B") Nothing (chc (Ref "A") Nothing (Just 3))]

in 
viewList l 
;;

(*** calculate the length of a variational list ***)

let ex3 = with choose_all handle
let l = viewList [Just 5; 
Nothing; 
Just 6; 
chc (Ref "A") Nothing (Just 10) ;
chc (Ref "B") Nothing (Just 30); 
chc (Ref "B") Nothing (chc (Ref "A") Nothing (Just 3))]

in 
length l 
;;


(***** calculate the sum of a variational list ****)

let ex4 = with choose_all handle
let l = viewList 
[Just 5; 
Nothing; 
Just 6; 
chc (Ref "A") Nothing (Just 10) ;
chc (Ref "B") Nothing (Just 30); 
chc (Ref "B") Nothing (chc (Ref "A") Nothing (Just 3))]

in 
sum l 
;;







let ex6 = with choose_all handle
let l =  
[(1,Just 4890);
(2,chc (Ref "Economy") (Just 790) (chc (Ref "Business") Nothing (Just 4500))) ;
(3,Just 2876);
(4,chc (Ref "Economy") (Just 580) (chc (Ref "Business") (Just 3200) Nothing )) ;
(5,chc (Ref "Economy") Nothing (chc (Ref "Business")  (Just 3780)  (Just 5939)))]

in 
l
(* viewList l  *)
;;



selectV (Ref "B") (Chc ((Ref "B"),(One 2),(One 3)));;

selectV (Not(Ref "B")) (Chc ((Ref "B"),(One 2),(One 3)));;

selectV (Lit true) (Chc (Not(Ref "B"),(One 2),(One 3)));;


