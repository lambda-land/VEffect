let read_line f ctx = dequeue ctx f;;

(*********************** Handlers ***********************)

let read_var_line_handler = handler
	| #Choice s k ->
		if_ s;
		let l = (k true) in
		else_ () ;
		let r = (k false) in
		end_ ();
		if (r = Hole )
			then l
		else
			(Chc (s,l,r))
		  
	 | #Read_line file k -> k "c"
		 
	 | val x -> 
		 if (x = "") then Hole
	 	 else 
			(let l =  #GetQueue() in
	  	    let (s',ls) = #Get() in
	  	    let (a,l') = read_line l (s') in
	  	    #SetQueue(l');  a)

;;

(*********************** Helpler Functions ***********************)


let dfile = #Open_in "outputs/dfile.eff" ;;
let readLine () =  #Read_line dfile;;

let read_line f = 
	with read_var_line_handler handle
			f ()
;;

let optToChc v = match v with
            (a,ctx) -> (chc ctx a "")
    ;;
	
let reflect_v = makeChc;;

let reify_v f = 
	with choose_all handle 
	f()
;;

let read_lines stack f =
	with stateQueue handle
		#SetQueue(stack) ;
		with state handle
			f ()

;;

(*********************** End ***********************)

let opt d t = chc d t ""