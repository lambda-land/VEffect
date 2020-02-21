(*********************** Helper Functions 1 ***********************)
	

let empty = "";;
let is_empty x =  x = "" ;;
let is_default s =  s = (Lit true);;

let wr_if_ file = 
	let (s,_) = #Get() in
	if (is_default s)
		then ()
	else
		(#Write_file (file, "#if ");
		#Write_file (file, (printCtx s) ^ "\n"))
;;

let wr_if s file = 
	(#Write_file (file, "#if ");
	#Write_file (file, (printCtx s) ^ "\n"))
;;


let wr_else file = 
	#Write_file (file, "#else\n")
;;

let wr_end file = 
	#Write_file (file, "#endif\n")
;;

(*********************** Handlers ***********************)

let v_write_handler file = handler

	| #Choice s k ->
		wr_if s file;
		(k true);
		wr_else file;
		(k false) ;
		wr_end file 

	| #Write_file _ k ->  k ()
	| val x ->
		if (is_empty x) then () else #Write_file(file, x ^ "\n")
;;


let wr_if file = 
	let (s,_) = #Get() in
	if (is_default s)
		then ()
	else
		(#Write_file (file, "#if ");
		#Write_file (file, (printCtx s) ^ "\n"))
;;


let write_var_file_handler2 file = handler

	| #Choice s k -> 
		if_ s;
		(k true);
		else_ () ;
		(k false) ;
		end_ ()		
	| #Write_file(_, x) k ->  k ()
	| val x ->  
		if (is_empty x)
		then 
			() 
		else 
			(wr_if_ file;
			#Write_file(file, x ^ "\n");
			wr_end file)
;;



(*********************** Helpler Functions 2 ***********************)


let write_h file v =
	with state handle	
		with v_write_handler file handle
			v ()
;;


let dfile = #Open_out "outputs/dfile.eff" ;;

let writeFile t =  let _ = #Write_file( dfile, t) in t;; (* The key to the solution!!! *)

let opt d t = chc d t "" ;;


