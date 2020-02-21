let enqueue' q t c = enqueue c (One t) q;;

let get_maybe_next_line skip file = 
	match skip with 
        (Just nextLine) ->  nextLine
        | (Nothing) -> trim (#Read_line file)
;;

let get_next_line file = 
	trim (#Read_line file)
;;


let is_eof text = text = "EOF" ;;


let rec read file q text ctx =

    if is_choice text then
			let ctx' = get_ctx text in 
			read_vline file q ctx ctx'
        else  
			enqueue' q text (Lit true)
			
and read_insert file q line ctx ctx'= 

	if is_choice line then (*line is what's after the choice line- what's in it?*)
		read file q line ctx'
	else 
		enqueue' q line (And (ctx',ctx)) 
				 
and	read_vline file q ctx ctx' = 

	let line = get_next_line file in (*line is what's after the choice line- what's in it?*)
	
	if is_endif line then 
		q
	else if is_else line then 
		read_vline file q ctx (Not ctx')
	else
		let q' = read_insert file q line ctx ctx' in
		read_vline file q' ctx ctx'		
;;


let rec read_v_ r_file q =
    let text = get_next_line r_file in
    if is_eof text 
		then q 
	else
    	let q' = read r_file q text (Lit true) 
		in read_v_ r_file q'
;;


let read_v r_file =
    read_v_ r_file emptyQueue
;;

(****************************************************************************)




