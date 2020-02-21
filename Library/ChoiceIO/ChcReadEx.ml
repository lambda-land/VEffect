(*********************** Examples 1 ***********************)

let r_file = #Open_in "inputs/fileR2.eff" ;;
let queue1 = read_v r_file;;
#Close_in r_file;;

(****** example 1 *****)


let a1 () = readLine () ;;
let a2 () = chc (Ref "A") (readLine () ) (readLine ()) ;;

let f1 () =
	let f1 =  read_line (a1 ) in
	let f2 =  read_line (a2 ) in
	let f3 =  read_line (a1 ) in

	with choose_all handle
	let a1' = reflect_v(f1) in (* Welcome *)
	let a2' = reflect_v(f2) in (* A <<C<C_T, C_F>>,Foo> *)
	let a3' = reflect_v(f3) in (* A <Bye,Heee> *)

	 a1' ^ " * " ^ a2' ^  " * " ^ a3'
;;

read_lines queue1 f1;; 

(* ***** example 2 *****)

let a2_1 () = opt (Ref "A") (readLine ()) ;;
let a2_2 () = opt (Not (Ref "A")) (readLine ()) ;;

let f2 () =

	let f1 =  read_line (a1 ) in
	let f2_1 = read_line (a2_1) in
	let f2_2 = read_line (a2_2) in
	let f3 =  read_line (a1 ) in

	let r () =
		let a1' =  reflect_v (f1) in (* Welcome *)
		let a2_1' =  reflect_v (f2_1) in (* <C<C_T, C_F>> *)
		let a2_2' = reflect_v (f2_2) in	(* Foo *)
		let a3' = reflect_v (f3) in (* A <Welcome,Heee> *)

   	 	a1' ^ " * " ^ a2_1' ^  " * " ^ a2_2' ^  " * "^ a3'

	in reify_v r ;;


read_lines queue1 f2;;

(****** example 3 *****)

let r_file = #Open_in "outputs/output1-1.eff" ;;
let queue2 = read_v r_file;;
#Close_in r_file;;


let f3 () =

	let f1 =  read_line (a1 ) in
	let x () = reflect_v f1 in
	reify_v ( x)
;;
 

read_lines queue2 f3;;


(*********************** Examples for paper **********************)

let r_file = #Open_in "outputs/paper.eff" ;;
let queue = read_v r_file;;
#Close_in r_file;;

let line1 () = readLine () ;;
let line2 () = opt (And (Ref "meal" , Ref "partial")) (readLine ()) ;;
let line3 () = opt (And (Ref "meal", Not (Ref "partial"))) (readLine ()) ;;

let f () =
	read_line (line1 );
	read_line (line2 );
	read_line (line3 )
;;

read_lines queue f;;


let line1 () = readLine () ;;
let line2 () = opt (Ref "meal") (readLine ()) ;;

let g () =
	let v_res1 = read_line (line1 ) in
	let v_res2 = read_line (line2 ) in

	let r () =
		let a1 =  reflect_v (v_res1) in
		let a2 =  reflect_v (v_res2) in

   	 	a1 ^ "  " ^ a2

	in reify_v r
;;

read_lines queue g;;


(* *********** *)

let r_file = #Open_in "inputs/paper3.eff" ;;
let queue = read_v r_file;;
#Close_in r_file;;


let reify_o = reify_v;;
let reflect_o = optToChc;;


let checkDocs w s =
  let words = concatMap (fun str -> split str x) s in
  let matches = filter (fun a -> a = w) words in
  length matches
;;

let r s =
	let s' () = map (fun v -> (reflect_o v)) s in
	let f ()  = checkDocs "$13,360" (s' ()) in
	reify_o f
;;

r queue ;;



(*********************** End **********************)


let r_file = #Open_in "inputs/paper2.eff" ;;
let queue = read_v r_file;;
#Close_in r_file;;






