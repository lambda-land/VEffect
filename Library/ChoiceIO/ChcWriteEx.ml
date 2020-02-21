(*********************** Examples ***********************)

(* Example 1 a *)
let w_file = #Open_out "outputs/output1-1.eff" ;;
let s1 () = chc (Ref "A") (writeFile "Hello") (chc (Ref "B") (writeFile "Hi") (writeFile "Ghadeer"));;
let f1 () = write_h w_file s1;;
f1 ();;
#Close_out w_file;;


(* Example 1 b *)

let w_file = #Open_out "outputs/output1-2.eff" ;;
let s2 () = chc (Ref "A") ("Hello") (chc (Ref "B") ("Hi") ("Ghadeer"));;
let s () = let s' = s2 () in writeFile s';;
let f3 () = write_h w_file s;;

f3 ();;
#Close_out w_file;;

(* -------------------------------- *)

(* (* Example 2 *)
let w_file = #Open_out "outputs/output2.eff" ;;
let s1 () = chc (Ref "A") (writeFile w_file "Hello") (chc (Ref "B") (writeFile w_file "Hi") (writeFile w_file "Ghadeer"));;
let f2 () = write_calc_file w_file s1;;

f2 ();;
#Close_out w_file;; *)

(* -------------------------------- *)

(* Example 3 *)
let w_file = #Open_out "outputs/output3.eff" ;;
let s3 () = opt (Ref "A") (opt (Ref "B") (writeFile "Line1") );; 
let s4 () = chc (Ref "C") (writeFile "Line2") (chc (Ref "D") (writeFile "Line3") (writeFile "Line4"));;

let f3 () = write_h w_file s3;;
let f4 () = write_h w_file s4;;
[f3 ();f4 ()];;
(* with seq_handler handle
	seq (f3 ()) (f4 ())  *)

#Close_out w_file;;

(* -------------------------------- *)

(* Example 3 *)
let w_file = #Open_out "outputs/paper.eff" ;;
let line1 () = writeFile "Below is a breakdown of the preliminary housing costs for 2018-2019." ;;
let line2 () = opt (Ref "meal") (writeFile (chc (Ref "partial") ("$1,902") ("$3372")));;

write_h w_file line1; write_h w_file line2;;


#Close_out w_file;;

(*********************** End ***********************)





