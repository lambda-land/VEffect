(* ****************************** context ************************************************ *)


type dim = string;;
type config = dim list;;
type ctx = Lit of bool | Ref of dim | Not of ctx | And of ctx * ctx | Or of ctx * ctx ;;

(* dimsCtx :: Ctx -> Set Dim *)
let rec dimsCtx d = 
    match d with
    (Lit _ ) -> []
    | (Ref f ) -> [f]
    | (Not e) -> dimsCtx e
    | (And (l,r)) -> (dimsCtx l) ++ (dimsCtx r)
    | (Or (l,r)) -> (dimsCtx l) ++ (dimsCtx r)
 ;; 

 (* evalCtx :: Config -> Ctx -> Bool *)
 let rec evalCtx cfg c = 
     match c with
     (Lit b) -> b
     | (Ref d) -> mem d cfg
     | (Not e) -> not (evalCtx cfg e)
     | (And (l,r)) -> (evalCtx cfg l) && (evalCtx cfg r)
     | (Or (l,r)) -> (evalCtx cfg l) || (evalCtx cfg r)
 ;; 
 
 (* implies :: Ctx -> Ctx -> Ctx *)
 let implies a b = (Or (Not a,b))


 (* impliesNot :: Ctx -> Ctx -> Ctx *)
 let impliesNot a b = (Or (Not a,Not b)) 

            
 (* satCtx :: Ctx -> Bool *) 
 let satCtx e = 
     let cfgs = powerSet (rmDup (dimsCtx e)) in 
     let go b cfg = b || evalCtx cfg e  in
     fold_left go false cfgs
 ;;

 (* tautCtx :: Ctx -> Bool *)
 let tautCtx e=  
     let cfgs = powerSet (rmDup (dimsCtx e)) in 
     let go b cfg = b && evalCtx cfg e  in
     fold_left go true cfgs
 ;;

(* unsatCtx :: Ctx -> Bool *)
 let unsatCtx = compose not satCtx;;
 
 
 
 (* -- | Are two contexts equivalent?
 equivCtx :: Ctx -> Ctx -> Bool *)
let equivCtx l r = tautCtx (And (implies l r, implies r l));;

let doesImply a b = tautCtx (implies a b);;

 
 
(* simplifyCtx :: Ctx -> Ctx *)
let rec simplifyCtx v = 
    match v with 
    (Lit b) -> v
    | (Ref b) -> v
    | (Not e) -> 
    let e' = simplifyCtx e in
    if tautCtx e' then Lit false
    else if unsatCtx e' then Lit true
    else 
        (match e with 
        (Not e'') -> simplifyCtx e''
        | _ ->  (Not e')
        )
    | (And (l,r)) -> 
    let l' = simplifyCtx l in
    let r' = simplifyCtx r in
    if unsatCtx l' || unsatCtx r' then Lit false
    else if  equivCtx l' r' then l' 
    else if doesImply l' r' then l'
    else if doesImply r' l' then r'
    else 
        (match (tautCtx l', tautCtx r') with
            (true,true) -> Lit true
            |(true,false) -> r'
            |(false,true) -> l'
            |(false,false) -> (And (l',r'))
        )
    | (Or (l,r)) ->
    let l' = simplifyCtx l in
    let r' = simplifyCtx r in
    if tautCtx l' || tautCtx r' then Lit true
    else if  equivCtx l' r' then l'         
    else 
        (match (unsatCtx l', unsatCtx r') with
            (true,true) -> Lit false
            |(true,false) -> r'
            |(false,true) -> l'
            |(false,false) -> (Or (l',r'))  
        ) 
;;
   
   
let isNegCtx l r = (equivCtx  (Not l ) r) || (equivCtx  (Not r ) l);;
 
    
let rec printCtx ctx = match ctx with 
                 (Lit true) -> "True"
                | (Lit false) -> "False"
                | (Ref s) -> s
                | (And (l,r)) -> printCtx l ^ " & " ^  printCtx r
                | (Or (l,r)) -> printCtx l ^ " | " ^  printCtx r
                | (Not s) -> " ~ ( " ^ printCtx s ^ " ) "
            ;;
       
