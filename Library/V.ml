(* ****************************** V ************************************************ *)

 type 'a v = Hole | One of 'a | Chc of ctx  *  ('a v) * ('a v) ;;
 

 (* getVCtx :: Ctx -> Ctx -> Ctx *)
 let rec getVCtx ctx ctx' = 
     match ctx with
 	    (Lit a ) -> ctx
 	    | (Ref f ) -> if tautCtx (implies ctx' ctx) then (Lit true) else ctx
 	    | (Not e) -> Not (getVCtx e ctx') 
 	    | (And (l,r)) -> And (getVCtx l ctx',getVCtx r ctx') 
 	    | (Or (l,r)) -> Or (getVCtx l ctx',getVCtx r ctx') 
		| _ -> ctx
 ;;	

 (* getVChc :: (V a) -> Ctx -> V a *)
 let rec getVChc v ctx' = 
 	match v with 
         (Chc (ctx, l, r)) -> (Chc (getVCtx ctx ctx', getVChc l ctx', getVChc r ctx' ))
         | _ -> v
 ;;             
	

 (* get :: (V a) -> Ctx -> V a  *)
 let rec getV v ctx' =
	 match v with
        (Chc (ctx, l, r)) ->
			if tautCtx (implies ctx' ctx)
            	then getV l ctx'
            else if tautCtx (impliesNot ctx' ctx)
                then getV r ctx'
            else (Chc (ctx ,getV l ctx',getV r ctx') )
        | _ -> v
;;


 (* evalV :: Config -> V a -> Maybe a *)
 let rec  evalV cfg v = 
     match v with 
       Hole         -> Nothing
     | (One a )     -> Just a
     | (Chc (c,l,r) ) -> evalV cfg (if evalCtx cfg c then l else r)
 ;;  

 (* maskV :: Ctx -> V a -> V a *)
 let rec maskV  m v =
     match v with 
     (Chc (c,l,r)) -> 
         let c' = simplifyCtx (And (m,c)) in
         let l' = maskV m l in
         let r' = maskV m r in 
         
         if unsatCtx (And (m, c)) then r'
         else if unsatCtx (And (m, Not c)) then l'
         else if l' = r' then l'  
         else 
             (match c' with 
                 Not c'' ->  maskV m (Chc (c'',r,l))
                 | _ -> (Chc (c',l,r))
             )
     | _ -> v
 ;;

let simplifyV = maskV (Lit true) ;; 
 
 (* simplifyV2 :: Eq a => V a -> V a *)
let  rec simplifyV2 v = match v with 
        (Chc (c,l,r)) -> (Chc (c,(simplifyV2 (getV l c)),(simplifyV2 (getV r (Not c)))))
        | _ ->  simplifyV v
;;


(* simplifyV3  :: Eq a => (V a) -> (V a) *)
let rec simplifyV3 v = match v with 
   (Chc (ctx,l,r)) -> 
              ( match (l,r) with 
             (Chc _ , Chc _  ) -> simplifyV3_ ctx l r
             | (_,_) -> v )
    | _ -> v 
    
and 
(* simplifyV3_ :: Eq a => Ctx -> (V a) -> (V a)  -> (V a)  *) 
simplifyV3_ ctx l r = 
    match (l,r) with 
        (Chc (ctx1,l1,r1), Chc (ctx2,l2,r2) ) -> 
                 if ctx1 = ctx2 then
                     if r1 = r2 then (Chc (ctx1,(simplifyV3 (Chc (ctx, l1, l2))),(simplifyV3 r1) ))
                     else if l1 = l2 then (Chc (ctx1,(simplifyV3 l1),(simplifyV3 (Chc (ctx, r1, r2))) ))
                     else (Chc (ctx,l,r))
                 else (Chc (ctx,l,r))
        | (_,_)-> (Chc (ctx,l,r))                           
;;


(* evalCh :: (V a) -> (V a) -> Ctx -> (V a) *)
let rec evalCh v' v ctx = match v' with 
                (Chc (ctx',l,r)) -> (Chc (ctx', (evalCh l v (And (ctx',ctx))), (evalCh r v (And ((Not ctx'), ctx)))))
                | _ ->  getV v ctx
;;

(* constChc_ :: [Dim] -> (V a) *)
let rec constChc_ l = match l with 
                [] -> Hole
                | (x::xs) -> Chc (Ref x,constChc_ xs, constChc_ xs) 
;;


(* chcDims :: (V a) -> Config *)
let rec chcDims v = match v with 
                (Chc (ctx,l,r)) -> rmDup (dimsCtx ctx ++ chcDims l ++ chcDims r)
                | _ -> []
;;       
         
(* constChc :: (V a) -> (V a) *)
let constChc v = match v with 
                (Chc (ctx,l,r)) -> let em = constChc_ (chcDims v) in (evalCh em v (Lit true))
                | _ -> v
;;


(* simpAll' :: Eq a => (V a) -> (V a) *)
let simpAll' v = simplifyV (simplifyV3 (constChc v) )
;;
 
 
(* simpAll :: Eq a => Maybe (V a) -> Maybe (V a) *)
let simpAll m = match m with 
         Nothing -> Nothing
         | (Just v) -> Just (simpAll' v)
;;


let rec printV v =
    match v with
    (One s) -> s
    | (Chc (d, l, r)) -> "Ch(" ^ printCtx d ^ ")<" ^ printV l ^ " , " ^ printV r ^ ">"
    | Hole -> "_"
;;

