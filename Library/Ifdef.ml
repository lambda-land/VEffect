let rec conjList l = 
            match l with 
            [] -> (Lit true)
            | (x::xs) -> simplifyCtx (And (x, conjList xs)) 
;;

let ctxPush d =
            let (s, l) = #Get() in 
            let l' = (d:: l) in
            #Set((conjList l', l'))            
;;
 
let ctxPop () =
            let (s, l) = #Get() in 
            match l with
                [] -> #Set((conjList l, l))
                | (x::l') -> #Set((conjList l', l'))            
;;

let ctxNeg () =
            let (s, l) = #Get() in 
            match l with
                [] -> #Set((conjList l, l))
                | (s'::l') -> 
                    let s'' = (Not s') in 
                    let l'' = (s''::l') in
                        #Set(( conjList l''  , l''))       
;;
 
    
let if_ d =
    ctxPush d
    
;;


let else_ () =
    ctxNeg ()
     
;;

let end_ () = 
    ctxPop ()
    
;;






  