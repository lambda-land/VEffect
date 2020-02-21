let (++) = (@);;

type 'a maybe = Just of 'a | Nothing

let rec concatMap f l = match l with
[]    -> []
| x::xs -> f x ++ (concatMap f xs)
;;

let double x = 2 * x;;

let func x = [x;x+7;x+9];;
let flip f x y   =  f y x;;

let rec reversed lx =
    match lx with
    [] -> []
    | (x::xs) -> reversed xs ++ [x]
;;

let rec findMin min ls = match ls with
[] -> min
| (x::xs) ->
            if x < min then findMin x xs
            else  findMin min xs
 ;;

let rec remove_i x ls =
    match ls with
    [] -> []
    | (y::xs) ->  match x = y with
            true -> xs
            | false -> y :: remove_i x xs
;;

let rec sort ls  =
    match ls with
    [] -> []
    | (x::xs) -> let min' = findMin x ls in
                 let lx = remove_i min' ls in
                 (min' :: sort lx  )
;;

let rec powerSet l =
  match l with
  | [] -> [[]]
  | x :: xs -> let l = powerSet xs in
                 l @ (map (fun y -> x :: y) l)

;;

let rec doesExist s ls = match ls with
            [] -> false
            | (x::xs) -> x = s || doesExist s xs
;;


let rec rmDup ls = match ls with
            [] -> []
            | (x::xs) -> if doesExist x xs then rmDup xs else (x:: rmDup xs)
;;


type commpare = EQ | LT | GT | NE;;

let compare d1 d2 =
if d1 = d2 then EQ
else if d1 < d2 then LT
else if d1 > d2 then GT
else NE
;;


let fmap f v =
    match v with
    Nothing  -> Nothing
    | Just x -> Just (f x)
;;


let rec length l = match l with 
	     [] -> 0
	     | x::xs -> 1 + length xs		
;;


let rec sum l = match l with 
	     [] -> 0
	     | x::xs -> x + sum xs		
;;

let rec viewList l = 
	match l with 
	     [] -> []
	     | x::xs -> match x with 
			Just a ->  [a] ++ viewList xs
			| Nothing -> [] ++ viewList xs		
;;

let rec ith_ele l i =
    match l, i with
      (x::xs), 0  -> x
    | (x::xs), n  -> ith_ele xs (n-1)
;;




 