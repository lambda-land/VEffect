(* ****************************** Opt ************************************************ *)



type 'a opt = ('a * ctx) ;; 


(* -- | The value that is optional. *)
(* val :: Opt a -> a *)
let value = fst ;;

(* -- | The presence condition of an optional value.
pc :: Opt a -> Ctx *)
let pc = snd;;


(* -- | Convert an optional value into a variational value with holes.
fromOpt :: Opt a -> V a *)
let fromOpt (a,c) = Chc (c, (One a), Hole);;


(* -- | Convert a variational value into a list of optional values.
toOpts :: V a -> [Opt a] *)
let toOpts = 
      let rec go c v = (match v with 
             Hole   -> []
          | (One a) -> [(a,c)]
          | (Chc (c', l,r) ) -> go (And (c, c')) l ++ go (And (c, (Not c'))) r)
          
      in go (Lit true);;