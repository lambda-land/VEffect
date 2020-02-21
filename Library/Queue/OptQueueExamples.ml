

let s1 = enqueue (Lit true) (Chc (Ref "A", One 2, One 3)) []
let s2 = enqueue (Not (Ref "A")) (One 4) s1
let (p1,s1') = dequeue (Ref "A") s1
let (p2,s2') = dequeue (Ref "A") s2
let (p3,s3') = dequeue (Lit true) s2
let (p4,s4') = dequeue (Not (Ref "A")) s3'
let (p5,s5') = dequeue (Ref "B") s1
let (p6,s6') = dequeue (Not (Ref "B")) s5'
let (p7,s7') = dequeue (And (Ref "A",Not (Ref "B")) ) s5'




