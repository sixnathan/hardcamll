open Hardcaml 
open Signal 

let halfadder a b = 
(a ^: b, a &: b)

let fulladder a b cin = 
let (s1, c1) = halfadder a b in 
let (sum2, c2) = halfadder s1 cin in 
(sum2, c1 |: c2) 
