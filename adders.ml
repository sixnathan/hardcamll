open Hardcaml 
open Signal 

let halfadder a b = 
(a ^: b, a &: b)

let fulladder a b cin = 
let (s1, c1) = halfadder a b in 
let (sum2, c2) = halfadder s1 cin in 
(sum2, c1 |: c2) 


let fourbit a b = 
let (s1, c1) = fulladder (bit a 0) (bit b 0) gnd in 
let (s2, c2) = fulladder (bit a 1) (bit b 1) c1 in 
let (s3, c3) = fulladder (bit a 2) (bit b 2) c2 in 
let (s4, c4) = fulladder (bit a 3) (bit b 3) c3 in 
concat_msb [c4; s4; s3; s2; s1]  

let muller a b = 

