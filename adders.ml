open Hardcaml 
open Signal 

let nand a b = ~:(a &: b) 

let and_ a b = 
nand (nand a b) (nand a b)  

let xor_ a b = 
let v = nand a b in 
nand (nand a v) (nand b v)

let halfadder a b = 
(xor_ a b, and_ a b) 

let fulladder a b cin = 
let (s1, c1) = halfadder a b in 
let (sum2, c2) = halfadder s1 cin in 
(sum2, xor_ c1 c2) 

let fourbit a b = 
let (s1, c1) = fulladder (bit a 0) (bit b 0) gnd in 
let (s2, c2) = fulladder (bit a 1) (bit b 1) c1 in 
let (s3, c3) = fulladder (bit a 2) (bit b 2) c2 in 
let (s4, c4) = fulladder (bit a 3) (bit b 3) c3 in 
concat_msb [c4; s4; s3; s2; s1]  

let eightbit a b = 
let (s0, c0) = fulladder (bit a 0) (bit b 0) gnd in 
let (s1, c1) = fulladder (bit a 1) (bit b 1) c0 in 
let (s2, c2) = fulladder (bit a 2) (bit b 2) c1 in 
let (s3, c3) = fulladder (bit a 3) (bit b 3) c2 in 
let (s4, c4) = fulladder (bit a 4) (bit b 4) c3 in 
let (s5, c5) = fulladder (bit a 5) (bit b 5) c4 in 
let (s6, c6) = fulladder (bit a 6) (bit b 6) c5 in 
let (s7, c7) = fulladder (bit a 7) (bit b 7) c6 in 
concat_msb[c7; s7; s6; s5; s4 ;s3; s2; s1; s0] 

let eightmuller a b = 
let r0 = concat_msb [repeat gnd 4; and_ (repeat (bit b 0) 4) a] in 
let r1 = concat_msb [repeat gnd 3; and_ (repeat (bit b 1) 4) a; gnd] in   
let r2 = concat_msb [repeat gnd 2; and_ (repeat (bit b 2) 4) a; repeat gnd 2] in  
let r3 = concat_msb [gnd; and_ (repeat (bit b 3) 4) a; repeat gnd 3] in 
let p1 = select (eightbit r0 r1) 7 0 in 
let p2 = select (eightbit p1 r2) 7 0 in 
let p3 = select (eightbit p2 r3) 7 0 in 
p3 




