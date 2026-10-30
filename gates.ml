open Hardcaml 
open Signal 

let nand a b = ~:(a &: b) 

let not_ a = 
nand a a 

let and_ a b = 
nand (nand a b) (nand a b)  

let or_ a b = 
nand (not_ a) (not_ b) 

let nor a b = 
not_ (or_ a b) 

let xor a b = 
let v = nand a b in 
nand (nand a v) (nand b v)


