(*
   CS:3820 Programing Language Concepts
   
   Homework 2

   Team:  <Yanchuan Guan, Junhyuk Kang>
*)


(* Problem Set 1 *)

type oper = Neg | Not | Add | Mul | Sub | Less | Eq | And

type expr = 
  | C of int
  | Op1 of oper * expr
  | Op2 of oper * expr * expr
  | If of expr * expr * expr


// size : expr -> int
let rec size (e : expr) : int =
    match e with
        | C(h) -> 1
        | Op1(h, t) -> size(t) + 1
        | Op2(h, m, t) -> size(m) + size(t) + 1
        | If(h, m, t) -> size(h) + size(m) + size(t) + 1



// subexpressions : expr -> expr list

let rec subexpressions (e:expr) : list<expr> =
    match e with
        | C(_) -> []
        | Op1(h, t) -> ([t;] @ subexpressions(t))
        | Op2(h, m, t) -> ([m; t;] @ subexpressions(m) @ subexpressions(t))
        | If(h, m, t) -> ([h; m; t;]@ subexpressions(h)@ subexpressions(m)@ subexpressions(t)) 
    
// normAdd : expr -> expr
let rec normAdd (e:expr) : expr = 
    match e with
        | C(h) -> C(h)
        | Op1(h, t) -> Op1(h, t)
        | Op2(Add, Op2(Add,b,c), t) -> Op2(Add, normAdd(b), Op2(Add, normAdd(c), t))
        | If(h, m,  t) -> If(h,m,t)
        | Op2(h, m, t) -> Op2(h,m,t)

        
 //       | Op1(h, t) -> (subexpressions(t))
  //      | Op2(h, m, t) -> (subexpressions(m);)
 //       | If(h, m, t) -> (subexpressions(h); subexpressions(m); subexpressions(t);)  

                
       


// some examples to try
let e1 = Op2 (Add, Op2 (Add, C 1, C 2), C 3)
let e2 = Op2 (Add, Op2 (Add, C 1, C 2), Op2 (Add, C 3, C 4))
let e3 = Op2 (Add, Op2 (Add, C 1, C 2), Op2 (Mul, C 3, Op2 (Add, C 4, C 5)))
let test_expr1 = Op1(Not, Op2(Add, Op1(Neg, C 3),C 3))



(* Problem Set 2 *)

type sInstr =
  | SC of int
  | SAdd 
  | SSub
  | SMul 
  | SNeg
  | SLess
  | SIfze of int
  | SJump of int


// scomp : expr -> sInstr list
let rec eval (e : expr) : int =
  match e with
    | C n                -> n
    | If (e1, e2, e3)    -> if (eval e1) = 0 then eval e3 else eval e2
    | Op1 (Neg, e1)      -> -(eval e1)
    | Op1 (Not, e1)      -> if (eval e1) = 0 then 1 else 0 
    | Op2 (Add, e1, e2)  -> (eval e1) + (eval e2)
    | Op2 (Mul, e1, e2)  -> (eval e1) * (eval e2)
    | Op2 (Sub, e1, e2)  -> (eval e1) - (eval e2)
    | Op2 (Less, e1, e2) -> if (eval e1) < (eval e2) then 1 else 0
    | Op2 (Eq, e1, e2)   -> if (eval e1) = (eval e2) then 1 else 0
    | Op2 (And, e1, e2)  -> if (eval e1) = 0 || (eval e2) = 0 then 0 else 1
    | _ -> failwith "Operator applied to wrong number of operands" 


let rec scomp (e : expr) : sInstr list =
  match e with
  | C n               -> [SC n]
  | Op1 (Neg, e1)     -> (scomp e1) @ [SNeg]
  | Op1 (Not, e1)     -> (scomp e1) @ [SIfze 0] @ [SC 1] @  [SJump 1] @ [SC 0]
  | Op2 (Add, e1, e2) -> (scomp e1) @ (scomp e2) @ [SAdd]
  | Op2 (Mul, e1, e2) -> (scomp e1) @ (scomp e2) @ [SMul]
  | Op2 (Sub, e1, e2) -> (scomp e1) @ (scomp e2) @ [SSub]
  | Op2 (Less, e1, e2)-> (scomp e1) @ (scomp e2) @ [SLess] @ [SC 1] @  [SJump 1] @ [SC 0]
  | Op2 (Eq, e1, e2)  -> [SC (List.length (scomp e1))] @ [SIfze (List.length (scomp e2))] @ [SC 1] @ [SJump 1] @ [SC 0]
  | Op2 (And, e1, e2) -> [SC (List.length (scomp e1))] @ [SIfze (List.length (scomp e2))] @[SIfze 0] @ [SC 1] @ [SJump 1] @ [SC 0]  
  | If (e0 , e1, e2)  -> (scomp e0) @[SIfze 0] @(scomp e3) @ [SJump 1] @ (scomp e2)
  | Op1 (a, b)        -> failwith "illed form: binaray operator used in Op1"
  | Op2 (a, b, c)     -> failwith "illed form: binary operator used in Op2"

   
// drop : int -> 'a list -> 'a list
let rec drop n l = 
  match l with
  |[] -> []
  |e::l when (n = 0) -> e::l
  |e::l when (n > 0) -> drop (n-1) l
  |_ -> failwith "Out of bound"


// seval : sInstr list -> int list -> int
let rec seval (inss : sInstr list) (stack : int list) : int =
  match (inss, stack) with 
  | (SC    n :: sis,  s            )  ->  seval sis (n :: s)
  | (SAdd    :: sis,  n2 :: n1 :: s)  ->  seval sis ((n1 + n2) :: s)
  | (SSub    :: sis,  n2 :: n1 :: s)  ->  seval sis ((n1 - n2) :: s)
  | (SMul    :: sis,  n2 :: n1 :: s)  ->  seval sis ((n1 * n2) :: s)
  | (SNeg    :: sis,        n1 :: s)  ->  seval sis (-n1 :: s)
  | (SLess   :: sis,  n2 :: n1 :: s)  ->
    if SC n2> SC n1 then
      seval sis (1 :: 1 :: s)
    else
      seval sis (0 :: 0 :: s)           
  | (SIfze n :: sis, n1 :: s)  ->
    if SC n1 = SC 0 then
      seval sis (drop n s)
    else
      seval sis (s)
  | (SJump n :: sis,  n1::n2::n3::n4::s)  ->  
        if n2 = n1 then
            seval sis (n3::s)
        else 
            seval sis (n4::s)
  | ([] , n::_ )  ->  n
  | ([] , [])  -> failwith "seval: no result on stack!"
  | (_ ) -> failwith "seval: too few operands on stack"
  

// run : sInstr list -> int
let run (p : sInstr list) : int = seval p []


// byteCode : sInstr list -> string
let rec byteCode(l : list<sInstr>) : string = 
     match l with 
        
        | (SC n) :: t  ->  "0 " + n.ToString() + " " + byteCode(t)
        | SAdd :: t  ->  "1 " + byteCode(t)
        | SSub :: t  ->  "2 " + byteCode(t)
        | SMul :: t  ->  "3 " + byteCode(t)
        | SNeg :: t  ->  "4 " + byteCode(t)
        | SLess :: t  ->  "5 " + byteCode(t)
        | SIfze h :: t  ->  "6 " + h.ToString() + byteCode(t)
        | SJump h :: t  ->  "7 " + h.ToString() + byteCode(t) 
        | E -> ""

// beval : int list -> int list -> int
let rec beval (l : list<int>) (stack: list<int>) : int =  
    match (l, stack) with 
        | (0 :: ris, s) -> beval ris (0 :: s)
        | (1 :: ris, n2::n1::s) -> beval ris ((n1+n2) ::s)
        | (2 :: ris, n2::n1::s) -> beval ris ((n1-n2) ::s)
        | (3 :: ris, n2::n1::s) -> beval ris ((n1*n2) ::s)
        | (4 :: ris, n1::s)     -> beval ris (-n1::s)
        | (5 :: n3 :: ris, n2::n1::s) -> 
            if n2> n1 then
                beval ris (1::s)
            else 
                beval ris (0::s)
        | (6 :: ris, n2::n1::s) -> 
            if n2 = n1 then
                beval ris (1::s)
            else
                beval ris (0::s)
        | (7 :: ris, n1::s) -> beval ris (n1 :: s) 
        | (n :: ris, s) -> beval ris (n::s)
        | (E, s) -> 0

   

// parse : string -> int list
let parse (p : string) : int list =  
  let l = Seq.toList (p.Split ' ') in
  List.map System.Int32.Parse l


