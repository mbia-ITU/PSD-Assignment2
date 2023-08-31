(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr;;

type aexpr = 
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Sub of aexpr * aexpr
  | Mul of aexpr * aexpr;;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

let e4 = Prim("==", CstI 10, CstI 10);;

let e5 = Prim("min", CstI 10, CstI 100000);;

let e6 = Prim("max", CstI 10, CstI 100000);;

let e7 = If(Var "a", CstI 11, CstI 22);;

let a1 = Sub(Var "v", Add(Var "w", Var "z"));;

let a2 = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")));;

let a3 = Add(Var "x", Add(Var "y", Add(Var "z", Add(Var "v"))));;


(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim(ops, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ops with
        | "+" -> i1 + i2
        | "-" -> i1 - i2
        | "*" -> i1 * i2
        | "==" -> if (i1 = i2) then 1 else 0
        | "min" -> min i1 i2
        | "max" -> max i1 i2
        | _     -> failwith "unknown primitive"
    | If(e1, e2, e3) -> if (eval e1 env <> 0) then (eval e2 env) else (eval e3 env);;

let rec fmt a : string =
    match a with
    | CstI i -> sprintf "%d" i
    | Var x -> x
    | Add(i1, i2) -> sprintf "(%s + %s)" (fmt i1) (fmt i2)
    | Sub(i1, i2) -> sprintf "(%s - %s)" (fmt i1) (fmt i2)
    | Mul(i1, i2) -> sprintf "(%s * %s)" (fmt i1) (fmt i2);;

let rec simplify a = 
    match a with 
    | CstI i -> i
    | Var x -> x
    | Add(x1, x2) -> 
        let i1 = simplify x1
        let i2 = simplify x2
        match (i1, i2) with
        | (x, 0) -> x
        | (0, x) -> x
        | x -> x
    | Sub(x1, x2) -> 
        let i1 = simplify x1
        let i2 = simplify x2
        match (i1, i2) with
        | (x, 0) -> x
        | (0, x) -> -x
        | (x1, x2) when x1 = x2 -> 0
        | x -> x
    | Mul(x1, x2) -> 
        let i1 = simplify x1
        let i2 = simplify x2
        match (i1, i2) with
        | (x, 0) -> 0
        | (0, x) -> 0
        | (x, 1) -> x
        | (1, x) -> x
        | x -> x;;

let rec diff a diffvar = 
    match a with
    | CstI i -> CstI 0
    | Var x -> if (x = diffvar) then CstI 1 else CstI 0
    | Add(x1, x2) -> Add(diff x1 diffvar, diff x2 diffvar)
    | Sub(x1, x2) -> Sub(diff x1 diffvar, diff x2 diffvar)
    | Mul(x1, x2) -> Add(Mul(diff x1 diffvar, x2), Mul(diff x2 diffvar, x1));;

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;
let e4v = eval e4 env;;
let e5v = eval e5 env;;
let e6v = eval e6 env;;
let e7v = eval e7 env;;
