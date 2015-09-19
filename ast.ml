open Base

type name_ext = string
type name = int
type label = name

type binop = 
  | BinOpPlus | BinOpMinus | BinOpEquals 
  | BinOpLess | BinOpGreater
  | BinOpLessEq | BinOpGreaterEq

type 'a tuple = 'a list

type lit =
  | LInt of int
  | LBool of bool

type pattern =
  | PName of name
  | PLit of lit
  | PTuple of (pattern tuple)

and expr =
  | ELit of lit
  | EName of name
  | ETuple of expr tuple
  | EBinOp of (binop * expr * expr)

  | EFun of (pattern * expr)

  | ELetIn of (pattern * expr * expr)
  | EApp of (expr * expr)
  
  | EJump of jump
  | EJumpIn of (jump * dispatch)

and jump = (label option) * (expr option)
  
and dispatch = branch list

and branch = (label option * pattern option * expr)

type decl =
  | DLet of pattern * expr


let all_names_in_pattern p = 
  let rec collect acc = function
  | PName n -> n :: acc
  | PLit _ -> acc
  | PTuple ls -> List.fold_left (fun acc p -> collect acc p) acc ls
  in
  collect [] p

(* printing *)
let indent lvl = String.make (lvl * 2) ' '
let break lvl = "\n" ^ indent lvl

let s_of_name ht lvl name = Hashtbl.find ht name

let s_of_label ht lvl label = Hashtbl.find ht label

let rec s_of_lit ht lvl = function
  | LInt i -> string_of_int i
  | LBool true -> "true"
  | LBool false -> "false"

let s_of_binop lvl = function 
  | BinOpPlus -> "+"
  | BinOpMinus -> "-"
  | BinOpEquals -> "=="
  | BinOpLess -> "<"
  | BinOpGreater -> ">"
  | BinOpLessEq -> "<="
  | BinOpGreaterEq -> ">="

let rec s_of_decl ht lvl = function
  | DLet (p,e) -> "let " ^ s_of_pattern ht lvl p ^ " = " 
      ^ break (lvl+1) ^ s_of_expr ht (lvl+1) e
      ^ break lvl

and s_of_expr ht lvl = function
  | ELit lit -> s_of_lit ht lvl lit 
  | EName n -> s_of_name ht lvl n
  | ETuple ls -> "(" ^ List.fold_left (fun acc lit -> acc ^ ", " ^ s_of_expr ht lvl lit) "" ls ^ ")"
  | EBinOp (op, e1, e2) -> "(" ^ s_of_expr ht lvl e1 ^ " " ^ s_of_binop lvl op ^ " " ^ s_of_expr ht lvl e2 ^ ")"
  | EFun (p, e) -> "(fun " ^ s_of_pattern ht lvl p ^ " -> " ^ s_of_expr ht (lvl+1) e ^ ")"
  | ELetIn (p, e1, e2) ->
      "let " ^ s_of_pattern ht lvl p ^ " = " 
        ^ break (lvl+1) ^ s_of_expr ht (lvl+1) e1
        ^ break lvl ^ "in"
        ^ break lvl ^ s_of_expr ht lvl e2
  | EApp (e1, e2) -> s_of_expr ht lvl e1 ^ " " ^ s_of_expr ht lvl e2
  | EJump j -> s_of_jump ht lvl j 
  | EJumpIn (j, d) -> s_of_jump ht lvl j ^ "where"
      ^ s_of_dispatch ht (lvl+1) d 
      ^ break lvl ^ "end"

and s_of_jump ht lvl (ol, oe) =
    ">> " ^
    (match ol with Some l -> s_of_label ht lvl l ^ " " | None -> "") ^ 
    (match oe with Some e -> s_of_expr ht lvl e ^ " " | None -> "")

and s_of_pattern ht lvl = function
  | PName n -> s_of_name ht lvl n
  | PLit lit -> s_of_lit ht lvl lit
  | PTuple ls -> "(" ^ List.fold_left (fun acc pat -> acc ^ ", " ^ s_of_pattern ht lvl pat) "" ls ^ ")"

and s_of_dispatch ht lvl brls =
  List.fold_left (fun acc br -> acc ^ break lvl ^ s_of_branch ht lvl br) "" brls

and s_of_branch ht lvl (ol, op, e) =
  "| " ^ 
  (match ol with Some l -> s_of_label ht lvl l ^ " " | None -> "") ^ 
  (match op with Some p -> s_of_pattern ht lvl p ^ " " | None -> "") ^ 
  "-> " ^ s_of_expr ht (lvl + 1) e

