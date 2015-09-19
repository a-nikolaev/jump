open Printf 
open Ast

module M = Map.Make (struct type t = name let compare = compare end)

exception RT_TypeError of string
exception RT_PatternMismatch of string
exception RT_IllegalJump of string
exception RT_NotFound of name


type binding = Simple of value | Recursive of (value ref)

and context = binding M.t

and value = 
  | VClosure of (pattern * expr * context) 
  | VLit of lit
  | VTuple of (value tuple)

let add n v ctx = M.add n (Simple v) ctx
let add_recursive n v ctx = 
    let reference = ref v in
    (reference, M.add n (Recursive reference) ctx)

let rm n ctx = M.remove n ctx
let find n ctx = 
  try 
    ( match M.find n ctx with
      | Simple v -> v
      | Recursive rv -> !rv
    )
  with Not_found -> raise (RT_NotFound n)

type value_or_jump =
  | V of value
  | J of (label option * value option)
  
let rec try_unify p v ctx =
  match p, v with
  | PName n, v -> Some (add n v ctx)
  | PLit l1, VLit l2 when l1 = l2 -> Some ctx
  | PTuple pls, VTuple vls -> 
      let rec next ctx_acc = function
        | (p::pls, v::vls) -> (match try_unify p v ctx_acc with Some ctx2 -> next ctx2 (pls, vls) | None -> None)
        | [], [] -> Some ctx_acc
        | _ -> None
      in
      next ctx (pls, vls)
  | _ -> None

let unify p v ctx = match try_unify p v ctx with Some ctx2 -> ctx2 | None -> raise (RT_PatternMismatch "unify")

let match_branch ctx (b_ol, b_op, expr) (j_ol, j_ov) =
  if b_ol = j_ol then
    match b_op, j_ov with
    | Some b_p, Some j_v ->
        ( match try_unify b_p j_v ctx with
          | Some ctx2 -> Some (ctx2, expr)
          | None -> None
        )
    | None, None -> Some (ctx, expr)
    | _ -> None
  else
    None

let match_dispatch ctx disp jv =
  let rec next = function
    | b::tl -> (match (match_branch ctx b jv) with Some ctx2_expr -> Some ctx2_expr | None -> next tl)
    | [] -> None
  in
  next disp

let rec eval_binop ctx (op, e1, e2) =
  
  let process e1 e2 vmatcher operation opname =
    ( match eval_expr ctx e1 with
      | J x -> J x
      | V v ->
          ( match vmatcher v with
            | Some v1 -> 
                ( match eval_expr ctx e2 with
                  | J y -> J y
                  | V v ->
                      ( match vmatcher v with
                        | Some v2 -> V (operation (v1, v2)) 
                        | none -> raise (RT_TypeError opname)
                      )
                )
            | None -> raise (RT_TypeError opname)
          )
    )
  in

  match op with
  | BinOpPlus -> 
    process e1 e2 
      (function VLit (LInt x) as z -> Some z | _ -> None) 
      (function VLit (LInt x), VLit (LInt y) -> VLit (LInt (x + y)) | _ -> failwith "op") "+"

  | BinOpMinus -> 
    process e1 e2 
      (function VLit (LInt x) as z -> Some z | _ -> None) 
      (function VLit (LInt x), VLit (LInt y) -> VLit (LInt (x - y)) | _ -> failwith "op") "-"

  | BinOpEquals -> 
    process e1 e2 
      (function VLit v1 as z -> Some z | _ -> None) 
      (function VLit v1, VLit v2 -> VLit (LBool (v1 = v2)) | _ -> failwith "op") "=="
  
  | BinOpLess -> 
    process e1 e2 
      (function VLit (LInt x) as z -> Some z | _ -> None) 
      (function VLit (LInt x), VLit (LInt y) -> VLit (LBool (x < y)) | _ -> failwith "op") "<"
  
  | BinOpGreater -> 
    process e1 e2 
      (function VLit (LInt x) as z -> Some z | _ -> None) 
      (function VLit (LInt x), VLit (LInt y) -> VLit (LBool (x > y)) | _ -> failwith "op") ">"
  
  | BinOpLessEq -> 
    process e1 e2 
      (function VLit (LInt x) as z -> Some z | _ -> None) 
      (function VLit (LInt x), VLit (LInt y) -> VLit (LBool (x <= y)) | _ -> failwith "op") "<="
  
  | BinOpGreaterEq -> 
    process e1 e2 
      (function VLit (LInt x) as z -> Some z | _ -> None) 
      (function VLit (LInt x), VLit (LInt y) -> VLit (LBool (x >= y)) | _ -> failwith "op") ">="

and eval_jump ctx oo = J (preeval_jump ctx oo)

and preeval_jump ctx (ol, oe) =
  match oe with 
  | Some e -> 
      ( match eval_expr ctx e with
        | J x -> x
        | V v -> (ol, Some v)
      )
  | None -> (ol, None)

and eval_expr ctx = function
  | ELit lit -> V (VLit lit)
  | EName n -> V (find n ctx)
  | ETuple ls -> 
      let rec next acc = function
        | hd::tl -> ( match eval_expr ctx hd with V v -> next (v::acc) tl | J x -> J x )
        | [] -> V (VTuple (List.rev acc))
      in
      next [] ls
  | EBinOp x -> eval_binop ctx x

  | EFun (pat, expr) -> V (VClosure (pat, expr, ctx))
  | ELetIn (pat, e1, e2) ->
      
      (* make empty references in the context *)
      let names = all_names_in_pattern pat in
      let rev_refs, ctx_prepared = 
        List.fold_left (fun (ls,c) n ->
            let r, c' = add_recursive n (VTuple []) c in
            (r::ls, c')
          ) 
          ([], ctx) names
      in
      let refs = List.rev rev_refs in

      
      ( match eval_expr ctx_prepared e1 with
        | J x -> J x
        | V v ->
            let pseudo_ctx = unify pat v ctx in
            List.iter2 (fun n r -> r := find n pseudo_ctx) names refs;
            
            eval_expr ctx_prepared e2
      )
  | EApp (e1, e2) ->
      ( 
        match eval_expr ctx e1 with
        | J x -> J x
        | V (VClosure (pat, expr, clsctx)) -> 
            ( match eval_expr ctx e2 with
              | J y -> J y
              | V v2 -> 
                  let ctx2 = unify pat v2 clsctx in
                  eval_expr ctx2 expr
            )
        | _ -> raise (RT_TypeError "Applied expression is not a function")
      )
  | EJump jump -> eval_jump ctx jump

  | EJumpIn (jump, disp) ->
      
      let starting_jv = preeval_jump ctx jump in

      let rec loop current_jv = 
        match match_dispatch ctx disp current_jv with
        | Some (ctx2, expr) ->
            ( match eval_expr ctx2 expr with
              | J new_jv -> loop new_jv 
              | v -> v
            )
        | None -> J current_jv
      in
      loop starting_jv


let rec s_of_value ht = function
  | VLit lit -> s_of_lit ht 0 lit
  | VClosure _ -> "<closure>"
  | VTuple ls -> 
      let mid = List.fold_left (fun acc v -> acc ^ s_of_value ht v ^ ", ") "" ls in
      "(" ^ mid ^ ")"

let run_program ht dls =
  let ctx0 = M.empty in

  let run () =
    List.fold_left (fun ctx dcl ->
      match dcl with
      | DLet (pat, expr) ->
          (
            (* make empty references in the context *)
            let names = all_names_in_pattern pat in
            let rev_refs, ctx_prepared = 
              List.fold_left (fun (ls,c) n ->
                  let r, c' = add_recursive n (VTuple []) c in
                  (r::ls, c')
                ) 
                ([], ctx) names
            in
            let refs = List.rev rev_refs in

            match eval_expr ctx_prepared expr with
            | V v ->

                let pseudo_ctx = unify pat v ctx in
                List.iter2 (fun n r -> r := find n pseudo_ctx) names refs;
                
                printf "%s\n%!" (s_of_value ht v);
                
                ctx_prepared

            | J _ -> raise (RT_IllegalJump "run_program")
          )
    )     
    ctx0 dls
  in
  ignore (run ())


