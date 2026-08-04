open Ast

module StringMap = Map.Make(String)

type var_info = {
  var_type: var_type;
  is_initialized: bool;

}

type type_error =
  | UnboundVariable of string
  | TypeMismatch of string
  | InvalidOperation of string
  | InvalidAssignment of string
  | InvalidCondition of string
  | InvalidDeclaration of string

exception TypeError of type_error

let string_of_type_error = function
  | UnboundVariable s -> "Unbound variable: " ^ s
  | TypeMismatch s -> "Type mismatch: " ^ s
  | InvalidOperation s -> "Invalid operation: " ^ s
  | InvalidAssignment s -> "Invalid assignment: " ^ s
  | InvalidCondition s -> "Invalid condition: " ^ s
  | InvalidDeclaration s -> "Invalid declaration: " ^ s

type type_env = var_info StringMap.t

let create_env () = StringMap.empty

let add_variable env name var_type = 
  StringMap.add name {var_type; is_initialized = false} env



let update_variable env name = 
  let var_info = StringMap.find name env in
  StringMap.add name {var_info with is_initialized = true} env

let get_variable_type env name =
  try 
    let info = StringMap.find name env in
    if not info.is_initialized then
      raise (TypeError (InvalidOperation ("Variable " ^ name ^ " used before initialization")))
    else info.var_type
  with Not_found -> 
    raise (TypeError (UnboundVariable name))


(* Helper functions for vector/matrix operations *)
let get_vector_element_type exprs =
  match exprs with
  | [] -> None
  | e::_ -> Some (match e with
    | Int _ -> TInt
    | Float _ -> TFloat
    | _ -> raise (TypeError (TypeMismatch "Vector elements must be numeric")))

let get_vector_dimension exprs = List.length exprs

let check_vector_compatibility v1 v2 =
  let dim1 = get_vector_dimension v1 in
  let dim2 = get_vector_dimension v2 in
  if dim1 != dim2 then
    raise (TypeError (InvalidOperation 
      (Printf.sprintf "Vector dimensions don't match: %d vs %d" dim1 dim2)))
  else dim1

(* Get type of expression *)
let rec type_of_expr env = function
  | Int _ -> TInt
  | Float _ -> TFloat
  | Bool _ -> TBool
  | String _ -> TString
  | Var x ->  get_variable_type env x
  | Vectori exprs ->
    let elem_types = List.map (type_of_expr env) exprs in
    let dim = List.length exprs in
    if dim = 0 then TvectorI 0
    else if List.for_all (fun t -> t = TInt) elem_types then 
         TvectorI dim
    else 
         raise (TypeError (TypeMismatch "Vector elements must have same type"))
  | Vectorf exprs ->
    let elem_types = List.map (type_of_expr env) exprs in
    let dim = List.length exprs in
    if dim = 0 then TvectorF 0
    else if List.for_all (fun t -> t = TFloat) elem_types then 
         TvectorF dim
    else 
         raise (TypeError (TypeMismatch "Vector elements must have same type"))
  | Matrixi rows ->
    (let numRows = List.length rows in
    match rows with
    | [] -> TmatrixI (0, 0)
    | Vectori firstRow :: _ ->
         let numCols = List.length firstRow in
         if List.for_all (function 
              | Vectori r -> List.length r = numCols
              | _ -> false) rows
         then TmatrixI (numRows, numCols)
         else raise (TypeError (TypeMismatch "Matrix rows must have equal length"))
    | _ -> raise (TypeError (InvalidOperation "Invalid matrix format")))
  | Matrixf rows ->
    (let numRows = List.length rows in
    match rows with
    | [] -> TmatrixF (0, 0)
    | Vectorf firstRow :: _ ->
         let numCols = List.length firstRow in
         if List.for_all (function 
              | Vectorf r -> List.length r = numCols
              | _ -> false) rows
         then TmatrixF (numRows, numCols)
         else raise (TypeError (TypeMismatch "Matrix rows must have equal length"))
    | _ -> raise (TypeError (InvalidOperation "Invalid matrix format")))
  | DualOp (op, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match op with
      | "+" | "-" ->
          (match t1, t2 with
          | TInt, TInt -> TInt
          | TFloat, TFloat -> TFloat
          | TvectorI(d1), TvectorI(d2) when d1=d2 -> TvectorI(d1)
          | TvectorF(d1), TvectorF(d2) when d1=d2 -> TvectorF(d1)
          | TmatrixI(r1,c1), TmatrixI(r2,c2) when r1=r2 && c1=c2 -> TmatrixI(r1,c1)
          | TmatrixF(r1,c1), TmatrixF(r2,c2) when r1=r2 && c1=c2 -> TmatrixF(r1,c1)
          | TVectorI, TVectorI -> TVectorI
          | TVectorI, TvectorI _ -> TVectorI
          | TvectorI _, TVectorI -> TVectorI
          | TVectorF, TVectorF -> TVectorF
          | TVectorF, TvectorF _ -> TVectorF
          | TvectorF _, TVectorF -> TVectorF
          | TMatrixI, TMatrixI -> TMatrixI
          | TMatrixI, TmatrixI _ -> TMatrixI
          | TmatrixI _, TMatrixI -> TMatrixI
          | TMatrixF, TMatrixF -> TMatrixF
          | TMatrixF, TmatrixF _ -> TMatrixF
          | TmatrixF _, TMatrixF -> TMatrixF
          | _, _ -> raise (TypeError (InvalidOperation ("Invalid types for operator " ^ op))))
      | "*" ->
          (match t1, t2 with
          | TInt, TInt -> TInt
          | TFloat, TFloat -> TFloat
          | TInt, TvectorI(d1) | TvectorI(d1), TInt -> TvectorI(d1)  
          |TFloat, TvectorF(d1) | TvectorF(d1), TFloat -> TvectorF(d1)  
          | TvectorI(d1), TvectorI(d2) when d1=d2-> TInt 
          | TvectorF(d1), TvectorF(d2) when d1=d2-> TFloat 
          | TInt, TmatrixI(r1,c1) | TmatrixI(r1,c1), TInt -> TmatrixI(r1,c1)  
          | TFloat, TmatrixF(r1,c1) | TmatrixF(r1,c1), TFloat -> TmatrixF(r1,c1)  
          | TmatrixI(r1,c1), TmatrixI(r2,c2) when c1=r2 -> TmatrixI(r1,c2)
          | TmatrixF(r1,c1), TmatrixF(r2,c2) when c1=r2 -> TmatrixF(r1,c2)
          | TmatrixI(r,c), TvectorI(d) when c = d -> TvectorI(r)  
          | TmatrixF(r,c), TvectorF(d) when c = d -> TvectorF(r)
          | TInt, TVectorI -> TVectorI
          | TVectorI, TInt -> TVectorI
          | TFloat, TVectorF -> TVectorF
          | TVectorF, TFloat -> TVectorF
          | TVectorI, TVectorI -> TInt
          | TVectorI, TvectorI _ -> TInt
          | TvectorI _, TVectorI -> TInt
          | TVectorF, TVectorF -> TFloat
          | TVectorF, TvectorF _ -> TFloat
          | TvectorF _, TVectorF -> TFloat
          | TMatrixI, TVectorI -> TVectorI
          | TMatrixI, TvectorI _ -> TVectorI
          | TmatrixI _, TVectorI -> TVectorI
          | TMatrixF, TVectorF -> TVectorF
          | TMatrixF, TvectorF _ -> TVectorF
          | TmatrixF _, TVectorF -> TVectorF
          | TMatrixI, TMatrixI -> TMatrixI
          | TMatrixI, TmatrixI _ -> TMatrixI
          | TmatrixI _, TMatrixI -> TMatrixI
          | TMatrixF, TMatrixF -> TMatrixF
          | TMatrixF, TmatrixF _ -> TMatrixF
          | TmatrixF _, TMatrixF -> TMatrixF
          | _, _ -> raise (TypeError (InvalidOperation ("Invalid types for operator " ^ op))))
      | "/" ->
        (match t1,t2 with
        | TInt , TInt -> TInt
        | TFloat , TFloat -> TFloat
        | _ , _ -> raise (TypeError (InvalidOperation ("Invalid types for operator " ^ op))))
      | "angle" ->
          (match t1, t2 with
          | TvectorI(d1), TvectorI(d2) when d1=d2 -> TFloat
          | TvectorF(d1), TvectorF(d2) when d1=d2 -> TFloat
          | TVectorI, TVectorI -> TFloat  
          | TVectorF, TVectorF -> TFloat
          | _, _ -> raise (TypeError (InvalidOperation "Angle operation requires two vectors of same dimension")))
      | ">" | "<" | ">=" | "<=" ->
          (match t1, t2 with
          | TInt, TInt | TFloat, TFloat -> TBool
          | TInt , TFloat | TFloat , TInt -> TBool
          | _, _ -> raise (TypeError (InvalidOperation ("Invalid types for comparison"))))
      | "=" | "!=" ->
        (match t1, t2 with
          | TInt, TInt | TFloat, TFloat -> TBool
          | TInt , TFloat | TFloat , TInt -> TBool
          | TvectorI(d1) , TvectorI(d2) when d1=d2 -> TBool
          | TvectorF(d1) , TvectorF(d2) when d1=d2 -> TBool
          | TmatrixI(r1,c1) , TmatrixI(r2,c2) when r1=r2 && c1=c2 -> TBool
          | TmatrixF(r1,c1) , TmatrixF(r2,c2) when r1=r2 && c1=c2 -> TBool
          | TVectorI, TVectorI -> TBool 
          | TVectorF, TVectorF -> TBool
          | TMatrixI, TMatrixI -> TBool
          | TMatrixF, TMatrixF -> TBool
          | TVectorI, TvectorI _ -> TBool
          | TvectorI _, TVectorI -> TBool
          | TVectorF, TvectorF _ -> TBool
          | TvectorF _, TVectorF -> TBool
          | TMatrixI, TmatrixI _ -> TBool
          | TmatrixI _, TMatrixI -> TBool
          | TMatrixF, TmatrixF _ -> TBool
          | TmatrixF _, TMatrixF -> TBool
          | _, _ -> raise (TypeError (InvalidOperation ("Invalid types for comparison"))))
      | "&&" | "||" ->
          if t1 = TBool && t2 = TBool then TBool
          else raise (TypeError (InvalidOperation "Boolean operators require boolean operands"))
      | "%" ->
        (match t1,t2 with
        |TInt , TInt -> TInt
        |(_ , _ )-> raise (TypeError (InvalidOperation("Invalid types for remainder")))
        )
      | _ -> raise (TypeError (InvalidOperation ("Unknown operator " ^ op))))
  | SinOp (op, e) ->
      let t = type_of_expr env e in
      match op with
      | "not" -> 
          if t = TBool then TBool
          else raise (TypeError (InvalidOperation "not operator requires boolean operand"))
      | "abs" ->
          (match t with
          | TInt -> TInt
          | TFloat -> TFloat
          | _ -> raise (TypeError (InvalidOperation "Absolute value requires numeric operand")))
      | "dim" ->
          (match t with
          | TvectorI(d1) -> TInt
           | TvectorF(d1) -> TInt
           | TVectorI -> TInt   
    | TVectorF -> TInt
    | TMatrixI -> TInt
    | TMatrixF -> TInt
          | _ -> raise (TypeError (InvalidOperation "Dimension operation requires vector or matrix")))
      | "mag" ->
          (match t with
          | TvectorI(d1) -> TFloat
          | TvectorF(d1) -> TFloat
          | TVectorI -> TFloat  
          | TVectorF -> TFloat
          | TmatrixI (r1,c1) -> TFloat
          | TmatrixF (r1,c1) -> TFloat
          | TMatrixI -> TFloat
          | TMatrixF -> TFloat
          | _ -> raise (TypeError (InvalidOperation "Modulus operation requires vector")))
      | "trace" ->
        (
          match t with
          | TmatrixI (r1,c1) when r1=c1 -> TInt
          | TmatrixF (r1,c1) when r1=c1 -> TFloat
          | TMatrixI -> TInt  
          | TMatrixF -> TFloat
          | _ -> raise (TypeError(InvalidOperation "Trace requires square matrix"))

        )
      | "transpose" ->
          (match t with
          | TmatrixI(r1,c1) -> TmatrixI(c1,r1)
           | TmatrixF(r1,c1) -> TmatrixF(c1,r1) 
           | TMatrixI -> TMatrixI  
          | TMatrixF -> TMatrixF
          | _ -> raise (TypeError (InvalidOperation "Transpose requires matrix input")))
      | "det" ->
          (match t with
          | TmatrixI(r1,c1) when r1=c1 -> TInt
          | TmatrixF(r1,c1) when r1=c1 -> TFloat
          | TMatrixI -> TInt   
          | TMatrixF -> TFloat
          | _ -> raise (TypeError (InvalidOperation "Determinant requires square matrix input")))
      | "inv" ->
          (match t with
          | TmatrixI(r1,c1) when r1=c1 -> TmatrixI(r1,c1)
          | TmatrixF(r1,c1) when r1=c1 -> TmatrixF(r1,c1)
          | TMatrixI -> TMatrixI  
         | TMatrixF -> TMatrixF
          | _ -> raise (TypeError (InvalidOperation "Inverse requires square matrix input"))
          )
      | "sqrt" ->
        (
          match t with
          | TInt -> TFloat
          | TFloat -> TFloat
          | _ -> raise (TypeError (InvalidOperation "sqrt requires constants"))

        )
      | _ -> raise (TypeError (InvalidOperation ("Unknown unary operator " ^ op)))

(* Type check statements *)

let rec typecheck_stmt env = function
  | Declaration (declared_type, var, e) ->
    let expr_type = type_of_expr env e in
    let effective_declared_type =
         match declared_type, expr_type with
         | TVectorI, TvectorI d -> TvectorI d
         | TVectorF, TvectorF d -> TvectorF d
         | TMatrixI, TmatrixI (r, c) -> TmatrixI (r, c)
         | TMatrixF, TmatrixF (r, c) -> TmatrixF (r, c)
         | _ -> declared_type
    in
    if effective_declared_type = expr_type then
      let env' = add_variable env var effective_declared_type in  
      update_variable env' var
    else
      raise (TypeError (InvalidDeclaration 
        (Printf.sprintf "Variable %s declared as %s but initialized with %s" 
           var 
           (string_of_type declared_type) 
           (string_of_type expr_type))))
  | Assign (var, e) ->
    let var_type = get_variable_type env var in
    let expr_type = type_of_expr env e in
    (* Update the variable type with computed dimensions if applicable *)
    let effective_var_type =
         match var_type, expr_type with
         | TVectorI, TvectorI d -> TvectorI d
         | TVectorF, TvectorF d -> TvectorF d
         | TMatrixI, TmatrixI (r, c) -> TmatrixI (r, c)
         | TMatrixF, TmatrixF (r, c) -> TmatrixF (r, c)
         | _ -> var_type
    in
    if effective_var_type = expr_type then 
      update_variable env var
    else 
      raise (TypeError (InvalidAssignment 
        (Printf.sprintf "Cannot assign %s to variable %s of type %s" 
           (string_of_type expr_type) 
           var 
           (string_of_type var_type))))

  | If (cond, then_stmt, else_stmt) ->
      let cond_type = type_of_expr env cond in
      if cond_type <> TBool then
        raise (TypeError (InvalidCondition "If condition must be boolean"))
      else
        let _ = typecheck_stmt env then_stmt in
        let _ = typecheck_stmt env else_stmt in
        env

  | While (cond, body) ->
      let cond_type = type_of_expr env cond in
      if cond_type <> TBool then
        raise (TypeError (InvalidCondition "While condition must be boolean"))
      else
        let _ = typecheck_stmt env body in
        env

  | Loop (init, cond, incr, body) ->
      let env' = typecheck_stmt env init in
      let cond_type = type_of_expr env' cond in
      if cond_type <> TBool then
        raise (TypeError (InvalidCondition "Loop condition must be boolean"))
      else
        let _ = typecheck_stmt env' incr in
        let _ = typecheck_stmt env' body in
        env'

  | Block stmts ->
     List.fold_left typecheck_stmt env stmts

  | Print e ->
      let _ = type_of_expr env e in
      env

  | Input expr ->
      (match expr with 
      | None -> raise (TypeError (InvalidOperation "Input requires a string filename"))
      | Some e -> 
          (match type_of_expr env e with
           | TString -> env  (* Only allow string arguments for filename *)
           | _ -> raise (TypeError (InvalidOperation "Input requires a string filename"))))
  | InputAssign(typ, var) ->  (* input() with assignment *)
      let env' = add_variable env var typ in  
      update_variable env' var

  | Do stmt ->
      typecheck_stmt env stmt

  | Expr e ->
      let _ = type_of_expr env e in
      env
    

(* Main type checking function *)
let typecheck ast =
  let _ = typecheck_stmt (create_env()) ast in
  ()







