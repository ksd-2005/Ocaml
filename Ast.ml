type var_type =
  | TInt
  | TFloat
  | TBool
  | TString
  | TVectorI
  | TVectorF
  | TMatrixI
  | TMatrixF
  | TvectorI of int
  | TvectorF of int
  | TmatrixI of int * int
  | TmatrixF of int * int


type expr =
  | Int of int
  | Float of float
  | Bool of bool
  | Var of string
  | Vectori of expr list
  | Vectorf of expr list
  | Matrixi of expr list
  | Matrixf of expr list
  | DualOp of string * expr * expr
  | SinOp of string * expr
  | String of string

type stmt =
  | Declaration of var_type * string * expr
  | Assign of string * expr
  | Print of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Loop of stmt * expr * stmt * stmt  
  | Do of stmt
  | Block of stmt list
  | Expr of expr
  | Input of expr option
  | InputAssign of var_type * string


let string_of_type = function
  | TInt -> "TInt"
  | TFloat -> "TFloat"
  | TBool -> "TBool"
  | TString -> "TString"
  | TVectorI -> "TVectorI"
  | TVectorF -> "TVectorF"
  | TMatrixI -> "TMatrixI"
  | TMatrixF -> "TMatrixF"
  | TvectorI(d1) -> "TVectorI"
  | TvectorF(d1) -> "TVectorF"
  | TmatrixI(r1,c1) -> "TMatrixI"
  | TmatrixF(r1,c1) -> "TMatrixF"

let rec string_of_expr = function
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Var s -> "Var(" ^ s ^ ")"
  | Vectori es -> "vectori([" ^ String.concat "; " (List.map string_of_expr es) ^ "])"
  | Vectorf es -> "vectorf([" ^ String.concat "; " (List.map string_of_expr es) ^ "])"
  | Matrixi es -> "Matrixi([" ^ String.concat "; " (List.map string_of_expr es) ^ "])"
  | Matrixf es -> "Matrixf([" ^ String.concat "; " (List.map string_of_expr es) ^ "])"
  | DualOp (op, e1, e2) -> 
      "DualOp(" ^ op ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | SinOp (op, e) -> "SinOp(" ^ op ^ ", " ^ string_of_expr e ^ ")"
  | String s -> "String(\"" ^ s ^ "\")"

let rec string_of_stmt = function
  | Declaration(t, s, e) -> 
      "Declaration(" ^ string_of_type t ^ ", Var(" ^ s ^ "), " ^ string_of_expr e ^ ")"
  | Assign (s, e) -> "Assign(Var(" ^ s ^ "), " ^ string_of_expr e ^ ")"
  | Print e -> "Print(" ^ string_of_expr e ^ ")"
  | If (e, s1, s2) -> 
      "If(" ^ string_of_expr e ^ ", " ^ string_of_stmt s1 ^ ", " ^ string_of_stmt s2 ^ ")"
  | While (e, s) -> 
      "While(" ^ string_of_expr e ^ ", " ^ string_of_stmt s ^ ")"
  | Loop (init, cond, incr, body) -> 
      "Loop(" ^ string_of_stmt init ^ ", " ^ 
      string_of_expr cond ^ ", " ^
      string_of_stmt incr ^ ", " ^
      string_of_stmt body ^ ")"
  | Do s -> "Do(" ^ string_of_stmt s ^ ")"
  | Block stmts -> "Block([" ^ String.concat "; " (List.map string_of_stmt stmts) ^ "])"
  | Expr e -> string_of_expr e
  | Input opt -> "Input(" ^ 
      (match opt with 
       | None -> "None"
       | Some e -> string_of_expr e) 
      ^ ")"
  | InputAssign(typ, var) ->
      "InputAssign(" ^ string_of_type typ ^ ", " ^ var ^ ")"


let string_of_ast ast = string_of_stmt ast

