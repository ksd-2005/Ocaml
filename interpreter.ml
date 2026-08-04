open Ast
open Printf
open Str

type values = 
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VVectorI of values list
   | VVectorF of values list            
  | VMatrixI of values list
  |  VMatrixF of values list      
  | VString of string
  | VError of string

let find_substring str sub start =
  let sub_len = String.length sub in
  let str_len = String.length str in
  let rec check i j =
    if j = sub_len then i
    else if i + j >= str_len then raise Not_found
    else if str.[i + j] = sub.[j] then check i (j + 1)
    else raise Not_found
  in
  let rec search i =
    if i > str_len - sub_len then raise Not_found
    else
      try check i 0
      with Not_found -> search (i + 1)
  in search start


let split_on_delim str ~by =
  let by_len = String.length by in
  let str_len = String.length str in
  let rec aux start acc =
    if start >= str_len then
      List.rev acc
    else
      try
        let idx = find_substring str by start in
        let part = String.sub str start (idx - start) in
        aux (idx + by_len) (part :: acc)
      with Not_found ->
        let last = String.sub str start (str_len - start) in
        List.rev (last :: acc)
  in aux 0 []


let rec print_value = function
  | VInt i -> print_int i
  | VFloat f -> printf "%.2f" f  
  | VBool b -> print_string (if b then "true" else "false")
  | VString s -> print_string s
  | VVectorI vs ->
      print_string "[";
      List.iteri (fun i v -> 
        print_value v;
        if i < List.length vs - 1 then print_string ", ") vs;
      print_string "]"
  | VVectorF vs ->
      print_string "[";
      List.iteri (fun i v -> 
        print_value v;
        if i < List.length vs - 1 then print_string ", ") vs;
      print_string "]"
  | VMatrixI rows ->
      print_endline "[";
      List.iteri (fun i row ->
        match row with
        | VVectorI vs -> 
            print_string "  ";  (* indent *)
            print_value (VVectorI vs);
            if i < List.length rows - 1 then print_endline "," else print_newline ()
        | _ -> raise (Failure "Invalid matrix row")
      ) rows;
      print_string "]"
      | VMatrixF rows ->
      print_endline "[";
      List.iteri (fun i row ->
        match row with
        | VVectorF vs -> 
            print_string "  ";  (* indent *)
            print_value (VVectorF vs);
            if i < List.length rows - 1 then print_endline "," else print_newline ()
        | _ -> raise (Failure "Invalid matrix row")
      ) rows;
      print_string "]"
  | VError e -> print_string ("Error: " ^ e)

let read_file filename =
  try
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (acc ^ line ^ "\n")
      with End_of_file -> 
        close_in ic;
        acc
    in
    read_lines ""
  with
  | Sys_error _ -> raise (Failure ("Could not open file: " ^ filename))
  | End_of_file -> raise (Failure ("File is empty: " ^ filename))



let dot_product_int row col =
  let rec dp l1 l2 acc =
    match l1, l2 with
    | [], [] -> acc
    | (VInt x1)::xs1, (VInt x2)::xs2 -> dp xs1 xs2 (acc + (x1 * x2))
    | _, _ -> raise (Failure "Invalid vector elements for dot product")
  in dp row col 0

let dot_product_float row col =
  let rec dp l1 l2 acc =
    match l1, l2 with
    | [], [] -> acc
    | (VFloat x1)::xs1, (VFloat x2)::xs2 -> dp xs1 xs2 (acc +. (x1 *. x2))
    | _, _ -> raise (Failure "Invalid vector elements for dot product")
  in dp row col 0.0

let submatrix rows i j =
  let remove_nth lst n = List.filteri (fun idx _ -> idx <> n) lst in
  let rows_without_i = remove_nth rows i in
  List.map (fun row -> match row with
    | VVectorI r -> VVectorI (remove_nth r j)
    | VVectorF r -> VVectorF (remove_nth r j)
    | _ -> raise (Failure "Invalid matrix row")) rows_without_i

let transpose_int rows =
  match rows with
  | [] -> []
  | VVectorI first :: _ ->
      let cols = List.length first in
      let get_col i = List.map (fun row ->
        match row with
        | VVectorI r -> List.nth r i
        | _ -> raise (Failure "Invalid matrix row")) rows
      in
      List.init cols (fun i -> VVectorI (get_col i))
  | _ -> raise (Failure "Invalid matrix format")

let transpose_float rows =
  match rows with
  | [] -> []
  | VVectorF first :: _ ->
      let cols = List.length first in
      let get_col i = List.map (fun row ->
        match row with
        | VVectorF r -> List.nth r i
        | _ -> raise (Failure "Invalid matrix row")) rows
      in
      List.init cols (fun i -> VVectorF (get_col i))
  | _ -> raise (Failure "Invalid matrix format")

let rec det_int m =
    match m with
    | [] -> VInt 1  (* 0x0 matrix *)
    | [VVectorI [VInt x]] -> VInt x  (* 1x1 matrix *)
    | [VVectorI [VInt a; VInt b]; VVectorI [VInt c; VInt d]] ->  (* 2x2 matrix *)
        VInt (a * d - b * c)
    | _ ->
        if List.length m <> List.length (match List.hd m with VVectorI r -> r | _ -> []) then
          raise (Failure "Matrix must be square for determinant")
        else
          (* Expand along the first row *)
          let fold_fn acc (col, elt) =
            let sign = if col mod 2 = 0 then 1 else -1 in
            match elt, det_int (submatrix m 0 col) with
            | VInt x, VInt d -> acc + sign * x * d
            | _ -> raise (Failure "Type mismatch in determinant calculation")
          in
          List.fold_left fold_fn 0 (List.mapi (fun i e -> (i, e)) 
            (match List.hd m with VVectorI r -> r | _ -> []))
          |> fun x -> VInt x

  let rec det_float m =
    match m with
    | [] -> VFloat 1.0  (* 0x0 matrix *)
    | [VVectorF [VFloat x]] -> VFloat x  (* 1x1 matrix *)
    | [VVectorF [VFloat a; VFloat b]; VVectorF [VFloat c; VFloat d]] ->  (* 2x2 matrix *)
        VFloat (a *. d -. b *. c)
    | _ ->
        if List.length m <> List.length (match List.hd m with VVectorF r -> r | _ -> []) then
          raise (Failure "Matrix must be square for determinant")
        else
          let fold_fn acc (col, elt) =
            let sign = if col mod 2 = 0 then 1.0 else -1.0 in
            match elt, det_float (submatrix m 0 col) with
            | VFloat x, VFloat d -> acc +. sign *. x *. d
            | _ -> raise (Failure "Type mismatch in determinant calculation")
          in
          List.fold_left fold_fn 0.0 (List.mapi (fun i e -> (i, e)) 
            (match List.hd m with VVectorF r -> r | _ -> []))
          |> fun x -> VFloat x


let rec values_equal v1 v2 =
  match v1, v2 with
  | VInt i1, VInt i2 -> i1 = i2
  | VFloat f1, VFloat f2 -> f1 = f2
  | VBool b1, VBool b2 -> b1 = b2
  | VString s1, VString s2 -> s1 = s2
  | VVectorI vs1, VVectorI vs2 -> 
      List.length vs1 = List.length vs2 && List.for_all2 values_equal vs1 vs2
  | VVectorF vs1, VVectorF vs2 -> 
      List.length vs1 = List.length vs2 && List.for_all2 values_equal vs1 vs2
  | VMatrixI m1, VMatrixI m2 -> 
      List.length m1 = List.length m2 && List.for_all2 values_equal m1 m2
  | VMatrixF m1, VMatrixF m2 -> 
      List.length m1 = List.length m2 && List.for_all2 values_equal m1 m2
  | VError _, VError _ -> true  
  | _, _ -> false

type env = (string * values) list



let lookup env var =
  try List.assoc var env
  with Not_found -> raise (Failure ("Undefined variable: " ^ var))

let add_to_env env var value = (var, value) :: env
let update_env env var value = 
    let rec update = function
    | [] -> raise (Failure ("Variable not found: " ^ var))
    | (v, _) :: rest when v = var -> (v, value) :: rest
    | pair :: rest -> pair :: update rest
  in
  update env

let rec eval_expr env = function
  | Ast.Int i -> VInt i
  | Ast.Float f -> VFloat f
  | Ast.Bool b -> VBool b
  | Ast.String s -> VString s
  | Ast.Var x -> lookup env x
  | Ast.Vectori es ->
      let elements = List.map (fun e ->
        match eval_expr env e with
        | VInt i -> VInt(i)
        | _ -> raise (Failure "Vector elements must be numeric")
      ) es in
      VVectorI elements
  | Ast.Vectorf es ->
      let elements = List.map (fun e ->
        match eval_expr env e with
        | VFloat f -> VFloat(f)
        | _ -> raise (Failure "Vector elements must be numeric")
      ) es in
      VVectorF elements
  | Ast.Matrixi rows ->
      let int_rows = List.map (fun row ->
        match eval_expr env row with
        | VVectorI fs -> fs
        | _ -> raise (Failure "Matrix rows must be vectors")
      ) rows in
      let len = List.length (List.hd int_rows) in
      if List.for_all (fun row -> List.length row = len) int_rows
      then VMatrixI (List.map (fun row -> VVectorI row) int_rows)  
      else raise (Failure "Matrix rows must have equal length")
  | Ast.Matrixf rows ->
      let float_rows = List.map (fun row ->
        match eval_expr env row with
        | VVectorF fs -> fs  
        | _ -> raise (Failure "Matrix rows must be vectors")
      ) rows in
      let len = List.length (List.hd float_rows) in
      if List.for_all (fun row -> List.length row = len) float_rows
      then VMatrixF (List.map (fun row -> VVectorF row) float_rows)  
      else raise (Failure "Matrix rows must have equal length")
  | Ast.DualOp (op, e1, e2) -> eval_dualop env op e1 e2
  | Ast.SinOp (op, e) -> eval_sinop env op e

  and eval_dualop env op e1 e2 =
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  match op, v1, v2 with
  | "+", VInt i1, VInt i2 -> VInt (i1 + i2)
  | "+", VFloat f1, VFloat f2 -> VFloat (f1 +. f2)
  | "+", VVectorI vs1, VVectorI vs2 ->
      if List.length vs1 <> List.length vs2 then
      raise (Failure "Vector dimensions must match for addition")
    else
      let add_elements v1 v2 =
        match v1, v2 with
        | VInt i1, VInt i2 -> VInt (i1 + i2)
        | _ , _ -> raise (Failure "Type mismatch")
      in
      VVectorI (List.map2 add_elements vs1 vs2)
  | "+", VVectorF vs1, VVectorF vs2 ->
      if List.length vs1 <> List.length vs2 then
      raise (Failure "Vector dimensions must match for addition")
      else 
      let add_elements v1 v2 =
        match v1, v2 with
        | VFloat i1, VFloat i2 -> VFloat (i1 +. i2)
        | _ , _ -> raise (Failure "Type mismatch")
        in
      VVectorF (List.map2 add_elements vs1 vs2)
  | "+", VMatrixI rows1, VMatrixI rows2 ->
      
      (let r1 = List.length rows1 in
    let r2 = List.length rows2 in
    if r1 <> r2 then 
      raise (Failure "Matrix dimensions must match for addition")
    else
      match (rows1, rows2) with
      | (VVectorI first1 :: _, VVectorI first2 :: _) ->
          let c1 = List.length first1 in
          let c2 = List.length first2 in
          if c1 <> c2 then
            raise (Failure "Matrix dimensions must match for addition")
          else
        let add_rows row1 row2 =
              match row1, row2 with
              | VVectorI vs1, VVectorI vs2 ->
                  VVectorI (List.map2 (fun v1 v2 ->
                    match v1, v2 with
                    | VInt i1, VInt i2 -> VInt (i1 + i2)
                    | _, _ -> raise (Failure "Invalid matrix elements")
                  ) vs1 vs2)
              | _, _ -> raise (Failure "Invalid matrix format")
            in
            VMatrixI (List.map2 add_rows rows1 rows2)
      | _, _ -> raise (Failure "Invalid matrix format"))
  | "+", VMatrixF rows1, VMatrixF rows2 ->
      (let r1 = List.length rows1 in
    let r2 = List.length rows2 in
    if r1 <> r2 then 
      raise (Failure "Matrix dimensions must match for addition")
    else
      match (rows1, rows2) with
      | (VVectorF first1 :: _, VVectorF first2 :: _) ->
          let c1 = List.length first1 in
          let c2 = List.length first2 in
          if c1 <> c2 then
            raise (Failure "Matrix dimensions must match for addition")
          else
        let add_rows row1 row2 =
              match row1, row2 with
              | VVectorF vs1, VVectorF vs2 ->
                  VVectorF (List.map2 (fun v1 v2 ->
                    match v1, v2 with
                    | VFloat i1, VFloat i2 -> VFloat (i1 +. i2)
                    | _, _ -> raise (Failure "Invalid matrix elements")
                  ) vs1 vs2)
              | _, _ -> raise (Failure "Invalid matrix format")
            in
            VMatrixF (List.map2 add_rows rows1 rows2)
      | _, _ -> raise (Failure "Invalid matrix format"))
| "-", VInt i1, VInt i2 -> VInt (i1 - i2)
  | "-", VFloat f1, VFloat f2 -> VFloat (f1 -. f2)
  | "-", VVectorI vs1, VVectorI vs2 ->
  if List.length vs1 <> List.length vs2 then
      raise (Failure "Vector dimensions must match for subtraction")
    else
      let sub_elements v1 v2 =
        match v1, v2 with
        | VInt i1, VInt i2 -> VInt (i1 - i2)
        | _ , _ -> raise (Failure "Type mismatch")
        in
      VVectorI (List.map2 sub_elements vs1 vs2)
  | "-", VVectorF vs1, VVectorF vs2 ->
  if List.length vs1 <> List.length vs2 then
      raise (Failure "Vector dimensions must match for subtraction")
    else
      let sub_elements v1 v2 =
        match v1, v2 with
        | VFloat i1, VFloat i2 -> VFloat (i1 -. i2)
        | _ , _ -> raise (Failure "Type mismatch")
        in
      VVectorF (List.map2 sub_elements vs1 vs2)
  | "-", VMatrixI rows1, VMatrixI rows2 ->
      (let r1 = List.length rows1 in
    let r2 = List.length rows2 in
    if r1 <> r2 then 
      raise (Failure "Matrix dimensions must match for subtraction")
    else
      match (rows1, rows2) with
      | (VVectorI first1 :: _, VVectorI first2 :: _) ->
          let c1 = List.length first1 in
          let c2 = List.length first2 in
          if c1 <> c2 then
            raise (Failure "Matrix dimensions must match for subtraction")
          else
        let add_rows row1 row2 =
              match row1, row2 with
              | VVectorI vs1, VVectorI vs2 ->
                  VVectorI (List.map2 (fun v1 v2 ->
                    match v1, v2 with
                    | VInt i1, VInt i2 -> VInt (i1 - i2)
                    | _, _ -> raise (Failure "Invalid matrix elements")
                  ) vs1 vs2)
              | _, _ -> raise (Failure "Invalid matrix format")
            in
            VMatrixI (List.map2 add_rows rows1 rows2)
      | _, _ -> raise (Failure "Invalid matrix format"))
  | "-", VMatrixF rows1, VMatrixF rows2 ->
      (let r1 = List.length rows1 in
    let r2 = List.length rows2 in
    if r1 <> r2 then 
      raise (Failure "Matrix dimensions must match for subtraction")
    else
      match (rows1, rows2) with
      | (VVectorF first1 :: _, VVectorF first2 :: _) ->
          let c1 = List.length first1 in
          let c2 = List.length first2 in
          if c1 <> c2 then
            raise (Failure "Matrix dimensions must match for subtraction")
          else
        let add_rows row1 row2 =
              match row1, row2 with
              | VVectorF vs1, VVectorF vs2 ->
                  VVectorF (List.map2 (fun v1 v2 ->
                    match v1, v2 with
                    | VFloat i1, VFloat i2 -> VFloat (i1 -. i2)
                    | _, _ -> raise (Failure "Invalid matrix elements")
                  ) vs1 vs2)
              | _, _ -> raise (Failure "Invalid matrix format")
            in
            VMatrixF (List.map2 add_rows rows1 rows2)
      | _, _ -> raise (Failure "Invalid matrix format"))
  | "/" , VInt i1 , VInt i2 when i2=0 -> VError("Divsion by zero")
  |  "/" , VInt i1 , VInt i2-> VInt (i1/i2)
  | "/" , VFloat i1 , VFloat i2 when i2=0.0 -> VError("Divsion by zero")
  |  "/" , VFloat i1 , VFloat i2-> VFloat (i1/.i2)
  | "&&", VBool b1, VBool b2 -> VBool (b1 && b2)
  | "||", VBool b1, VBool b2 -> VBool (b1 || b2)
  | "<", VInt i1, VInt i2 -> VBool (i1 < i2)
  | "<", VFloat f1, VFloat f2 -> VBool (f1 < f2)
  | ">", VInt i1, VInt i2 -> VBool (i1 > i2)
  | ">", VFloat f1, VFloat f2 -> VBool (f1 > f2)
  | "<=", VInt i1, VInt i2 -> VBool (i1 <= i2)
  | "<=", VFloat f1, VFloat f2 -> VBool (f1 <= f2)
  | ">=", VInt i1, VInt i2 -> VBool (i1 >= i2) 
  | ">=", VFloat f1, VFloat f2 -> VBool (f1 >= f2)
  | "=", VInt i1, VInt i2 -> VBool (i1 = i2)
  | "=", VFloat f1, VFloat f2 -> VBool (f1 = f2)
  | "=", VBool b1, VBool b2 -> VBool (b1 = b2)
  | "!=", VInt i1, VInt i2 -> VBool (i1 <> i2)
  | "!=", VFloat f1, VFloat f2 -> VBool (f1 <> f2)
  | "!=", VBool b1, VBool b2 -> VBool (b1 <> b2)
  | "=", VVectorI vs1 , VVectorI vs2 -> VBool (values_equal v1 v2)
  | "=", VVectorF vs1 , VVectorF vs2 -> VBool (values_equal v1 v2)
  | "=", VMatrixI m1, VMatrixI m2 -> VBool(values_equal v1 v2)
  | "=", VMatrixF m1, VMatrixF m2 -> VBool(values_equal v1 v2)
  | "!=", VVectorI vs1 , VVectorI vs2 -> VBool (not(values_equal v1 v2))
  | "!=", VVectorF vs1 , VVectorF vs2 -> VBool (not(values_equal v1 v2))
  | "!=", VMatrixI m1, VMatrixI m2 -> VBool(not(values_equal v1 v2))
  | "!=", VMatrixF m1, VMatrixF m2 -> VBool(not(values_equal v1 v2))
  | "%" , VInt i1 , VInt i2 -> VInt (i1 mod i2)
  | "*", VInt i1, VInt i2 -> VInt (i1 * i2)
  | "*", VFloat f1, VFloat f2 -> VFloat (f1 *. f2)
  | "*", VInt s, VVectorI vs -> 
      VVectorI (List.map (fun v -> 
        match v with
        | VInt x -> VInt (s * x)
        | _ -> raise (Failure "Invalid vector element type")) vs)
  | "*", VVectorI vs, VInt s-> 
      VVectorI (List.map (fun v -> 
        match v with
        | VInt x -> VInt (s * x)
        | _ -> raise (Failure "Invalid vector element type")) vs)
  | "*", VFloat s, VVectorF vs -> 
      VVectorF (List.map (fun v ->
        match v with
        | VFloat x -> VFloat (s *. x)
        | _ -> raise (Failure "Invalid vector element type")) vs)
  | "*", VVectorF vs, VFloat s -> 
      VVectorF (List.map (fun v ->
        match v with
        | VFloat x -> VFloat (s *. x)
        | _ -> raise (Failure "Invalid vector element type")) vs)
  | "*", VVectorI vs1, VVectorI vs2 ->
        let rec dot_product l1 l2 acc =
          match l1, l2 with
          | [], [] -> acc
          | (VInt x1)::xs1, (VInt x2)::xs2 -> 
              dot_product xs1 xs2 (acc + (x1 * x2))
          | _, _ -> raise (Failure "Invalid vector elements")
        in VInt (dot_product vs1 vs2 0)
  | "*", VVectorF vs1, VVectorF vs2 ->
        let rec dot_product l1 l2 acc =
          match l1, l2 with
          | [], [] -> acc
          | (VFloat x1)::xs1, (VFloat x2)::xs2 -> 
              dot_product xs1 xs2 (acc +. (x1 *. x2))
          | _, _ -> raise (Failure "Invalid vector elements")
        in VFloat (dot_product vs1 vs2 0.0)
  | "*", VMatrixI rows, VVectorI vec ->
    (match rows with
     | [] -> VVectorI []
     | VVectorI first_row :: _ ->
         let num_cols = List.length first_row in
         let vec_len = List.length vec in
         if num_cols <> vec_len then
           raise (Failure "Matrix-vector dimensions incompatible for multiplication")
         else
           let multiply_row row =
            let result = dot_product_int row vec in 
            VInt result
           in
           VVectorI (List.map (fun row_vec ->
             match row_vec with
             | VVectorI row -> multiply_row row
             | _ -> raise (Failure "Invalid matrix row")
           ) rows)
     | _ -> raise (Failure "Invalid matrix format"))
  
  | "*", VMatrixF rows, VVectorF vec ->
    (match rows with
     | [] -> VVectorF []
     | VVectorF first_row :: _ ->
         let num_cols = List.length first_row in
         let vec_len = List.length vec in
         if num_cols <> vec_len then
           raise (Failure "Matrix-vector dimensions incompatible for multiplication")
         else
           let multiply_row row =
            let result = dot_product_float row vec in 
            VFloat result
           in
           VVectorF (List.map (fun row_vec ->
             match row_vec with
             | VVectorF row -> multiply_row row
             | _ -> raise (Failure "Invalid matrix row")
           ) rows)
     | _ -> raise (Failure "Invalid matrix format"))
  | "*", VMatrixI m1, VMatrixI m2 ->
    let m2_t = transpose_int m2 in  
    (match m1 with
     | [] -> VMatrixI []
     | VVectorI first_row :: _ ->
         let num_cols_m1 = List.length first_row in
         let num_rows_m2 = List.length m2 in
         if num_cols_m1 <> num_rows_m2 then
           raise (Failure "Matrix dimensions incompatible for multiplication")
         else
          
           let multiply_row row cols =
             List.map (fun col_vec ->
               match col_vec with
               | VVectorI col -> VInt (dot_product_int row col)
               | _ -> raise (Failure "Invalid matrix column")
             ) cols
           in
           VMatrixI (List.map (fun row_vec ->
             match row_vec with
             | VVectorI row -> VVectorI (multiply_row row m2_t)
             | _ -> raise (Failure "Invalid matrix row")
           ) m1)
     | _ -> raise (Failure "Invalid matrix format"))
  | "*", VMatrixF m1, VMatrixF m2 ->
    let m2_t = transpose_float m2 in  
    (match m1 with
     | [] -> VMatrixF []
     | VVectorF first_row :: _ ->
         let num_cols_m1 = List.length first_row in
         let num_rows_m2 = List.length m2 in
         if num_cols_m1 <> num_rows_m2 then
           raise (Failure "Matrix dimensions incompatible for multiplication")
         else
        
           let multiply_row row cols =
             List.map (fun col_vec ->
               match col_vec with
               | VVectorF col -> VFloat (dot_product_float row col)
               | _ -> raise (Failure "Invalid matrix column")
             ) cols
           in
           VMatrixF (List.map (fun row_vec ->
             match row_vec with
             | VVectorF row -> VVectorF (multiply_row row m2_t)
             | _ -> raise (Failure "Invalid matrix row")
           ) m1)
    | _ -> raise (Failure "Invalid matrix format"))
  | "angle", VVectorI vs1, VVectorI vs2 ->
      let dot = dot_product_int vs1 vs2 in
      let mag1 = sqrt (float_of_int (dot_product_int vs1 vs1)) in
      let mag2 = sqrt (float_of_int (dot_product_int vs2 vs2)) in
      if mag1 = 0.0 || mag2 = 0.0 then
        raise (Failure "Cannot calculate angle with zero vector")
      else
        let cos_theta = (float_of_int dot) /. (mag1 *. mag2) in
        VFloat ((acos cos_theta) *. 180.0 /. Float.pi)  (* Convert to degrees *)

  (* Angle for float vectors *)
  | "angle", VVectorF vs1, VVectorF vs2 ->
      let dot = dot_product_float vs1 vs2 in
      let mag1 = sqrt (dot_product_float vs1 vs1) in
      let mag2 = sqrt (dot_product_float vs2 vs2) in
      if mag1 = 0.0 || mag2 = 0.0 then
        raise (Failure "Cannot calculate angle with zero vector")
      else
        let cos_theta = dot /. (mag1 *. mag2) in
        VFloat ((acos cos_theta) *. 180.0 /. Float.pi)  (* Convert to degrees *)
  | _ , _, _ -> VError ("Invalid")

  and eval_sinop env op e =
  let v = eval_expr env e in
  match op, v with
  | "not", VBool b -> VBool (not b)
  | "abs", VInt i -> VInt (abs i)
  | "abs", VFloat f -> VFloat (abs_float f)
  | "dim", VVectorI vs -> VInt (List.length vs)
  | "dim", VVectorF vs -> VInt (List.length vs)
  | "trace", VMatrixI rows ->
   let n = List.length rows in
      if n = 0 then VInt 0
      else 
        (match List.hd rows with
        | VVectorI first_row ->
            let m = List.length first_row in
            if n <> m then
              raise (Failure "Trace requires square matrix")
            else
              let rec sum_diagonal i acc =
                if i >= n then acc
                else
                  match List.nth rows i with
                  | VVectorI row -> 
                      (match List.nth row i with
                       | VInt x -> sum_diagonal (i + 1) (acc + x)
                       | _ -> raise (Failure "Invalid matrix element"))
                  | _ -> raise (Failure "Invalid matrix row")
              in VInt (sum_diagonal 0 0)
        | _ -> raise (Failure "Invalid matrix format"))
   | "trace", VMatrixF rows ->
      let n = List.length rows in
      if n = 0 then VFloat 0.0
      else 
        (match List.hd rows with
        | VVectorF first_row ->
            let m = List.length first_row in
            if n <> m then
              raise (Failure "Trace requires square matrix")
            else
              let rec sum_diagonal i acc =
                if i >= n then acc
                else
                  match List.nth rows i with
                  | VVectorF row -> 
                      (match List.nth row i with
                       | VFloat x -> sum_diagonal (i + 1) (acc +. x)
                       | _ -> raise (Failure "Invalid matrix element"))
                  | _ -> raise (Failure "Invalid matrix row")
              in VFloat (sum_diagonal 0 0.0)
        | _ -> raise (Failure "Invalid matrix format"))
  | "mag", VVectorI vs ->
      let rec magnitude vs acc =
        match vs with
        | [] -> sqrt (float_of_int acc)
        | VInt x :: rest -> magnitude rest (acc + x * x)
        | _ -> raise (Failure "Invalid vector element")
      in VFloat (magnitude vs 0)
  | "mag", VVectorF vs ->
      let rec magnitude vs acc =
        match vs with
        | [] -> sqrt acc
        | VFloat x :: rest -> magnitude rest (acc +. x *. x)
        | _ -> raise (Failure "Invalid vector element")
      in VFloat (magnitude vs 0.0)
  | "mag", VMatrixI rows ->
    let rec sum_squares_matrix rows acc =
      match rows with 
      | [] -> sqrt (float_of_int acc)
      | VVectorI row :: rest ->
          let rec sum_row_squares row acc =
            match row with
            | [] -> acc
            | VInt x :: rest_row -> sum_row_squares rest_row (acc + x * x)
            | _ -> raise (Failure "Invalid matrix element")
          in
          sum_squares_matrix rest (acc + sum_row_squares row 0)
      | _ -> raise (Failure "Invalid matrix row")
    in
    VFloat (sum_squares_matrix rows 0)

  | "mag", VMatrixF rows ->
    let rec sum_squares_matrix rows acc =
      match rows with
      | [] -> sqrt acc
      | VVectorF row :: rest ->
          let rec sum_row_squares row acc =
            match row with
            | [] -> acc
            | VFloat x :: rest_row -> sum_row_squares rest_row (acc +. x *. x)
            | _ -> raise (Failure "Invalid matrix element")
          in
          sum_squares_matrix rest (acc +. sum_row_squares row 0.0)
      | _ -> raise (Failure "Invalid matrix row")
    in
    VFloat (sum_squares_matrix rows 0.0)
  
  | "det", VMatrixI rows -> det_int rows
  

  | "det", VMatrixF rows -> det_float rows
    
  | "transpose", VMatrixI rows -> VMatrixI (transpose_int rows)
  | "transpose", VMatrixF rows -> VMatrixF (transpose_float rows)
  
  | "inv", VMatrixI rows ->
    let n = List.length rows in
      if n = 0 then
        VMatrixI [VVectorI [VInt 0]]
      else if n = 1 then
        match rows with
        | [VVectorI [VInt x]] ->
            if x = 0 then raise (Failure "Matrix is not invertible")
            else VMatrixI [VVectorI [VInt (1 / x)]]
        | _ -> raise (Failure "Invalid matrix format")
      else
        let det_val = match det_int rows with
                    | VInt d -> d
                    | _ -> raise (Failure "Invalid determinant")
        in
        if det_val = 0 then raise (Failure "Matrix is not invertible")
        else
          let cofactor_mat =
            List.mapi (fun i _ ->
              VVectorI (List.mapi (fun j _ ->
                let sign = if (i + j) mod 2 = 0 then 1 else -1 in
                let sub_det = match det_int (submatrix rows i j) with
                              | VInt d -> d
                              | _ -> raise (Failure "Invalid subdeterminant")
                in VInt (sign * sub_det)
              ) (match List.hd rows with VVectorI r -> r | _ -> []))
            ) rows
          in
          let adjugate = transpose_int cofactor_mat in
          let inverse_rows = List.map (fun row ->
            match row with
            | VVectorI r ->
                VVectorI (List.map (fun v ->
                  match v with
                  | VInt x -> VInt (x / det_val)
                  | _ -> raise (Failure "Invalid element")
                ) r)
            | _ -> raise (Failure "Invalid row")
          ) adjugate in
          VMatrixI inverse_rows


  | "inv", VMatrixF rows ->
    let n = List.length rows in
      if n = 0 then raise (Failure "Cannot invert empty matrix")
      else if n = 1 then
        match rows with
        | [VVectorF [VFloat x]] ->
            if x = 0.0 then raise (Failure "Matrix is not invertible")
            else VMatrixF [VVectorF [VFloat (1.0 /. x)]]
        | _ -> raise (Failure "Invalid matrix format")
      else
        let det_val = match det_float rows with
                    | VFloat d -> d
                    | _ -> raise (Failure "Invalid determinant")
        in
        if det_val = 0.0 then raise (Failure "Matrix is not invertible")
        else
          let cofactor_mat =
            List.mapi (fun i _ ->
              VVectorF (List.mapi (fun j _ ->
                let sign = if (i + j) mod 2 = 0 then 1.0 else -1.0 in
                let sub_det = match det_float (submatrix rows i j) with
                              | VFloat d -> d
                              | _ -> raise (Failure "Invalid subdeterminant")
                in VFloat (sign *. sub_det)
              ) (match List.hd rows with VVectorF r -> r | _ -> []))
            ) rows
          in
          let adjugate = transpose_float cofactor_mat in
          let inv_det = 1.0 /. det_val in
          let inverse_rows = List.map (fun row ->
            match row with
            | VVectorF r -> 
                VVectorF (List.map (fun v ->
                  match v with
                  | VFloat x -> VFloat (x *. inv_det)
                  | _ -> raise (Failure "Invalid element")
                ) r)
            | _ -> raise (Failure "Invalid row")
          ) adjugate in
          VMatrixF inverse_rows
  | "sqrt", VInt i -> 
    if i >= 0 then 
      VFloat (sqrt (float_of_int i))
    else 
      raise (Failure "Square root of negative number")
  | "sqrt", VFloat f -> 
    if f >= 0.0 then
      VFloat (sqrt f)
    else
      raise (Failure "Square root of negative number")
  | _ ,_ -> VError("Invalid")
  
  
let rec eval_stmt env = function
  | Declaration (_, var, expr) ->
      let value = eval_expr env expr in
      add_to_env env var value
  | Assign (var, expr) ->
      let value = eval_expr env expr in
      update_env env var value
  | Print expr ->
      let value = eval_expr env expr in
      print_value value;
      print_newline();
      env
  | If (cond, then_stmt, else_stmt) ->
      (match eval_expr env cond with
       | VBool true -> eval_stmt env then_stmt
       | VBool false -> eval_stmt env else_stmt
       | _ -> raise (Failure "If condition must be boolean"))
  | While (cond, body) ->
      let rec loop env =
        match eval_expr env cond with
        | VBool true ->
            let env' = eval_stmt env body in
            loop env'
        | VBool false -> env
        | _ -> raise (Failure "While condition must be boolean")
      in loop env
  | Loop (init, cond, incr, body) ->
      let env' = eval_stmt env init in
      let rec loop env =
        match eval_expr env cond with
        | VBool true ->
            let env'' = eval_stmt env body in
            let env''' = eval_stmt env'' incr in
            loop env'''
        | VBool false -> env
        | _ -> raise (Failure "Loop condition must be boolean")
      in loop env'
  | Block stmts ->
      List.fold_left eval_stmt env stmts
  | Expr expr ->
      let _ = eval_expr env expr in
      env
  | InputAssign(typ, var) ->      
      (* Read input *)
      let input = read_line () in
      
      (* Convert input based on type *)
      let value = match typ with
        | TInt -> 
            (try VInt(int_of_string input)
             with _ -> raise (Failure "Invalid integer input"))
        | TFloat -> 
            (try VFloat(float_of_string input)
             with _ -> raise (Failure "Invalid float input"))
        | TBool ->
            (match String.lowercase_ascii input with
             | "true" -> VBool true
             | "false" -> VBool false
             | _ -> raise (Failure "Invalid boolean input (use 'true' or 'false')"))
        | TVectorI ->
             (try
             let parts = String.split_on_char ' ' input in
             match parts with
             | [dim_str; vec_str] ->
                 let dim = int_of_string dim_str in
                 (* Remove brackets and split vector elements *)
                 let content = String.sub vec_str 1 ((String.length vec_str) - 2) in
                 let nums = String.split_on_char ',' content
                          |> List.map String.trim
                          |> List.map int_of_string
                          |> List.map (fun x -> VInt x) in
                 if List.length nums = dim then
                   VVectorI nums
                 else
                   raise (Failure "Vector dimension mismatch")
             | _ -> raise (Failure "Invalid vector format. Use: dimension [x,y,z]")
           with _ -> raise (Failure "Invalid vector input"))
        | TVectorF ->
            (try
             let parts = String.split_on_char ' ' input in
             match parts with
             | [dim_str; vec_str] ->
                 let dim = int_of_string dim_str in
                 let content = String.sub vec_str 1 ((String.length vec_str) - 2) in
                 let nums = String.split_on_char ',' content
                          |> List.map String.trim
                          |> List.map float_of_string
                          |> List.map (fun x -> VFloat x) in
                 if List.length nums = dim then
                   VVectorF nums
                 else
                   raise (Failure "Vector dimension mismatch")
             | _ -> raise (Failure "Invalid vector format. Use: dimension [x.x,y.y,z.z]")
           with _ -> raise (Failure "Invalid vector input"))
        | TMatrixI ->
          (try
    let parts = String.split_on_char ' ' input in
    match parts with
    | [dim_str; mat_str] ->
        let dim_parts = String.split_on_char ',' dim_str in
        (match dim_parts with
         | [rows_str; cols_str] ->
             let rows = int_of_string rows_str in
             let cols = int_of_string cols_str in
             if String.length mat_str < 4 || 
                not (String.sub mat_str 0 2 = "[[" && 
                     String.sub mat_str (String.length mat_str - 2) 2 = "]]") then
               raise (Failure "Invalid matrix format")
             else
               let inner = String.sub mat_str 2 (String.length mat_str - 4) in
               let row_strings = split_on_delim inner ~by:"],[" in
               let matrix_rows = List.map (fun row_str ->
                 let nums_str = String.split_on_char ',' row_str in
                 let nums = List.map (fun s -> VInt (int_of_string (String.trim s))) nums_str in
                 if List.length nums = cols then VVectorI nums
                 else raise (Failure "Matrix column count mismatch")
               ) row_strings in
               if List.length matrix_rows = rows then VMatrixI matrix_rows
               else raise (Failure "Matrix row count mismatch")
         | _ -> raise (Failure "Invalid dimension format"))
    | _ -> raise (Failure "Invalid matrix format")
  with _ -> raise (Failure "Invalid matrix input"))
        | TMatrixF ->
            (try
    let parts = String.split_on_char ' ' input in
    match parts with
    | [dim_str; mat_str] ->
        let dim_parts = String.split_on_char ',' dim_str in
        (match dim_parts with
         | [rows_str; cols_str] ->
             let rows = int_of_string rows_str in
             let cols = int_of_string cols_str in
             if String.length mat_str < 4 || 
                not (String.sub mat_str 0 2 = "[[" && 
                     String.sub mat_str (String.length mat_str - 2) 2 = "]]") then
               raise (Failure "Invalid matrix format. Use: rows,cols [[a.b,c.d],[e.f,g.h]]")
             else
               let inner = String.sub mat_str 2 (String.length mat_str - 4) in
               let row_strings = split_on_delim inner ~by:"],[" in
               let matrix_rows = List.map (fun row_str ->
                 let nums_str = String.split_on_char ',' row_str in
                 let nums = List.map (fun s -> VFloat (float_of_string (String.trim s))) nums_str in
                 if List.length nums = cols then VVectorF nums
                 else raise (Failure "Matrix column count mismatch")
               ) row_strings in
               if List.length matrix_rows = rows then VMatrixF matrix_rows
               else raise (Failure "Matrix row count mismatch")
         | _ -> raise (Failure "Invalid dimension format. Use: rows,cols"))
    | _ -> raise (Failure "Invalid matrix input format. Use: rows,cols [[a.b,c.d],[e.f,g.h]]")
  with _ -> raise (Failure "Invalid matrix input"))
      in
      add_to_env env var value
  | Input expr ->
    (match expr with
     | Some e ->
         (match eval_expr env e with
          | VString filename ->
              (* Try to read from file *)
              try 
                let content = read_file filename in
                print_string content;  (* Print the file content *)
                print_newline();
                env  (* Return environment unchanged *)
              with Failure _ -> 
                raise (Failure ("File error: " ^ filename))
         
          | _ -> raise (Failure "Input requires a string argument"))
         
     | None ->
         raise (Failure "Input requires a filename"))
         
  | Do stmt ->
      eval_stmt env stmt

(* Top-level evaluation function *)
let eval ast =
  let _ = eval_stmt [] ast in
  ()

  
  
  


