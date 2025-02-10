

exception DimensionError of string;;
exception ZeroLengthError of string;; 

(*Custom Vector Module*) 
type vector = float list ;;

type expr =  
    T | F   (* Boolean constants *)
  | ConstS of float    (* Scalar constants *)
  | ConstV of float list    (* Vector constants *)
  | Add of expr * expr   (* overloaded — disjunction of two booleans or sum of  two scalars or sum of two vectors of the same dimension *)
  | Inv of expr     (* overloaded — negation of a boolean or additive inverse of  a scalar or additive inverse of a vector *)
  | ScalProd of expr * expr   (* overloaded — conjunction of two booleans or product of a scalar with another scalar or product of a scalar and a vector *)
  | DotProd of expr * expr  (* dot product of two vectors of the same dimension *)
  | Mag of expr   (* overloaded: absolute value of a scalar or magnitude of a vector *)
  | Angle of expr * expr  (* in radians, the angle between two vectors *)
  | IsZero of expr (* overloaded: checks if a boolean expression evaluates to F,  or if a given scalar is within epsilon of 0.0 or is the vector close — within epsilon on each coordinate —  to the zero vector *)
  | Cond of expr * expr * expr  (* "if_then_else" --  if the first expr evaluates to T then evaluate the second expr, else the third expr *)
;;
  
exception Wrong of expr;;

let epsilon = 1e-10;;



  

(*-----------------------------------------------------------*)
  
  
let create (n:int) (x:float) =
  let rec create_helper n x acc =
    if n<=0 then acc
    else 
      create_helper (n - 1) x (x :: acc) 
  in
  if n < 1 then
    raise (DimensionError "Dimension cannot be less than 1")
  else
    create_helper n x []  (* Start with an empty accumulator and build the list *) 
  

(*-----------------------------------------------------------*)
    
                        
let dim v =
  if v=[] then
    raise (DimensionError "Vector should have a dimension greater than or equal to one")
  else
    List.fold_left(fun acc _->acc+1) 0 v
  
        
(*-----------------------------------------------------------*) 
  
  
let  rec is_zero v=
  if v=[] then 
    raise (DimensionError "Vector Cannot be empty") 
  else match v with
      [x]->if x<>0.0 then false else true
    | x::xs -> if x<>0.0 then false else is_zero xs
    |[] -> raise (DimensionError "Vector Cannot be empty") 
                
  

        
(*-----------------------------------------------------------*) 
  
let unit n j =
  if n<=0 then raise(DimensionError "Vector cannot be of size less than one")
  else
  if j < 1 || j > n then
    raise (DimensionError "Out of Bounds ")
  else
    let rec create_unit_vector n j acc current_index =
      if current_index > n then
        List.rev acc
      else
        let new_element = if current_index = j then 1.0 else 0.0 in
        create_unit_vector n j (new_element :: acc) (current_index + 1)
    in
    create_unit_vector n j [] 1  (* Start with index current_index=1 *)

  
(*-----------------------------------------------------------*)
  
  
let scale c v=
  if v=[] then 
    raise(DimensionError "Dimension of vector should be greater than or equal to 1")
  else
    List.map(fun x -> if x<>0.0 then (c *. x) else 0.0 ) v
      
      
(*-----------------------------------------------------------*) 

let rec addv (v1:vector) (v2:vector) = 
  if (dim v1) <>(dim v2) then raise(DimensionError "Vectors must have same dimension")
  else match (v1 ,v2) with 
      ([x],[y])-> [x+.y]
    |(x::xs,y::ys)->(x+.y)::addv xs ys 
    | _->raise(DimensionError "Vectors must have same dimension")
               
(*-----------------------------------------------------------*)

let rec dot_prod v1 v2 = 
  if v1 = [] || v2 = [] then
    raise (DimensionError "Vectors dimension should be >= 1")
  else if List.length v1 <> List.length v2 then
    raise (DimensionError "Vectors must have the same dimension")
  else
    match (v1, v2) with 
      ([x], [y]) -> (x *. y)
    | (x :: xs, y :: ys) -> ((x *. y) +. dot_prod xs ys)
    | _ -> raise(DimensionError "Vectors must have the same dimension")


  
      
(*-----------------------------------------------------------*)


let inv v=
  if v=[] then 
    raise(DimensionError "Dimension of vector should be greater than or equal to 1")
  else
    List.map(fun x-> if x<>0.0 then (-1.0)*.x else 0.0) v 
      
                                  
(*-----------------------------------------------------------*)


let length v=
  if v=[] then 
    raise(DimensionError "Dimension of vector should be greater than or equal to 1")
  else
    let a=List.fold_left(fun acc x-> acc +. (x *. x)) 0.0 v in  
    Float.sqrt a 
           
            
(*-----------------------------------------------------------*)
    
let angle v1 v2 =
  if length v1 = 0.0 || length v2 = 0.0 then 
    raise (ZeroLengthError "Vectors must have non-zero length")
  else
    let a = (dot_prod v1 v2) /. ((length v1) *. (length v2)) in
    if a <= 1.0 && a >= -1.0 then 
      acos a
    else if a > 1.0 then 
      acos 1.0
    else if a < -1.0 then 
      acos (-1.0) 
    else 
      raise (DimensionError "Dimension Mismatch")
    
    






type types=Bool|Scalar|Vector of int;; 

let rec type_of x=
  try (match x with(* expr->types*)
        T->Bool
      |F->Bool
      |ConstS _ ->Scalar
      |ConstV v -> if(v<>[]) then (Vector (dim v)) else raise(Wrong x)
      | Add(e1, e2) ->
          (let v1 = type_of e1 in
           let v2 = type_of e2 in
           match (v1, v2) with
             (Bool, Bool) -> Bool
           | (Scalar , Scalar) -> Scalar
           | (Vector n1, Vector n2) when n1 = n2 -> Vector n1
           | _ -> raise (Wrong x))
      
      |Inv e ->
          (match (type_of e) with 
             Vector n -> if(n<>0) then (Vector n) else raise(Wrong x)
           |Bool -> Bool
           |Scalar -> Scalar 
          )
      |ScalProd(e1,e2) ->
          (match ((type_of e1),(type_of e2)) with 
             (Bool,Bool) -> Bool
           |(Bool,_)->raise(Wrong x)
           |(_,Bool) ->raise(Wrong x)
           |(Scalar,Scalar)->Scalar 
           |(Scalar,Vector n)-> Vector n 
           |(Vector n,Scalar)-> Vector n 
           |(Vector _ , Vector _) ->raise(Wrong x))
      |DotProd(e1,e2)->
          (match ((type_of e1),(type_of e2))with 
             (Vector n1,Vector n2) -> if (n1=n2) then Scalar else (raise (Wrong x))
           |(_,_) ->raise(Wrong x))
      |Mag e-> 
          (match (type_of e) with 
             Scalar -> Scalar
           |Vector _ -> Scalar
           |Bool -> raise (Wrong x))
      |Angle(e1,e2) ->
          ( let (t1, t2) = (type_of e1, type_of e2) in
            match (t1, t2) with
            | (Vector n1, Vector n2) -> 
                if n1 = n2 then Scalar else raise (Wrong x)
            | _ -> raise (Wrong x)
          )
      |IsZero e ->
          (match (type_of e) with 
             Bool ->  Bool 
           |Vector _ -> Bool
           |Scalar -> Bool)
      |Cond (e1,e2,e3) ->
          (if( (type_of e1 = Bool) && ((type_of e2)=(type_of e3))) then (type_of e2 )
           else raise (Wrong x))
    )   
  with 
    DimensionError _  -> (raise (Wrong x))
  |ZeroLengthError _  -> (raise (Wrong x))
  |_ -> raise (Wrong x)
  
;;

type values = B of bool |  S of float | V of vector;; 

let rec eval x =
  try (match x with
        T-> B true
      |F -> B false
      |ConstS f ->  S f
      |ConstV v -> if(v<>[]) then (V v) else raise (Wrong x)
      |Add(e1,e2)->
          (match (eval e1,eval e2) with 
             (B b1,B b2) -> B (b1 || b2)
           |(V v1, V v2) ->if(((dim v1)=(dim v2))&& (dim v1 <> 0)) then V (addv v1 v2) else raise(Wrong x)
           |(S f1, S f2) -> S (f1+.f2)
           |(_,_)-> raise (Wrong x))
      |Inv e ->
          (match (eval e) with 
             B b1 -> B (not b1)
           |S f -> S (0. -. f)
           | V v -> if(dim v <> 0) then (V (inv v)) else raise (Wrong x) )
      
      |ScalProd (e1,e2) ->
          (match ((eval e1),(eval e2)) with 
             (B b1, B b2) -> B (b1 && b2)
           |(S f1,S f2) -> S (f1*.f2)
           |(S f1,V v1) ->  if((dim v1) <> 0) then (V (scale f1 v1)) else raise (Wrong x)
           |(V v, S f) ->  if((dim v) <> 0) then (V(scale f v)) else raise (Wrong x)
           |(_,_) -> raise (Wrong x) )
      
      |DotProd(e1,e2) ->
          (match ((eval e1),(eval e2)) with 
             (V v1,V v2) -> if(((dim v1)=(dim v2))&& ((dim v1) <> 0)) then S (dot_prod v1 v2) else raise (Wrong x)
           |(_,_) -> raise (Wrong x))
      |Mag e -> 
          (match (eval e) with 
             S f -> if(f > 0.) then (S f) else (S (-1.0 *. f))
           | V v -> if((dim v) <>0)then (S (length v)) else raise (Wrong x) 
           |_ -> raise (Wrong x) )
      |Angle ( e1, e2) ->
          (match ((eval e1),(eval e2)) with 
             (V v1,V v2) -> if(((dim v1)=(dim v2))&& ((dim v1) <> 0)) then (S (angle v1 v2)) else raise (Wrong x)
           |(_,_) -> raise (Wrong x))
      | IsZero e ->
          (match (eval e) with 
             B b -> if (b=false) then (B true) else (B false) 
           | S f -> if (f >= 0.0) then 
                 (if (f <= epsilon) then (B true) else (B false))
               else 
                 (if ((-1.0 *. f) <= epsilon) then (B true) else (B false))
           | V v -> B (is_zero v))
      
      |Cond(e1,e2,e3) -> 
          (match((eval e1),(eval e2),(eval e3)) with 
             (B b, V v1, V v2) ->  
               (if(((dim v1) = 0) || ((dim v2) = 0)) then raise (Wrong x)
                else
                  (if(b=true) then (V v1) else (V v2))
               )
           |(B b, S f1, S f2)-> if(b=true)then (S f1) else (S f2)
           | (B b, B b1, B b2) -> if(b=true)then (B b1) else (B b2)
           |(_,_,_)-> raise (Wrong x)
          )
    )
  with 
    DimensionError _ -> raise(Wrong x)
  |ZeroLengthError _ -> raise(Wrong x)
  | _ -> raise(Wrong x)
      
;;
  
