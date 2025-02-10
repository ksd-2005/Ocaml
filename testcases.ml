
open A2 
let%test "type_of T" = (type_of T = Bool)
let%test "type_of F" = (type_of F = Bool)   
let%test "type_of (ConstS 5.0)" = (type_of (ConstS 5.0) = Scalar) 
let%test "type_of (ConstS 0.0)" = (type_of (ConstS 0.0) = Scalar) 
let%test "type_of (Add T F)" = ( type_of (Add (T,F)) = Bool)
let%test "type_of (Add T T)" = (type_of (Add (T,T)) = Bool)  
let%test "type_of (Add (ConstS 4.0 ) (ConstS 3.0 ))" = (type_of (Add ((ConstS 4.0 ),(ConstS 3.0 )))  = Scalar) 
let%test "type_of (Add ((ConstV [3.0; 4.0]),(ConstV [4.0; 5.0])))"=(type_of (Add ((ConstV [3.0; 4.0]),(ConstV [4.0; 5.0])) )=Vector 2) 
(*Wrong test case for Add*)
let%test "type_of (Add ((ConstV [3.0; 4.0]),(ConstV [4.0]))) raises Wrong(Add ((ConstV [3.0; 4.0]),(ConstV [4.0])))" = 
  (try
     let _ = type_of (Add ((ConstV [3.0; 4.0]),(ConstV [4.0]))) in false
   with
   | Wrong e -> e =Add ((ConstV [3.0; 4.0]),(ConstV [4.0]))
   | _ -> false
  )

let%test "type_of (Add (ConstS 3.0, F)) raises Wrong(Add (ConstS 3.0, F))" = 
  (try
     let _ = type_of (Add (ConstS 3.0, F)) in false
   with
   | Wrong e -> e = Add (ConstS 3.0, F)
   | _ -> false
  )  
let%test "type_of (Add (ConstV [1.0; 2.0], T)) raises Wrong(Add (ConstV [1.0; 2.0], T))" = 
  (try
     let _ = type_of (Add (ConstV [1.0; 2.0], T)) in false
   with
   | Wrong e -> e = Add (ConstV [1.0; 2.0], T)
   | _ -> false
  )
  
  

(*Test cases for Inv*)
let%test "type_of (Inv T )" = (type_of (Inv T ) = Bool) 
let%test "type_of (Inv (ConstS 4.0) )" = (type_of (Inv (ConstS 4.0) ) = Scalar)
let%test "type_of (Inv (ConstV [3.0; 4.0]) )" = (type_of (Inv (ConstV [3.0; 4.0]) ) = Vector 2)
let%test "type_of (Inv (ConstV [])) raises Wrong((Inv (ConstV []) ))" = 
  (try
     let _ = type_of (Inv (ConstV [])) in false
   with
   | Wrong e -> e =  (Inv (ConstV []))
   | _ -> false
  )
  
  
(*TestCases for ScalProd*)
let%test "type_of(ScalProd((ConstS 5.0),(ConstS 4.0)))"=(type_of(ScalProd((ConstS 5.0),(ConstS 4.0)))=Scalar )
let%test "type_of (ScalProd ((ConstV [3.0; 4.0]),(ConstS 5.0)))"=(type_of (ScalProd ((ConstV [3.0; 4.0]),(ConstS 5.0)) )=Vector 2) 
let%test "type_of (ScalProd ((ConstS 5.0),(ConstV [1.0; 2.0;3.0])))"=(type_of (ScalProd ((ConstS 5.0),(ConstV [1.0; 2.0;3.0])) )=Vector 3) 
let%test "type_of (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) raises Wrong(ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = 
  (try
     let _ = type_of (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) in false
   with
   | Wrong e -> e = ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])
   | _ -> false
  ) 
let%test "type_of (ScalProd (ConstV [1.0; 2.0], T)) raises Wrong(ScalProd (ConstV [1.0; 2.0], T))" = 
  (try
     let _ = type_of (ScalProd (ConstV [1.0; 2.0], T)) in false
   with
   | Wrong e -> e = ScalProd (ConstV [1.0; 2.0], T)
   | _ -> false
  ) 
let%test "type_of (ScalProd (F, ConstS 3.0)) raises Wrong(ScalProd (F, ConstS 3.0))" = 
  (try
     let _ = type_of (ScalProd (F, ConstS 3.0)) in false
   with
   | Wrong e -> e = ScalProd (F, ConstS 3.0)
   | _ -> false
  ) 
let%test "type_of (ScalProd (T, ConstV [3.0; 4.0])) raises Wrong(ScalProd (T, ConstV [3.0; 4.0]))" = 
  (try
     let _ = type_of (ScalProd (T, ConstV [3.0; 4.0])) in false
   with
   | Wrong e -> e = ScalProd (T, ConstV [3.0; 4.0])
   | _ -> false
  ) 
let%test "type_of (ScalProd (ConstS 3.0, ConstV [])) raises Wrong(ScalProd (ConstS 3.0, ConstV []))" = 
  (try
     let _ = type_of (ScalProd (ConstS 3.0, ConstV [])) in false
   with
   | Wrong e -> e =  (ScalProd (ConstS 3.0, ConstV []))
   | _ -> false
  )
  
  
(*DotProd of expr * expr*)

let%test "type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Scalar) 
                                                                      
let%test "type_of (DotProd (ConstV [], ConstV [1.0; 2.0])) raises Wrong(DotProd (ConstV [], ConstV [1.0; 2.0]))" =
  (try
     let _ = type_of (DotProd (ConstV [], ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = (DotProd (ConstV [], ConstV [1.0; 2.0]))
   | _ -> false
  )

let%test "type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0])) raises Wrong(DotProd (ConstV [1.0; 2.0], ConstV [3.0]))" =
  (try
     let _ = type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0])) in false
   with
   | Wrong e -> e = DotProd (ConstV [1.0; 2.0], ConstV [3.0])
   | _ -> false
  )

let%test "type_of (DotProd (ConstS 3.0, ConstV [1.0; 2.0])) raises Wrong(DotProd (ConstS 3.0, ConstV [1.0; 2.0]))" =
  (try
     let _ = type_of (DotProd (ConstS 3.0, ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = DotProd (ConstS 3.0, ConstV [1.0; 2.0])
   | _ -> false
  )
  
let%test "type_of (DotProd (T, ConstV [1.0; 2.0])) raises Wrong(DotProd (T, ConstV [1.0; 2.0]))" =
  (try
     let _ = type_of (DotProd (T, ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = DotProd (T, ConstV [1.0; 2.0])
   | _ -> false
  )

  

(*Mag*)

let%test "type_of (Mag T) raises Wrong(Mag T)" =
  (try
     let _ = type_of (Mag T) in false
   with
   | Wrong e -> e = Mag T
   | _ -> false
  )

  
let%test "type_of (Mag (ConstV [])) raises Wrong(Mag (ConstV []))" =
  (try
     let _ = type_of (Mag (ConstV [])) in false
   with
   | Wrong e -> e = (Mag (ConstV []))
   | _ -> false
  )

  
let%test "type_of (Mag (ConstV [-3.0; -4.0]))" =
  (type_of (Mag (ConstV [(-3.0); (-4.0)])) = Scalar)

  
let%test "type_of (Mag (ConstS (-5.0)))" =
  (type_of (Mag (ConstS (-5.0))) = Scalar)




(*Angle*)

let%test "type_of (Angle (ConstV [1.0; 2.0], ConstV [3.0])) raises Wrong(Angle (ConstV [1.0; 2.0], ConstV [3.0]))" =
  (try
     let _ = type_of (Angle (ConstV [1.0; 2.0], ConstV [3.0])) in false
   with
   | Wrong e -> e = Angle (ConstV [1.0; 2.0], ConstV [3.0])
   | _ -> false
  )

let%test "type_of (Angle (ConstV [], ConstV [1.0; 2.0])) raises Wrong(Angle (ConstV [], ConstV [1.0; 2.0]))" =
  (try
     let _ = type_of (Angle (ConstV [], ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e =  (Angle (ConstV [], ConstV [1.0; 2.0]))
   | _ -> false
  )

  
let%test "type_of (Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0])) raises Wrong(Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0]))" =
  (type_of (Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0])) =Scalar) 

  
let%test "type_of (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]))" =
  (type_of (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])) = Scalar)

  
let%test "type_of (Angle (ConstS 3.0, ConstS 4.0)) raises Wrong(Angle (ConstS 3.0, ConstS 4.0))" =
  (try
     let _ = type_of (Angle (ConstS 3.0, ConstS 4.0)) in false
   with
   | Wrong e -> e = Angle (ConstS 3.0, ConstS 4.0)
   | _ -> false
  )

  
    (*IsZero *)

let%test "type_of (IsZero (ConstV [0.0; 0.0; 0.0]))" =
  (type_of (IsZero (ConstV [0.0; 0.0; 0.0])) = Bool)

  
let%test "type_of (IsZero (ConstS 0.0))" =
  (type_of (IsZero (ConstS 0.0)) = Bool)
  
let%test "type_of (IsZero F)" =
  (type_of (IsZero F) = Bool)

  
let%test "type_of (IsZero (ConstV [])) raises Wrong(IsZero (ConstV []))" =
  (try
     let _ = type_of (IsZero (ConstV [])) in false
   with
   | Wrong e -> e = (IsZero (ConstV []))
   | _ -> false
  )

let%test "type_of (IsZero (Inv (Add (ScalProd (ConstS (-1.0), ConstS 0.0), ConstS 0.0))))" = 
  (type_of (IsZero (Inv (Add (ScalProd (ConstS (-1.0), ConstS 0.0), ConstS 0.0))))) = Bool
  


(*Cond *)

let%test "type_of (Cond (T, ConstS 3.0, ConstS 4.0))" =
  (type_of (Cond (T, ConstS 3.0, ConstS 4.0)) = Scalar)

let%test "type_of (Cond (F, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  (type_of (Cond (F, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

  
let%test "type_of (Cond (ConstS 2.0, ConstS 3.0, ConstS 4.0)) raises Wrong(Cond (ConstS 2.0, ConstS 3.0, ConstS 4.0))" =
  (try
     let _ = type_of (Cond (ConstS 2.0, ConstS 3.0, ConstS 4.0)) in false
   with
   | Wrong e -> e = Cond (ConstS 2.0, ConstS 3.0, ConstS 4.0)
   | _ -> false
  )

  
let%test "type_of (Cond (T, ConstS 3.0, ConstV [1.0; 2.0])) raises Wrong(Cond (T, ConstS 3.0, ConstV [1.0; 2.0]))" =
  (try
     let _ = type_of (Cond (T, ConstS 3.0, ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = Cond (T, ConstS 3.0, ConstV [1.0; 2.0])
   | _ -> false
  )

let%test "type_of (Cond (IsZero (Add (ConstS (-1.0), ConstS 1.0)), Inv (ConstS 5.0), Add (ConstS 2.0, ConstS 3.0)))" =
  (type_of (Cond (IsZero (Add (ConstS (-1.0), ConstS 1.0)), Inv (ConstS 5.0), Add (ConstS 2.0, ConstS 3.0)))) = Scalar 
  



(*Test Cases for eval*)

let%test "eval (T)"=(eval(T)=(B true))
let%test "eval (ConstS 4.0)"=(eval(ConstS 4.0)=(S 4.0))
let%test "eval (ConstV [2.0;3.0])"=(eval(ConstS 4.0)=(S 4.0)) 
let%test "eval (Add (ConstS 3.0, ConstS 4.0)) = S 7.0" = 
  eval (Add (ConstS 3.0, ConstS 4.0)) = S 7.0

let%test "eval (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [4.0; 6.0]" = 
  eval (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [4.0; 6.0]

let%test "eval (Add (T, F)) = B true" = 
  eval (Add (T, F)) = (B true)
let%test "eval (Add (ConstV [], ConstV [1.0])) raises Wrong (Add (ConstV [], ConstV [1.0]))" = 
  (try eval (Add (ConstV [], ConstV [1.0])) = V [] with 
   | Wrong e -> e = (Add (ConstV [], ConstV [1.0]))
   | _ -> false)

let%test "eval (Add (ConstV [], ConstV [])) raises Wrong (Add (ConstV [], ConstV []))" = 
  (try eval (Add (ConstV [], ConstV [])) = V [] with 
   | Wrong e -> e =  (Add (ConstV [], ConstV []))
   | _ -> false)

let%test "eval (Add (Inv (Add (ConstS 2.0, ConstS (-2.0)))), Add (ConstS 3.0, Inv (ConstS (-3.0))))) = S 6.0" =
  eval (Add ((Inv (Add (ConstS 2.0, ConstS (-2.0)))), Add (ConstS 3.0, Inv (ConstS (-3.0))))) = S 6.0 


  (*Inv*)
let%test "eval (Inv (ConstV [])) raises Wrong (Inv (ConstV []))" =
  (try
     let _ = eval (Inv (ConstV [])) in false
   with
   | Wrong e -> e = Inv (ConstV [])
   | _ -> false)

let%test "eval (Inv T) = B false" =
  eval (Inv T) = B false

let%test "eval (Inv F) = B true" =
  eval (Inv F) = B true

let%test "eval (Inv (ConstS (-3.0))) = S 3.0" =
  eval (Inv (ConstS (-3.0))) = S 3.0

let%test "eval (Inv (ConstV [1.0; -2.0; 3.0])) = V [-1.0; 2.0; -3.0]" =
  eval (Inv (ConstV [1.0; -2.0; 3.0])) = V [-1.0; 2.0; -3.0]

let%test "eval (Inv (Add (ConstS 2.0, Inv (ConstS (-2.0))))) = S 4.0" =
  eval (Inv (Add (ConstS 2.0, Inv (ConstS (-2.0))))) = (S (-4.0))

    (*ScalProd*)
let%test "eval (ScalProd (T, T)) = B true" =
  eval (ScalProd (T, T)) = B true

let%test "eval (ScalProd (T, F)) = B false" =
  eval (ScalProd (T, F)) = B false

let%test "eval (ScalProd (ConstS (-4.0), ConstS 2.5)) = S (-10.0)" =
  eval (ScalProd (ConstS (-4.0), ConstS 2.5)) = S (-10.0)

let%test "eval (ScalProd (ConstS 2.0, ConstV [1.0; -3.0; 2.5])) = V [2.0; -6.0; 5.0]" =
  eval (ScalProd (ConstS 2.0, ConstV [1.0; (-3.0); 2.5])) = V [2.0; (-6.0); 5.0]

let%test "eval (ScalProd (ConstS 3.0, ConstV [])) raises Wrong (ScalProd (ConstS 3.0, ConstV []))" =
  (try
     let _ = eval (ScalProd (ConstS 3.0, ConstV [])) in false
   with
   | Wrong e -> e = ScalProd (ConstS 3.0, ConstV [])
   | _ -> false)

let%test "eval (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) raises Wrong (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  (try
     let _ = eval (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) in false
   with
   | Wrong e -> e = ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])
   | _ -> false)

let%test "eval (ScalProd (ConstS 2.0, Inv (Add (ConstS (-1.0), ConstS 1.0)))) = S 0.0" =
  eval (ScalProd (ConstS 2.0, Inv (Add (ConstS (-1.0), ConstS 1.0)))) = (S 0.0)

  
  (*DotProd*)
let%test "eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = S 11.0" =
  eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = S 11.0

let%test "eval (DotProd (ConstV [], ConstV [1.0])) raises Wrong (DotProd (ConstV [], ConstV [1.0]))" =
  (try
     let _ = eval (DotProd (ConstV [], ConstV [1.0])) in false
   with
   | Wrong e -> e = DotProd (ConstV [], ConstV [1.0])
   | _ -> false)

let%test "eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0])) raises Wrong (DotProd (ConstV [1.0; 2.0], ConstV [3.0]))" =
  (try
     let _ = eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0])) in false
   with
   | Wrong e -> e = DotProd (ConstV [1.0; 2.0], ConstV [3.0])
   | _ -> false)

    (*Mag*)
let%test "eval (Mag (ConstS (-5.0))) = S 5.0" =
  eval (Mag (ConstS (-5.0))) = S 5.0

let%test "eval (Mag (ConstV [3.0; 4.0])) = S 5.0" =
  eval (Mag (ConstV [3.0; 4.0])) = S 5.0

let%test "eval (Mag (ConstV [])) raises Wrong (Mag (ConstV []))" =
  (try
     let _ = eval (Mag (ConstV [])) in false
   with
   | Wrong e -> e = Mag (ConstV [])
   | _ -> false)

let%test "eval (Mag T) raises Wrong (Mag T)" =
  (try
     let _ = eval (Mag T) in false
   with
   | Wrong e -> e = Mag T
   | _ -> false)

  
    (*Angle*)
let%test "eval (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])) = S (Float.pi /. 2.0)" =
  eval (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])) = S (Float.pi /. 2.0)

let%test "eval (Angle (ConstV [1.0; 2.0], ConstV [1.0])) raises Wrong (Angle (ConstV [1.0; 2.0], ConstV [1.0]))" =
  (try
     let _ = eval (Angle (ConstV [1.0; 2.0], ConstV [1.0])) in false
   with
   | Wrong e -> e = Angle (ConstV [1.0; 2.0], ConstV [1.0])
   | _ -> false)

let%test "eval (Angle (ConstS 5.0, ConstS 3.0)) raises Wrong (Angle (ConstS 5.0, ConstS 3.0))" =
  (try
     let _ = eval (Angle (ConstS 5.0, ConstS 3.0)) in false
   with
   | Wrong e -> e = Angle (ConstS 5.0, ConstS 3.0)
   | _ -> false)

let%test "eval (Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0])) raises Wrong (Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0]))" =
  (try
     let _ = eval (Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0])
   | _ -> false)

    (*IsZero*)
let%test "eval (IsZero (ConstS 0.0)) = B true" =
  eval (IsZero (ConstS 0.0)) = B true

let%test "eval (IsZero (ConstS 5.0)) = B false" =
  eval (IsZero (ConstS 5.0)) = B false

let%test "eval (IsZero (ConstV [0.0; 0.0; 0.0])) = B true" =
  eval (IsZero (ConstV [0.0; 0.0; 0.0])) = B true

let%test "eval (IsZero (ConstV [])) raises Wrong (IsZero (ConstV []))" =
  (try
     let _ = eval (IsZero (ConstV [])) in false
   with
   | Wrong e -> e = IsZero (ConstV [])
   | _ -> false)

let%test "eval (IsZero T) = B false" =
  eval (IsZero T) = B false

let%test "eval (IsZero (Add (ConstS (-3.0), ConstS 3.0))) = B true" =
  eval (IsZero (Add (ConstS (-3.0), ConstS 3.0))) = B true

(*Cond*)
let%test "eval (Cond (T, ConstS 5.0, ConstS 3.0)) = S 5.0" =
  eval (Cond (T, ConstS 5.0, ConstS 3.0)) = S 5.0

let%test "eval (Cond (F, ConstS 5.0, ConstS 3.0)) = S 3.0" =
  eval (Cond (F, ConstS 5.0, ConstS 3.0)) = S 3.0

let%test "eval (Cond (ConstS 1.0, ConstS 5.0, ConstS 3.0)) raises Wrong (Cond (ConstS 1.0, ConstS 5.0, ConstS 3.0))" =
  (try
     let _ = eval (Cond (ConstS 1.0, ConstS 5.0, ConstS 3.0)) in false
   with
   | Wrong e -> e = Cond (ConstS 1.0, ConstS 5.0, ConstS 3.0)
   | _ -> false)

let%test "eval (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [1.0; 2.0]" =
  eval (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [1.0; 2.0]

let%test "eval (Cond (IsZero (Add (ConstS (-2.0), ConstS 2.0)), ConstS 10.0, ConstS 20.0)) = S 10.0" =
  eval (Cond (IsZero (Add (ConstS (-2.0), ConstS 2.0)), ConstS 10.0, ConstS 20.0)) = S 10.0


