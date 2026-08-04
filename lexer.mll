{
open Ast
}

let digit = ['0'-'9']
let chars = ['A'-'Z' 'a'-'z']
let space = [' ' '\n' '\t' '\r']+
let int_list = "["space*(('-'?digit+(space)*",")space*)*'-'?digit+space*"]"
let float_list = "["space*((('-'?digit+"."digit*)(space)*",")space*)*('-'?digit+"."digit*)space*"]"
let string = '"' [^ '"']* '"'  

rule token = parse
    | space {token lexbuf}
    | string as str  { 
        let content = String.sub str 1 (String.length str - 2) in
        Grammar.STRING(content)
    }  
    | ("bool" | "float" | "vectori" |"vectorf"| "matrixi"|"matrixf" | "true" | "false" | "while" | "do" | "done" | "if" | "then" | "else" | "dim" | "abs" |"sqrt"| "mag" | "angle"|"trace" | "transpose" | "det" | "inv" | "print"| "input" )['A'-'Z' 'a'-'z' '0'-'9' '\'' '_']+ as str  {Grammar.VAR (str)} 
    | "int" {Grammar.INT_TYPE}
    | "bool"    {Grammar.BOOL_TYPE}
    | "float"   {Grammar.FLOAT_TYPE}
    | "vectori"  {Grammar.VECTORI_TYPE}
    | "vectorf"  {Grammar.VECTORF_TYPE}
    | "matrixi"  {Grammar.MATRIXI_TYPE}
    | "matrixf"  {Grammar.MATRIXF_TYPE}
    | "true"    {Grammar.BOOL(true)}
    | "false"   {Grammar.BOOL(false)}
    | "while"   {Grammar.WHILE}
    | "do"      {Grammar.DO}
    | "done"    {Grammar.DONE}
    | "if"      {Grammar.IF}
    | "then"    {Grammar.THEN}
    | "else"    {Grammar.ELSE}
    | "print"   {Grammar.PRINT}
    | "input"   {Grammar.INPUT}
     | '-'?digit"."?digit*"e""-"?digit+ as num  {Grammar.FLOAT (float_of_string num)}
    | "+"   {Grammar.PLUS}
    | "*"   {Grammar.MUL}
    | "/"   {Grammar.DIV}
    | "="   {Grammar.EQ}
    | ">"   {Grammar.GT}
    | "<"   {Grammar.LT}
    | ">="  {Grammar.GEQ}
    | "<="  {Grammar.LEQ}
    | "!="  {Grammar.NEQ}
    | '-'?digit+ as num {Grammar.INT (int_of_string num)}
    | '-'?digit+"."digit* as num    {Grammar.FLOAT (float_of_string num)}
    | "-"  {Grammar.SUB}
    | '"' ([^ '"']*) '"' { STRING (Lexing.lexeme lexbuf) }
    |"["space*(('-'?digit+(space)*",")space*)*'-'?digit+space*"]" as vec_str {
        
        let values = String.sub vec_str 1 ((String.length vec_str) - 2) |>
            String.split_on_char ',' |>
            List.map String.trim |>
            List.map (fun s -> Ast.Int(int_of_string s))
        in
        Grammar.VECTORI values  (* Return just the values *)
    }
    | "["space*((('-'?digit+"."digit*)(space)*",")space*)*('-'?digit+"."digit*)space*"]" as vec_str {
        
        let values = String.sub vec_str 1 ((String.length vec_str) - 2) |>
            String.split_on_char ',' |>
            List.map String.trim |>
            List.map (fun s -> Ast.Float(float_of_string s))
        in
        Grammar.VECTORF values
    }
    
    | "["space*((int_list(space)*",")space*)*(int_list)space*"]" as mat_str {
        
        let len = String.length mat_str in
        let inner = String.sub mat_str 1 (len - 2) in
        let split_rows s =
          let len = String.length s in
          let rec aux i j depth acc =
            if j = len then
              let part = String.trim (String.sub s i (j - i)) in
              List.rev (part :: acc)
            else
              let c = s.[j] in
              if c = '[' then
                aux i (j + 1) (depth + 1) acc
              else if c = ']' then
                aux i (j + 1) (depth - 1) acc
              else if c = ',' && depth = 0 then
                let part = String.trim (String.sub s i (j - i)) in
                aux (j + 1) (j + 1) depth (part :: acc)
              else
                aux i (j + 1) depth acc
          in
          aux 0 0 0 []
        in
        let row_strings = split_rows inner in
        let convert_row row_str =
            let row_len = String.length row_str in
            let row_inner = String.sub row_str 1 (row_len - 2) in
            let rec split s =
                try
                    let idx = String.index s ',' in
                    let part = String.sub s 0 idx in
                    let rest = String.sub s (idx + 1) (String.length s - idx - 1) in
                    (String.trim part) :: split rest
                with Not_found ->
                    [String.trim s]
            in
            List.map (fun s -> Int (int_of_string s)) (split row_inner)
        in
        let rows = List.map (fun row_str ->
            Vectori(convert_row row_str)
        ) row_strings
        in
        Grammar.MATRIXI rows
    }
    | "["space*((float_list(space)*",")space*)*(float_list)space*"]" as mat_str {
        
        let len = String.length mat_str in
        let inner = String.sub mat_str 1 (len - 2) in
        let split_rows s =
          let len = String.length s in
          let rec aux i j depth acc =
            if j = len then
              let part = String.trim (String.sub s i (j - i)) in
              List.rev (part :: acc)
            else
              let c = s.[j] in
              if c = '[' then
                aux i (j + 1) (depth + 1) acc
              else if c = ']' then
                aux i (j + 1) (depth - 1) acc
              else if c = ',' && depth = 0 then
                let part = String.trim (String.sub s i (j - i)) in
                aux (j + 1) (j + 1) depth (part :: acc)
              else
                aux i (j + 1) depth acc
          in
          aux 0 0 0 []
        in
        let row_strings = split_rows inner in
        let convert_row row_str =
            let row_len = String.length row_str in
            let row_inner = String.sub row_str 1 (row_len - 2) in
            let rec split s =
                try
                    let idx = String.index s ',' in
                    let part = String.sub s 0 idx in
                    let rest = String.sub s (idx + 1) (String.length s - idx - 1) in
                    (String.trim part) :: split rest
                with Not_found ->
                    [String.trim s]
            in
            List.map (fun s -> Float (float_of_string s)) (split row_inner)
        in
        let rows = List.map (fun row_str ->
            Vectorf(convert_row row_str)
        ) row_strings
        in
        Grammar.MATRIXF rows
    }
    | "&&"     {Grammar.AND}
    | "||"      {Grammar.OR}
    | "!"     {Grammar.NOT}
    | "loop"     {Grammar.LOOP}
    | "("       {Grammar.LPREN}
    | ")"       {Grammar.RPREN}
    | "["       {Grammar.LSQU}
    | "]"       {Grammar.RSQU}
    | "{"       {Grammar.LBRA}
    | "}"       {Grammar.RBRA}
    | ","       {Grammar.COMMA}
    | ";"       {Grammar.SEMICOL}
    | ":="      {Grammar.ASSGN}
    | "dim"     {Grammar.DIM}
    | "abs"     {Grammar.ABS}
    | "%"       {Grammar.MOD}
    | "$"       {Grammar.DOLL}
    | "mag"     {Grammar.MAG}
    | "angle"   {Grammar.ANGLE}
    | "inv"  {Grammar.INVERSE}
    | "trace" {Grammar.TRACE}
    | "transpose"   {Grammar.TRANSMAT}
    | "sqrt"        {Grammar.SQRT}
    | "det"         {Grammar.DET}
    | "##"[^'\n']*  {token lexbuf}
    | "#~"([^'~'] | "~"[^'#'])*"~#"   {token lexbuf}
| ("bool" | "float" | "vectori" |"vectorf"| "matrixi"|"matrixf" | "true" | "false" | "loop" | "while" | "do" | "done" | "if" | "then" | "else" | "dim" | "abs" | "mag" |"sqrt"| "angle" |"trace"| "transpose" | "det" | "inv" | "print" | "input" )*['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '\'' '_']* as str  {Grammar.VAR (str)}
    | eof           {Grammar.EOF}
    | _     {Grammar.ERROR}
