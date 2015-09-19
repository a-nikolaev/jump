{
  open Parser
}
let digit = ['0' - '9']
let lower_letter = ['a' - 'z']
let upper_letter = ['A' - 'Z']
let letter = ['a' - 'z' 'A' - 'Z']
rule token = 
  parse
  | '(' {OPAR}
  | ')' {CPAR}
  | ',' {COMMA}
  | '+' {PLUS}
  | '-' {MINUS}
  | '=' {EQUALS}
  | '>' {GREATER}
  | '<' {LESS}
  | ">=" {GREATEREQ}
  | "<=" {LESSEQ}
  | "->" {ARROW}
  | ">>" {JUMP}
  | "==" {EQUALS2}
  | '|' {BAR}
  | digit+ as num {INT (int_of_string num)}
  | (lower_letter | '_') (letter | digit | '_')* as s 
      {
        match s with
        | "let" -> LET
        | "where" -> WHERE
        | "in" -> IN
        | "fun" -> FUN
        | "end" -> END
        | "true" -> TRUE
        | "false" -> FALSE
        | _ -> NAME s
      }
  | (upper_letter) (letter | digit | '_')* as s 
      {
        match s with
        | _ -> LABEL s
      }
  | [' ' '\t' '\n'] {token lexbuf}
  | eof { EOF }
  | _ { token lexbuf }
 
and skip =
  parse
  | ['\n'] {token lexbuf}
  | _ { skip lexbuf }
  | eof { raise End_of_file }
