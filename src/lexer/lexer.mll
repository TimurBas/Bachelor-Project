{
open Parser  

exception Error of string

let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str)
let char_to_string c = String.make 1 c
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let digits = ['0'-'9']+
let identifier = alpha (alpha | digits | '_')*

let digit_code = ['0'-'1'] digit digit | '2' ['0'-'4'] digit | '2' '5' ['0'-'5']
let control_char = ['?'-'_' 'a'-'z']
let whitespace = [' ' '\t' '\012' '\n']

rule token = parse 
| digits alpha+ as i            { error lexbuf ("Error: Integer can not be written as " ^ i) }
| digits as i                   { try INT (int_of_string i)
                                  with e -> error lexbuf ("Error: Raised " ^ Printexc.to_string e) }
| whitespace as w               { if w = '\n' 
                                    then Lexing.new_line lexbuf;
                                  token lexbuf }                                  
| '"'                           { STRING (read_string (Buffer.create 256) lexbuf.lex_start_p lexbuf) }
| "/*"                          { multi_line_comment 0 lexbuf }
| "let"                         { LET }
| "in"                          { IN }
| "fun"                         { FUN }
| "="                           { ASSIGN }
| "("                           { LPAREN }
| ")"                           { RPAREN }
| ","                           { COMMA }
| "->"                          { RARROW }
| "fst"                         { FST }
| "snd"                         { SND }
| identifier as id              { ID (id) }
| eof                           { EOF }
| _ as c                        { error lexbuf ("Error: Invalid character '" ^ char_to_string c ^ "'") }

and read_string buf pos = parse
| '"'                           { lexbuf.lex_start_p <- pos;
                                  Buffer.contents buf }
| [^'"' '\n' '\\'] as c         { if Char.code c > 127
                                    then error lexbuf ("Error: Invalid character '" ^ char_to_string c ^ "'")
                                    else Buffer.add_char buf c;
                                  read_string buf pos lexbuf }
| '\\'                          { escape_sequence buf lexbuf;
                                  read_string buf pos lexbuf }                                       
| eof                           { error lexbuf "Unexpected: Reached EOF without terminating the string" }
| _ as c                        { error lexbuf ("Error: Invalid character '" ^ char_to_string c ^ "'") }

and escape_sequence buf = parse
| 'n'                           { Buffer.add_char buf '\n' }
| 't'                           { Buffer.add_char buf '\t' }
| '"'                           { Buffer.add_char buf '"' }
| '\\'                          { Buffer.add_char buf '\\' }
| '^' (control_char as c)       { Buffer.add_char buf (Char.chr ((Char.code (Char.uppercase_ascii c) + 64) mod 128)) }
| '^' (_ as c)                  { error lexbuf ("Invalid control character '" ^ char_to_string c ^ "'") }
| '^' eof                       { error lexbuf "Error: Reached EOF without escaping a control character" }
| digit_code as s               { Buffer.add_char buf (Char.chr (int_of_string s)) }
| whitespace as w               { if w = '\n' 
                                    then Lexing.new_line lexbuf;
                                  multi_line_string lexbuf }
| _                             { error lexbuf ("Error: Invalid escape sequence") }
| eof                           { error lexbuf "Unexpected: Reached EOF without terminating the string" }

and multi_line_comment level = parse
| "*/"                          { if level = 0 then token lexbuf else multi_line_comment (level-1) lexbuf }
| "/*"                          { multi_line_comment (level + 1) lexbuf }
| '\n'                          { Lexing.new_line lexbuf; multi_line_comment level lexbuf }
| eof                           { error lexbuf "Error: Reached EOF in multiline comment"}
| _                             { multi_line_comment level lexbuf }

and multi_line_string = parse
| '\\'                          {  }
| '\n'                          { Lexing.new_line lexbuf;
                                  multi_line_string lexbuf }
| [' ' '\t' '\012']             { multi_line_string lexbuf }
| eof                           { error lexbuf "Unexpected: Reached EOF without terminating the multiline string" }
| _ as c                        { error lexbuf ("Error: Invalid character in multiline string '" ^ char_to_string c ^ "'") }