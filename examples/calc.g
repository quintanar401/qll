/ Lexer

top: NUM | WS | ID | KEYWORDS | ">" | "<" | ">=" | "<=" | "==" | "<>" | "-" | "+" | "*" | "/" | "(" | ")" | "%" | "=" | "++" | "--"

ID: "."? ID2 ("."|ID2|NUM2)*
ID2: "a" - "z" | "A" - "Z"

NUM: NUM2+ NUM3? | "." NUM2+ NUM3? | NUM2+ "." NUM2+ NUM3?
NUM2: "0"-"9"
NUM3: "e" NUM2+

WS: (" " | "\t" | "\n" | "\r")+

KEYWORDS: "cos" | "sin" | "exp" | "log" | "tan" | "and" | "or" | "not"

Q .ll.lexmap:{[v] v:.[v;(0;j:where `ID=v 1);`$]}

LEXER top

/ Parser

TOKENS: NUM ID

expr:
      expr ("++"|"--")                                            {: {$["+"=y 0;x+1;x-1]}/[v1;v2]}
    | expr ("*" | "/" {:(%)}) expr                                {: .cc.exp[v1;v2]}
    | expr ("+" | "-") expr                                       {: .cc.exp[v1;v2]}
    | expr ("and" | "or") expr                                    {: .cc.exp[v1;v2]}
    | expr (">" | "<" | ">=" | "<=" | "==" {:(=)}| "<>") expr     {: .cc.exp[v1;v2]}
    | "(" expr ")"                                                2
    | "%" expr                                                    {: reciprocal v2}
    | ("++"|"--") expr                                            {: v2+ -1 1 "+"=v1 0}
    | "not" expr                                                  {: not v2}
    | "-" expr                                                    {: neg v2}
    | ID "=" expr                                                 { v1 set v3; :v3}
    | ID                                                          {: get v1}
    | NUM                                                         {: get v1}
    | ("cos" | "sin" | "exp" | "log" | "tan") "(" expr ")"        {: value[v1] v3}


PARSER expr

Q .cc.exp:{{$[10=type y 0;value y 0;y 0][x;y 1]}/[x;y]}
Q .c.e:{.ll.parse x}

\

Calc demonstrates the left rec elimination

\l qll.q
\l calc.g

.ll.parse "1+1" or
c)1+1
c)(a=2)+a*10++---a
