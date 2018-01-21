/ Q light: like Q but with operation priorities
/ Q light exprs will be compiled into Q exprs
/ q).ll.parse "2*10+1"
/ "((2)* 10)+ 1"
/ q).ll.parse "select a*10+1 from ([a:1 2 3] b: 2 5 6) where b*2+3>a"
/ "select ((a)* 10)+ 1 from ([a:1 2 3](b):2 5 6) where (((b)* 2)+ 3)> a"

GNAME Q

lex: NAME | NUM | SYM | STR | ASSIGN | COLON | KEYWORDS | WS | COMMENT | "(" | ")" | "[" | "]" | "{" | "}" | ";"
   | "+" | "*" | "/" | "~" | "!" | "@" | "#" | "$" | "^" | "&" | "=" | "<" | ">" | "?" | "." | "|" | "_" | "<>" | "<=" | ">=" | "," | "%" | "'"

NAME: A ANDU* | "." A ANDU*
NUM: (WS0 "-")? NUM2
NUM2: N (AND (AND | ":")*)? | "." N (AND | ":")*

SYM: ("`" (AND ANDU* (":" (ANDU | ":" | "/")*)?)?)+

STR: "\"" STR2
STR2: ("\"\\"^)* ("\"" | "\\" ("n" | "r" | "t" | N N N) STR2)

ASSIGN: ("," | "+" | "-" | "*" | "/") ":" ":"? | "::"
ASSIGN2: ":=" | "::=" | ("," | "+" | "-" | "*" | "/") "="
COLON: ":"

WS: WS0+
WS0: " " | "\t" | "\r" | "\n"

COMMENT: "//" ("\n"^)*

KEYWORDS: `if | `do | `while | `select | `delete | `update | `exec | `from | `by | `where | `and | `or | `map | `mapl | `mapr | `fold | `mfold

KEYRULES: BIN MINUS

MINUS: WS0? "-"

Q .ll.eval "BIN: "," | "sv  {"\"",/:x,\:"\""}string{v where 102=(type value string@)each v:.Q.res except `if`do`while`select`exec`delete`update}[],where {$[102=type x;1b;100=type x;2=count value[x]1;0b]}each .q;  / define dynamically

ANDU: AND | "_"
AND: A | N | "."
A: "a"-"z" | "A"-"Z"
N: "0"-"9"

LEXER lex

TOKENS: LEXER

Q 0N!"parser"

expr: expr ("[" {x:`} exprLst? "]" 2)                                                 {: $[count v2;{x,$[count y;"[",y,"]";"[]"]}/["(",v1,")";v2];v1]}
    | {not`wh~x}? expr expr                                                           {: $[count v2;v1," "," "sv v2;v1]}
    | expr BIN expr                                                                   {: .ql.ejoin[v1;v2]}
    | expr ("#" | "$" | "^" | "?" | "_") expr                                         {: .ql.ejoin[v1;v2]}
    | {not`col~x}? expr "," expr                                                      {: .ql.ejoin[v1;v2]}
    | expr "!" expr                                                                   {: .ql.ejoin[v1;v2]}
    | expr ("*" | "/" {:"%"} | "%") expr                                              {: .ql.ejoin[v1;v2]}
    | expr ("+" | "-") expr                                                           {: .ql.ejoin[v1;v2]}
    | expr ("=" | "<>" | "<=" | ">=" | "<" | ">" | "~") expr                          {: .ql.ejoin[v1;v2]}
    | expr ("&" | "and" {:"and "}) expr                                               {: .ql.ejoin[v1;v2]}
    | expr ("|" | "or" {:"or "}) expr                                                 {: .ql.ejoin[v1;v2]}
    | {assoc:right} expr ("@" | ".") expr                                             {: $[count v2;"(",v1,")",v2[0]," ",v2 1;v1]}
    | {assoc:right} expr (ASSIGN|COLON) expr                                          {: $[count v2;"(",v1,")",v2[0],v2 1;v1]}
    | {x:`} expr0


expr0: "(" ({.ll.match "["}! "[" namedExprLst? "]" 2)? exprLst? ")"                   {: "(",$[()~v2;();"[",v2,"]"],v3,")"}
     | (SYM | STR | NAME)
     | ({1b}? NUM)+                                                                   {: " "sv v1}
     | "{" ({.ll.match "["}! "[" (NAME (";" NAME 2)* {:";"sv enlist[v1],v2})? "]" {:"[",v2,"]"})? exprLst? "}"  {: "{",v2,v3,"}"}
     | ("do"|"while"|"if") "[" stmExprLst "]"                                         {: v1,"[",v3,"]"}
     | "$" "[" statement (";" statement ";" statement {: v1,";",v2})+ "]"             {: "$[",v3,";",(";"sv v4),"]"}
     | ("map" {: "'"} | "fold" {: "/"} | "mfold" {: "\\"} | "mapl" {: "\\:"} | "mapr" {: "/:"}) "[" statement ("; exprLst" 2)? "]"  {: "((",v3,")",v1,")",$[count v4;"[",v4,"]";()]}
     | ("(" "'" ")" "[" exprLst"]" 5 | "(" "'" "[" exprLst "]" ")" 4)                 {: "(')[",v1,"]"}
     | ("select" | "delete" | "update" | "exec")                                      {.ll.p_i-:1; : .ql.sql x}
     | "where"

sql: "select" sqlLst? ("by" sqlLst 2)? "from" sqlStm ("where" sqlLst 2)?                 {: "select ",v2,$[count v3;" by ",v3;()]," from ",v5,$[count v6;" where ",v6;()]}
   | "exec" sqlLst? ("by" sqlLst 2)? "from" sqlStm ("where" sqlLst 2)?                   {: "exec ",v2,$[count v3;" by ",v3;()]," from ",v5,$[count v6;" where ",v6;()]}
   | "update" sqlLst? ("by" sqlLst 2)? "from" sqlStm ("where" sqlLst 2)?                 {: "update ",v2,$[count v3;" by ",v3;()]," from ",v5,$[count v6;" where ",v6;()]}
   | "delete" (NAME ("," NAME 2)* {: ","sv enlist[v1],v2})? "from" sqlStm ("where" sqlLst)? {: "delete ",v2," from ",v4,$[count v5;" where ",v5;()]}

sqlLst: {x:`col} statement ("," statement 2)*                                                  {: ","sv enlist[v1],v2}
sqlStm: {x:`wh} statement

namedExpr: NAME COLON statement                                                       {: v1,":",v3}
namedExprLst: namedExpr (";" namedExpr 2)*                                            {: ";"sv enlist[v1],v2}

stmExprLst: statement (";" statement 2 | ";" {:enlist"::"})*  {: ";"sv enlist[v1],v2}
          | (";" statement 2 | ";" {:enlist"::"})+            {: ";"sv enlist["::"],v1}

exprLst: statement (";" statement 2 | ";" {:""})*  {: ";"sv enlist[v1],v2}
       | (";" statement 2 | ";" {:""})+            {: ";"sv enlist[""],v1}

statement: "[" stmExprLst? "]"  expr? {: "[",v2,"]",v4}
         | expr
         | "'" expr
         | ("+" | "-" | "*" | "/" {:(),"%"} | "~" | "!" | "@" | "#" | "$" | "^" | "&" | "=" | "<" | ">" | "?" | "." | "|" | "_" | "<>" | "<=" | ">=" | ",")
         | BIN

PARSER stmExprLst

Q .ql.ejoin:{$[count y;{"(",x,")",y[0]," ",y 1}/[x;y];x]}
Q .ql.sql:.ll.getPF`Q.sql
