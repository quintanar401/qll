/ ANTLR Python grammar

GNAME PY

top: STRING | NUMBER | KEYWORDS | NAME | WS | COMMENT | NLWS | "." | "..." | "*" | "," | ";" | "**" | "=" | "|" | "^" | "&" | "<<" | ">>" | "+" | "-" | "/" | "%" | "//" | "~"
   | "<" | ">" | "==" | "<=" | ">=" | "<>" | "!=" | "@" | "->" | "+=" | "*=" | "-=" | "/=" | "@=" | "%=" | "|=" | "&=" | "^=" | "<<=" | ">>=" | "**=" | "//=" | ":"

KEYWORDS: `def | `return | `raise | `from | `import | `as | `global | `nonlocal | `assert | `if | `elsif | `else | `while | `for | `in | `try | `finally | `with
        | `except | `lambda | `or | `and | `not | `is | `None | `True | `False | `class | `yield | `del | `pass | `continue | `break | `async | `await

/ for INDENT/DEDENT
KEYRULES: POPEN PCLOSE BOPEN BCLOSE COPEN CCLOSE
POPEN: "("
PCLOSE: ")"
BOPEN: "["
BCLOSE: "]"
COPEN: "{"
CCLOSE: "}"

NAME: ID_START ID_CONTINUE*
ID_START: "_" | "a"-"z" | "A"-"Z"
ID_CONTINUE: ID_START | DIGIT

STRING: STRING_LITERAL | BYTES_LITERAL
STRING_LITERAL: ("u" | "U" | "R" | "r" | "f" | "F" | "fr" | "Fr" | "fR" | "FR" | "rf" | "rF" | "Rf" | "RF")? ( SHORT_STRING | LONG_STRING )
SHORT_STRING: "'" ( STRING_ESCAPE_SEQ | "\\\r\n'"^ )* "'" | "\"" ( STRING_ESCAPE_SEQ | "\\\r\n\""^ )* "\""
LONG_STRING: "'''" LONG_STRING_ITEM1* "'''" | "\"\"\"" LONG_STRING_ITEM2* "\"\"\""
LONG_STRING_ITEM1: LONG_STRING_ITEM11 | "'" LONG_STRING_ITEM11 | "''" LONG_STRING_ITEM11
LONG_STRING_ITEM11: STRING_ESCAPE_SEQ | "\\'"^
LONG_STRING_ITEM2: LONG_STRING_ITEM22 | "\"" LONG_STRING_ITEM22 | "\"\"" LONG_STRING_ITEM22
LONG_STRING_ITEM22: STRING_ESCAPE_SEQ | "\\\""^
STRING_ESCAPE_SEQ: "\\" 0i^

/ define bytes as a string as of now
BYTES_LITERAL: ("b" | "B" | "rb" | "Rb" | "rB" | "RB" | "br" | "Br" | "bR" | "BR") ( SHORT_BYTES | LONG_BYTES )
SHORT_BYTES: SHORT_STRING
LONG_BYTES: LONG_STRING

NUMBER: INTEGER | FLOAT_NUMBER | IMAG_NUMBER
INTEGER: DECIMAL_INTEGER | OCT_INTEGER | HEX_INTEGER | BIN_INTEGER
DECIMAL_INTEGER: NON_ZERO_DIGIT DIGIT* | "0"+
OCT_INTEGER: "0" ("o"|"O") OCT_DIGIT+
HEX_INTEGER: "0" ("x"|"X") HEX_DIGIT+
BIN_INTEGER: "0" ("b"|"B") BIN_DIGIT+
FLOAT_NUMBER: POINT_FLOAT | EXPONENT_FLOAT
IMAG_NUMBER: ( FLOAT_NUMBER | INT_PART ) ("j"|"J")
NON_ZERO_DIGIT: "1"-"9"
DIGIT: "0"-"9"
OCT_DIGIT: "0"-"7"
HEX_DIGIT: DIGIT | "a"-"f" | "A"-"F"
BIN_DIGIT: "0" | "1"
POINT_FLOAT: INT_PART? FRACTION | INT_PART "."
EXPONENT_FLOAT: ( INT_PART | POINT_FLOAT ) EXPONENT
INT_PART: DIGIT+
FRACTION: "." DIGIT+
EXPONENT: ("e"|"E") ("+"|"-")? DIGIT+

WS: (" "|"\t")+ | "\\" (" \t")* ("\r"? "\n" | "\r")
COMMENT: "#" ("\r\n"^)*
NLWS: ("\r"? "\n" | "\r") (" "|"\t")*

/ Q .ll.mkU[1;"0080-FFFF"];  / map all utf8 into 1 class atm

/ Insert INDENT/DEDENT/NEWLINE + add dedent count
Q .py.pmap:`PY.POPEN`PY.PCLOSE`PY.BOPEN`PY.BCLOSE`PY.COPEN`PY.CCLOSE!1 -1 1 -1 1 -1
Q .ll.lexer:.py.lexer:{v:(.ll.lexer0 x),'(enlist(),"\n";`PY.NLWS;count x); v:v@\:where not(v[1]in`PY.WS`PY.COMMENT)|((sums 0^.py.pmap v 1)|(next v[1]in`PY.COMMENT`PY.NLWS))&`PY.NLWS=v 1;
  if[0W=first last d:{$[y=l:last x;x;y>l;x,y;$[count[x]=i:x?y;(),0W;(i+1)#x]]}\[(),0;sum each(til[9]!0 1 1 1 1 4 3 2 1)(0 1 5;0 2 6;0 3 7;0 4 8;0 1 5;0 1 5;0 1 5;0 1 5;0 1 5)\[0;]each("\r\n \t"!0 0 1 2)v[0] i:where v[1] in `PY.NLWS];
  '"Indentation error at ",string first .ll.getPos[v,enlist x;i first where 0W=first each d]]; :.[v;(1;i);:;(-1 0 1!`PY.DEDENT`PY.NEWLINE`PY.INDENT) -1|dx],(x;@[(count v 1)#0;i;:;dx:deltas -1+count each d])}

LEXER top

TOKENS: LEXER INDENT DEDENT NEWLINE

file_input: (NEWLINE | stmt)*;

decorator: "@" dotted_name ( "(" (arglist)? ")" )? NEWLINE;
decorators: decorator+;
decorated: decorators (classdef | funcdef | async_funcdef);

async_funcdef: "async" funcdef;
funcdef: "def" NAME parameters ("->" test)? ":" suite;

parameters: "(" (typedargslist)? ")";
typedargslist: (tfpdef ("=" test)? ("," tfpdef ("=" test)?)* ("," (
        "*" (tfpdef)? ("," tfpdef ("=" test)?)* ("," ("**" tfpdef (",")?)?)?
      | "**" tfpdef (",")?)?)?
  | "*" (tfpdef)? ("," tfpdef ("=" test)?)* ("," ("**" tfpdef (",")?)?)?
  | "**" tfpdef (",")?);
tfpdef: NAME (":" test)?;
varargslist: (vfpdef ("=" test)? ("," vfpdef ("=" test)?)* ("," (
        "*" (vfpdef)? ("," vfpdef ("=" test)?)* ("," ("**" vfpdef (",")?)?)?
      | "**" vfpdef (",")?)?)?
  | "*" (vfpdef)? ("," vfpdef ("=" test)?)* ("," ("**" vfpdef (",")?)?)?
  | "**" vfpdef (",")?);
vfpdef: NAME;

stmt: simple_stmt | compound_stmt;
simple_stmt: small_stmt (";" small_stmt)* (";")? (NEWLINE | DEDENT {.ll.p_i-:1});  / roll back dedent
small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |  import_stmt | global_stmt | nonlocal_stmt | assert_stmt);
expr_stmt: testlist_star_expr (annassign | augassign (yield_expr|testlist) | ("=" (yield_expr|testlist_star_expr))*);
annassign: ":" test ("=" test)?;
testlist_star_expr: (test|star_expr) ("," (test|star_expr))* (",")?;
augassign: ("+=" | "-=" | "*=" | "@=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "**=" | "//=");

// For normal and annotated assignments, additional restrictions enforced by the interpreter
del_stmt: "del" exprlist;
pass_stmt: "pass";
flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt;
break_stmt: "break";
continue_stmt: "continue";
return_stmt: "return" (testlist)?;
yield_stmt: yield_expr;
raise_stmt: "raise" (test ("from" test)?)?;
import_stmt: import_name | import_from;
import_name: "import" dotted_as_names;
// note below: the ("." | "...") is necessary because "..." is tokenized as ELLIPSIS
import_from: "from" (("." | "...")* dotted_name | ("." | "...")+)  "import" ("*" | "(" import_as_names ")" | import_as_names);
import_as_name: NAME ("as" NAME)?;
dotted_as_name: dotted_name ("as" NAME)?;
import_as_names: import_as_name ("," import_as_name)* (",")?;
dotted_as_names: dotted_as_name ("," dotted_as_name)*;
dotted_name: NAME ("." NAME)*;
global_stmt: "global" NAME ("," NAME)*;
nonlocal_stmt: "nonlocal" NAME ("," NAME)*;
assert_stmt: "assert" test ("," test)?;

compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated | async_stmt;
async_stmt: "async" (funcdef | with_stmt | for_stmt);
if_stmt: "if" test ":" suite ("elif" test ":" suite)* ("else" ":" suite)?;
while_stmt: "while" test ":" suite ("else" ":" suite)?;
for_stmt: "for" exprlist "in" testlist ":" suite ("else" ":" suite)?;
try_stmt: ("try" ":" suite
           ((except_clause ":" suite)+
            ("else" ":" suite)?
            ("finally" ":" suite)? |
           "finally" ":" suite));
with_stmt: "with" with_item ("," with_item)*  ":" suite;
with_item: test ("as" expr)?;
// NB compile.c makes sure that the default except clause is last
except_clause: "except" (test ("as" NAME)?)?;
suite: simple_stmt | INDENT stmt+ DEDENT {if[0>.ll.p_t[4;.ll.p_i-1]+:1; .ll.p_i-:1]}

test: or_test ("if" or_test "else" test)? | lambdef;
test_nocond: or_test | lambdef_nocond;
lambdef: "lambda" (varargslist)? ":" test;
lambdef_nocond: "lambda" (varargslist)? ":" test_nocond;
or_test: and_test ("or" and_test)*;
and_test: not_test ("and" not_test)*;
not_test: "not" not_test | comparison;
comparison: expr (comp_op expr)*;

// <> isn't actually a valid comparison operator in Python. It's here for the
// sake of a __future__ import described in PEP 401 (which really works :-)
comp_op: "<"|">"|"=="|">="|"<="|"<>"|"!="|"in"|"not" "in"|"is"|"is" "not";
star_expr: "*" expr;
expr: xor_expr ("|" xor_expr)*;
xor_expr: and_expr ("^" and_expr)*;
and_expr: shift_expr ("&" shift_expr)*;
shift_expr: arith_expr (("<<"|">>") arith_expr)*;
arith_expr: term (("+"|"-") term)*;
term: factor (("*"|"@"|"/"|"%"|"//") factor)*;
factor: ("+"|"-"|"~") factor | power;
power: atom_expr ("**" factor)?;
atom_expr: "await"? atom trailer*;
atom: ("(" (yield_expr|testlist_comp)? ")" |
       "[" (testlist_comp)? "]" |
       "{" (dictorsetmaker)? "}" |
       NAME | NUMBER | STRING+ | "..." | "None" | "True" | "False");
testlist_comp: (test|star_expr) ( comp_for | ("," (test|star_expr))* (",")? );
trailer: "(" (arglist)? ")" | "[" subscriptlist "]" | "." NAME;
subscriptlist: subscript ("," subscript)* (",")?;
subscript: test | (test)? ":" (test)? (sliceop)?;
sliceop: ":" (test)?;
exprlist: (expr|star_expr) ("," (expr|star_expr))* (",")?;
testlist: test ("," test)* (",")?;
dictorsetmaker: ( ((test ":" test | "**" expr)
                   (comp_for | ("," (test ":" test | "**" expr))* (",")?)) |
                  ((test | star_expr)
                  (comp_for | ("," (test | star_expr))* (",")?)) );

classdef: "class" NAME ("(" (arglist)? ")")? ":" suite;

arglist: argument ("," argument)* (",")?;

// The reason that keywords are test nodes instead of NAME is that using NAME
// results in an ambiguity. ast.c makes sure it's a NAME.
// "test '=' test" is really "keyword '=' test", but we have no such token.
// These need to be in a single rule to avoid grammar that is ambiguous
// to our LL(1) parser. Even though 'test' includes '*expr' in star_expr,
// we explicitly match '*' here, too, to give it proper precedence.
// Illegal combinations and orderings are blocked in ast.c:
// multiple (test comp_for) arguments are blocked; keyword unpackings
// that precede iterable unpackings are blocked; etc.
argument: ( test (comp_for)? |
            test "=" test |
            "**" test | "*" test );

comp_iter: comp_for | comp_if;
comp_for: ("async")? "for" exprlist "in" or_test (comp_iter)?;
comp_if: "if" test_nocond (comp_iter)?;

// not used in grammar, but may appear in "node" passed from Parser to Compiler
encoding_decl: NAME;

yield_expr: "yield" (yield_arg)?;
yield_arg: "from" test | testlist;

PARSER file_input
