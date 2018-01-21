TOKENS: STR NUM NAME
GNAME SQL

start: sel | cre | ins | upd | del | drp
sel: `select `distinct? selA (`into name)? (`from join)? (`where expr)? (`group `by exprLst)? (`having expr)? (`order `by expr (`asc|`desc)?)? (`limit expr)? | fnCall
cre: `create `table name "(" decl ("," decl)* ")"
ins: `insert `into name exprLstP? (`values exprLstPLst | sel)
upd: `update name `set exprLst `where expr
del: `delete `from name `where expr
drp: `drop `table name

selA: "*" | namedExpr ("," namedExpr )*
expr: expr4 (`or expr)?
expr4: `nott expr4 | expr3 (`and expr4)? 
expr3: expr2 (("="|"<>"|"!="|">"|"<"|">="|"<=") expr3)? | `is `not `null
expr2: expr1 ((`not? (`in|`like)|"+"|"-") expr2)?
expr1: expr0 (("*"|"/"|"div") expr1)?
expr0: "(" expr ")" | (`not|"-") expr0 | fnCall | fnCall1 | fnCall2 | atom | case | extcast | "{" "fn" expr "}" | "{" name name "}"
namedExpr: expr (`as? name)?
exprLst: "*" | expr ("," expr)?
exprLstP: "(" exprLst ")"
exprLstPLst: exprLstP ("," exprLstP)*

fnCall: (name | `insert | `left) "(" exprLst? ")"
fnCall1: (`convert | `timestampadd | `timestampdiff) "(" exprLst ")"
fnCall2: (`sum | `avg | `count | `min | `max) "(" `distinct? expr ")" | `count "(" "*" ")"
atom: name | `null | STR | NUM
name: NAME
case: `case expr? (`when expr `then expr)+ `else expr `end
extcast: `extract "(" name `from expr ")" | `cast "(" expr `as type ")"
type: name ("(" exprLst? ")")?
join: tbl (((`left | `inner | `cross)? `join | ",") tbl (`on expr)?)* | "(" join ")"
tbl: (fnCall | name | "(" sel ")") (`as? name)?
decl: name type

GENERATE start