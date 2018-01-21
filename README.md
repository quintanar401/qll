## Parser

### Info

The parser is based on ANTL parsers V3 and V4. Like ANTLR it uses LL(\*) top-down strategy and creates DFAs(deterministic finite automata) to predict productions. Unlike ANTLR4 it precalculates as
much DFA states as possible to reduce calculations at runtime thus it can report some grammar issues beforehand - ambiguity, excessive recursion and etc. Like ANTLR the parser can augment the rule's
DFAs during parsing but unlike ANTLR4 it will never fallback to the full LL mode when a context sensitive ambiguity is detected - instead it will immediately choose the first available option (if no
predicate is specified), this is done because full LL mode would be extreamly inefficient in Q and if you need it you probably should use ANTLR in the first place.

This parser has an improvement on ANTLR - it eliminates the rule's tail recursion during DFA construction and can produce more powerful DFAs (for example 'not\* a | not\* b' rule will not cause the recursion
overflow error). Otherwise the Q algorithm closely mirrors the ANTLR3/4 algorithm that can be found in their papers.

Though the parser can be used as a lexer there is a special mode to create very efficient lexers based on a state matrix. Basically the difference between the parser and lexer is that for the lexer I take the first rule
and update its DFA until it stops to grow when for the parser DFA construction stops when it starts to predict at most 1 production on every input. Lexer's speed doesn't depend on the grammar complexity.

### Usage

```
\l qll.q
\l grammar.g / or
.ll.eval each ("rule1: ...";"rule2:...";..); .ll.lgen[] or .ll.gen[] / gen - parser, lgen - lexer
.ll.parse "1+2"
.ll.lexer "1+2"
```

### Rule format

Parser rule's format:
```
TOKENS: LEXER NAME1 NAME2 ...          / list of tokens. If LEXER is present tokens from the lexer will be added + all string consts corresponding to lexer tokens will be assigned the token name ("if"->IF) - for ANTLR compatibility.
NAME name                              / optional name for the parser/lexer
PARSER rule                            / generate a parser, start with the rule
LEXER rule                             / generate a lexer
Q qexpr                                / optional q code for actions

/ comment
rulename: "term" nonterm `term ...     / direct rule
rulename: ... | .... | ...             / rule with options
rulename: (a|b) c                      / rule with a subexpression
rulename: aaaa
        | bbbb ;                       / rule can occupy several indented lines and end with an optional ;
rulename: {calc} a {calc} | c {calc}   / actions can appear anywhere within the rule, they will be executed exactly in this order after/before subparse calls.
rulename: {pred}? a | {pred}! b        / predicates must be the first expression

xxxx?                                  / repeat 1 or 0 times
xxxx*                                  / repeat 0 or more times
xxxx+                                  / repeat 1 or more times
xxxx^                                  / anything but xxxx where xxxx must be a term or a reference to a list of terms, EOF is not included in anything
"x"-"y"                                / only for char terms, range

{:v1+1}                                / action to be included into the rule parse function
123                                    / shorthand for {:(v1;v2;v3)} action
0i                                     / EOF term
{...}?                                 / weak predicate
{...}!                                 / strong predicate
```

### Predicates

Predicates can be used to resolve ambiguities. They must accept 1 arg (x in actions) and return 1b or 0b. They must be the first expression of a subrule. They can be applied to simple rules too but they will never be called for them.

Weak predicates (with ?) will be executed only if there is an ambiguity. Strong predicates (with !) will be executed on each prediction, if they return 0b the next strong predictate will be executed or the rule's DFA prediction fn will be called.

By default all weak predicated expressions like ({..}? xxxx)+ or ? or * will be modified to be non-ambiguous. If the predicate is true then the main rule will be invoked otherwise the empty alternative. To make them gready use this `({1b} ....)*`

### Actions

Actions can be used to modify parse trees. They can appear anywhere in a rule after predicates. They will be inserted as is into the rule's parse function. Example:
```
a: Func "(" args ")" {:(v1;v3)} / rule
{[x] v1:Func[predict alt][x]; v2:match "("; v3:args[predict][x]; v4:match ")"; :(v1;v3); :(v1;v2;v3;v4)} / rule parse fn
```
Actions can use variables like v1, v2 and etc with parsed subexpressions. Do not forget to add ":" (return) in the final actions. Also they can use or modify x - it is passed to all subexpressions.

Action shortcut - a number - can be used to select some subexpressions:
```
a: Func "(" args ")" 13 / ~ {:(v1;v3)}
```

### API

* .ll.parse "string" - main parse function, it starts from the top rule and consumes all input including EOF.
* .ll.getPF \`rule  - get the parse fn for a rule. If you want to parse only part of the input you may have to make this rule open (just add an alias for it that it not referenced in any other rule).
* .ll.e - it is called for grammar errors, feel free to modify.
* .ll.ferr - error fn for parse errors.
* .ll.eval - define a rule/setting manually (Q .ll.eval "rule: ..." for example).

Note that the parser is just a set of functions. You can substitute any of them and/or add your functions that call parse funcs.

Settings:
* .ll.depth:1 - max recursion depth for DFA construction.
* .ll.autoraze:1b - autoflat the results of X\* and X+ rules.
* .ll.lr:1b - autofix left recursion.
* .ll.statelim:50 - limit max static state num for DFAs

### Left recursion elimination

It is possible to use rules like this:
```
Q .ll.lr:1b; / by default
expression: expression "++" | "++" expression | expresion "*" expression | expresion "+" expression | NUMBER
```
The subrules will be renamed and reodered to eliminate the left recursion and ambiguity. You must ensure:
* All subrules with the left or right recursion must be listed explicitly (end or start with the rule name) in one rule
* Subrules must appear in the desc order of priority (add at the end, mult at the start and etc). Non recursive subrules can appear anywhere but there must be at least 1 such rule.
* All operations with the same priority must be put in one subrule.
* For actions keep in mind that the final rules will look like `expression+2 ("+" expression+3)*` or `expr+2 ("=" expr+2)?` (left and right assoc). I'd recommend to inspect .ll.R to ensure that they are ok.
* Predicates will be moved into subrules like `{prd}? expr + expr ` -> `expr ({prd}? + expr)*`

It is possible to mark a subrule as right recursive:
```
 | {assoc:right} expr ASSIGN expr
```

### Speed

Lexer is quite fast. For Java it processes 27Mb chars per sec on my machine. Parser is much slower (Q is ill suited for parsing) but still is good enough - ~600Kb per sec for Java on my machine (10-15x slower than ANTLR).

### Errors

During parser construction the following errors can be reported:
* recursion overflow on ... - minor error, the rule's recursion prevents DFA completion.
* at least two recursive paths on ... - more serious error, it can be caused by an ambiguity. It indicates that two or more subrules have a recursion that prevents effective prediction. This may not be important in the real world (because there is no infinite recursion) but you have to check this, especially because unlimited lookahead might be required to resolve such rules. Java's anotations are a good example.
* grammar ambiguity in ... - very serious error. It will certainly cause runtime errors. Either your grammar is incorrect or you need to add predicates to explicitly resolve its issues.
* state limit: ... - this one indicates that DFA is too big to continue. Its purpose is to stop DFA construction on rules like expression which DFAs can have tens of thousands of states.

During parsing the following error can appear except input errors:
* Multiple alternatives in  ... - indicates that there is a grammar ambiguity.

## Lexer

The lexer is created as a one big DFA thus the grammar should not contain ambiguities (use KEYRULES and KEYWORDS to bypass this, they can conflict with identifiers for example).

The main lexer rule must be a list of all possible tokens and raw strings like:
```
top: ID | NUMBER | "!=" | "{"
...
LEXER top
```
The lexer will produce a list of 4 lists: raw strings, token names (taken from the top rule names, string consts will have no name), index into src, src itself. The lexer will call `.ll.lexmap` function, it can be redefined to postprocess the result. `.ll.getPos` can be used to calculate the line and column of a token by its src idx.

The lexer ignores predicates and actions, all other parser directives are valid. The most useful are:
```
"a"-"z"   / range
"a"^ | ("a"-"z")^
```

Special rule names:
* KEYWORDS - keywords can be listed under this rule, the lexer will ignore ambiguities if they are caused by these keywords. Keywords will not have a token name. If you need their names list them with KEYRULES setting.
* WS and COMMENT - terms with these types will be deleted from the result (use other names if they are needed).
* ERROR - if a string doesn't end with a valid final state it will be marked as ERROR and an exception will be raised.

KEYRULES setting can be used instead of KEYWORDS - all listed rules will be added to the main rule + any conflict in their final states will be ignored. You must ensure that these rules are correct (only consts). A large KEYRULES list will slowdown the lexer generation, it may be better to map names in the postprocessing function from the general IDENTIFIER rule.
```
KEYRULES: DELETE SELECT UPDATE
DELETE: "delete"
...
```

Utf8 and non ASCII chars: one byte non ASCII chars can be used as is - if you have utf8 only in strings this may be enough (all utf8 non ASCII bytes are > 127). If not you'll have to define a map `.ll.U` from unicode values into
char classes selected from the first 256 chars. Each utf8 sequence will be transformed into a sequence that starts with 128 and ends with its unicode value and then all input will be mapped into the first 256 chars using `.ll.U`.
Values in the utf8 sequences will always be greater than 127 and if you map them in classes a b c then your lexer rules should expect `"\200" (a|b|c)* a` for `a` unicode chars and etc. Usually the unicode chars fall into two groups -
identifiers and all other so the rules should be easy enough. Use `.ll.mkU[class - 128 for example;"AAAA-BBBB"]` to add a range to `.ll.U`.

If you are not satisfied with how fast the lexer is generated just save .ll.L into a binary file or use `.ll.lsave fname` to save it in Q format. The generated lexer is just a matrix with a map from the final states to token names.

The lexer will cleanup all rules and etc (except the grammar name) to not interfere with the parser.

See calc.g and java.g as an example.

## Pretty print

Run this to get all rules, states, transitions in txt form:
```
`:lang.txt 0: .ll.pp[]
```

On error run:
```
.ll.pppath[`$"Rule name";state]
```
To see all possible (without recursion) paths from the initial state into `state`. The path will not include the term that caused the error.

## ANTLR grammars

Basically they are compatible but there are some issues best illustrated with java:
* Lexer needs to be reworked completely though changes are simple and straightforward.
* All \' strings must be changed to " strings.
* All additional syntax like lables (name=) must be removed.
* Predicates should be inserted to resolve conflicts - java is extreamly irregular.
* Java also had implicit and wrongly ordered postfix/prefix rules in `expression` that relied on the left rec elimination. They must be explicit and all postfix rules with the same priority must be on the same level.
