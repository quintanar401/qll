/ Java ANTLR grammar
/ It will report some errors, they can be ignored
/ Usage:
/ .ll.parse "c"$read1 `:java7.java

GNAME Java

lexer:  DECIMAL_LITERAL | HEX_LITERAL | OCT_LITERAL | BINARY_LITERAL | FLOAT_LITERAL | HEX_FLOAT_LITERAL | CHAR_LITERAL | STRING_LITERAL | WS | COMMENT | URSHIFT | LSHIFT | RSHIFT
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACK | RBRACK | SEMI | COMMA | DOT | ASSIGN | GT | LT | BANG | TILDE | QUESTION | COLON | EQUAL | LE | GE | NOTEQUAL | AND | OR | INC | DEC | ADD | SUB
  | MUL | DIV | BITAND | BITOR | CARET | MOD | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | AND_ASSIGN | OR_ASSIGN | XOR_ASSIGN | MOD_ASSIGN | LSHIFT_ASSIGN | RSHIFT_ASSIGN | URSHIFT_ASSIGN
  | ARROW | COLONCOLON | AT | ELLIPSIS | IDENTIFIER

DECIMAL_LITERAL: ("0" | ("1"-"9") (Digits? | "_"+ Digits)) ("l"|"L")?;
HEX_LITERAL: "0" ("x"|"X") NH ((NH|"_")* NH)? ("l"|"L")?;
OCT_LITERAL: "0" "_"* O ((O|"_")* O)? ("l"|"L")?;
BINARY_LITERAL: "0" ("b"|"B") ("0"|"1") (("0"|"1"|"_")* ("0"|"1"))? ("l"|"L")?;
FLOAT_LITERAL: (Digits "." Digits? | "." Digits) ExponentPart? ("f"|"F"|"d"|"D")?
    | Digits (ExponentPart ("f"|"F"|"d"|"D")? | ("f"|"F"|"d"|"D"))
HEX_FLOAT_LITERAL: "0" ("x"|"X") (HexDigits "."? | HexDigits? "." HexDigits) ("p"|"P") ("+"|"-")? Digits ("f"|"F"|"d"|"D")?;
BOOL_LITERAL: "true" | "false"
CHAR_LITERAL: "'" ("'\\\r\n"^  | EscapeSequence) "'";
STRING_LITERAL: "\"" ("\"\\\r\n"^  | EscapeSequence)* "\"";
NULL_LITERAL:  "null";

LPAREN:             "(";
RPAREN:             ")";
LBRACE:             "{";
RBRACE:             "}";
LBRACK:             "[";
RBRACK:             "]";
SEMI:               ";";
COMMA:              ",";
DOT:                ".";

ASSIGN:             "=";
GT:                 ">";
LT:                 "<";
BANG:               "!";
TILDE:              "~";
QUESTION:           "?";
COLON:              ":";
EQUAL:              "==";
LE:                 "<=";
GE:                 ">=";
NOTEQUAL:           "!=";
AND:                "&&";
OR:                 "||";
INC:                "++";
DEC:                "--";
ADD:                "+";
SUB:                "-";
MUL:                "*";
DIV:                "/";
BITAND:             "&";
BITOR:              "|";
CARET:              "^";
MOD:                "%";
LSHIFT:             "<<";
RSHIFT:             ">>";
URSHIFT:            ">>>";

ADD_ASSIGN:         "+=";
SUB_ASSIGN:         "-=";
MUL_ASSIGN:         "*=";
DIV_ASSIGN:         "/=";
AND_ASSIGN:         "&=";
OR_ASSIGN:          "|=";
XOR_ASSIGN:         "^=";
MOD_ASSIGN:         "%=";
LSHIFT_ASSIGN:      "<<=";
RSHIFT_ASSIGN:      ">>=";
URSHIFT_ASSIGN:     ">>>=";

ARROW:              "->";
COLONCOLON:         "::";

AT:                 "@";
ELLIPSIS:           "...";

IDENTIFIER: Letter LetterOrDigit*;

WS: (" "|"\t"|"\r"|"\n")+
COMMENT: "/*" COMM2 | "//" (("\r" "\n")^)*
COMM2: ("*"^)* "*" ("/" | "/"^ COMM2)


ExponentPart: ("e"|"E") ("+"|"-")? Digits

EscapeSequence
    : "\\" ("b"|"t"|"n"|"f"|"r"|"\""|"'"|"\\")
    | "\\" (("0"-"3")? O)? O
    | "\\" "u"+ HexDigit HexDigit HexDigit HexDigit

HexDigits: HexDigit ((HexDigit | "_")* HexDigit)?
HexDigit: NH

Digits: N ((N|"_")* N)?

LetterOrDigit: Letter| N
Letter: "a"-"z"|"A"-"Z"|"$"|"_"

NH: N | H
N: "0"-"9"
O: "0"-"7"
H: "a"-"f" | "A"-"F"

KEYRULES: NULL_LITERAL BOOL_LITERAL ABSTRACT ASSERT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTENDS FINAL FINALLY FLOAT FOR IF GOTO IMPLEMENTS IMPORT
   INSTANCEOF INT INTERFACE LONG NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED THIS THROW THROWS TRANSIENT TRY
   VOID VOLATILE WHILE

ABSTRACT:           "abstract";
ASSERT:             "assert";
BOOLEAN:            "boolean";
BREAK:              "break";
BYTE:               "byte";
CASE:               "case";
CATCH:              "catch";
CHAR:               "char";
CLASS:              "class";
CONST:              "const";
CONTINUE:           "continue";
DEFAULT:            "default";
DO:                 "do";
DOUBLE:             "double";
ELSE:               "else";
ENUM:               "enum";
EXTENDS:            "extends";
FINAL:              "final";
FINALLY:            "finally";
FLOAT:              "float";
FOR:                "for";
IF:                 "if";
GOTO:               "goto";
IMPLEMENTS:         "implements";
IMPORT:             "import";
INSTANCEOF:         "instanceof";
INT:                "int";
INTERFACE:          "interface";
LONG:               "long";
NATIVE:             "native";
NEW:                "new";
PACKAGE:            "package";
PRIVATE:            "private";
PROTECTED:          "protected";
PUBLIC:             "public";
RETURN:             "return";
SHORT:              "short";
STATIC:             "static";
STRICTFP:           "strictfp";
SUPER:              "super";
SWITCH:             "switch";
SYNCHRONIZED:       "synchronized";
THIS:               "this";
THROW:              "throw";
THROWS:             "throws";
TRANSIENT:          "transient";
TRY:                "try";
VOID:               "void";
VOLATILE:           "volatile";
WHILE:              "while";

LEXER lexer

/**********
/* PARSER
/**********

TOKENS: LEXER

compilationUnit
    : packageDeclaration? importDeclaration* typeDeclaration*
    ;

packageDeclaration
    : annotation* PACKAGE qualifiedName ";"
    ;

importDeclaration
    : IMPORT STATIC? qualifiedName ("." "*")? ";"
    ;

typeDeclaration
    : classOrInterfaceModifier*
      (classDeclaration | enumDeclaration | interfaceDeclaration | annotationTypeDeclaration)
    | ";"
    ;

modifier
    : classOrInterfaceModifier
    | NATIVE
    | SYNCHRONIZED
    | TRANSIENT
    | VOLATILE
    ;

classOrInterfaceModifier
    : annotation
    | PUBLIC
    | PROTECTED
    | PRIVATE
    | STATIC
    | ABSTRACT
    | FINAL    // FINAL for class only -- does not apply to interfaces
    | STRICTFP
    ;

variableModifier
    : FINAL
    | annotation
    ;

classDeclaration
    : CLASS IDENTIFIER typeParameters?
      (EXTENDS typeType)?
      (IMPLEMENTS typeList)?
      classBody
    ;

typeParameters
    : "<" typeParameter ("," typeParameter)* ">"
    ;

typeParameter
    : annotation* IDENTIFIER (EXTENDS typeBound)?
    ;

typeBound
    : typeType ("&" typeType)*
    ;

enumDeclaration
    : ENUM IDENTIFIER (IMPLEMENTS typeList)? "{" enumConstants? ","? enumBodyDeclarations? "}"
    ;

enumConstants
    : enumConstant ("," enumConstant)*
    ;

enumConstant
    : annotation* IDENTIFIER arguments? classBody?
    ;

enumBodyDeclarations
    : ";" classBodyDeclaration*
    ;

interfaceDeclaration
    : INTERFACE IDENTIFIER typeParameters? (EXTENDS typeList)? interfaceBody
    ;

classBody
    : "{" classBodyDeclaration* "}"
    ;

interfaceBody
    : "{" interfaceBodyDeclaration* "}"
    ;

classBodyDeclaration
    : ";"
    | STATIC? block
    | ({1b}? modifier)* memberDeclaration
    ;

memberDeclaration
    : methodDeclaration
    | genericMethodDeclaration
    | fieldDeclaration
    | constructorDeclaration
    | genericConstructorDeclaration
    | interfaceDeclaration
    | annotationTypeDeclaration
    | classDeclaration
    | enumDeclaration
    ;

/  We use rule this even for void methods which cannot have [] after parameters.
/   This simplifies grammar and we can consider void to be a type, which
/   renders the [] matching as a context-sensitive issue or a semantic check
/   for invalid return type after parsing.

methodDeclaration
    : typeTypeOrVoid IDENTIFIER formalParameters ("[" "]")*
      (THROWS qualifiedNameList)?
      methodBody
    ;

methodBody
    : block
    | ";"
    ;

typeTypeOrVoid
    : typeType
    | VOID
    ;

genericMethodDeclaration
    : typeParameters methodDeclaration
    ;

genericConstructorDeclaration
    : typeParameters constructorDeclaration
    ;

constructorDeclaration
    : IDENTIFIER formalParameters (THROWS qualifiedNameList)? block
    ;

fieldDeclaration
    : typeType variableDeclarators ";"
    ;

interfaceBodyDeclaration
    : ({1b}? modifier)* interfaceMemberDeclaration
    | ";"
    ;

interfaceMemberDeclaration
    : constDeclaration
    | interfaceMethodDeclaration
    | genericInterfaceMethodDeclaration
    | interfaceDeclaration
    | annotationTypeDeclaration
    | classDeclaration
    | enumDeclaration
    ;

constDeclaration
    : typeType constantDeclarator ("," constantDeclarator)* ";"
    ;

constantDeclarator
    : IDENTIFIER ("[" "]")* "=" variableInitializer
    ;

// see matching of [] comment in methodDeclaratorRest
// methodBody from Java8
interfaceMethodDeclaration
    : ({1b}? interfaceMethodModifier)* (typeTypeOrVoid | typeParameters ({1b}? annotation)* typeTypeOrVoid)
      IDENTIFIER formalParameters ("[" "]")* (THROWS qualifiedNameList)? methodBody
    ;

// Java8
interfaceMethodModifier
    : annotation
    | PUBLIC
    | ABSTRACT
    | DEFAULT
    | STATIC
    | STRICTFP
    ;

genericInterfaceMethodDeclaration
    : typeParameters interfaceMethodDeclaration
    ;

variableDeclarators
    : variableDeclarator ("," variableDeclarator)*
    ;

variableDeclarator
    : variableDeclaratorId ("=" variableInitializer)?
    ;

variableDeclaratorId
    : IDENTIFIER ("[" "]")*
    ;

variableInitializer
    : arrayInitializer
    | expression
    ;

arrayInitializer
    : "{" (variableInitializer ("," variableInitializer)* (",")? )? "}"
    ;

classOrInterfaceType
    : IDENTIFIER typeArguments? ({1b}? "." IDENTIFIER typeArguments?)*
    ;

typeArgument
    : typeType
    | "?" ((EXTENDS | SUPER) typeType)?
    ;

qualifiedNameList
    : qualifiedName ("," qualifiedName)*
    ;

formalParameters
    : "(" formalParameterList? ")"
    ;

formalParameterList
    : formalParameter ("," formalParameter)* ("," lastFormalParameter)?
    | lastFormalParameter
    ;

formalParameter
    : ({1b}? variableModifier)* typeType variableDeclaratorId
    ;

lastFormalParameter
    : ({1b}? variableModifier)* typeType "..." variableDeclaratorId
    ;

qualifiedName
    : IDENTIFIER ("." IDENTIFIER)*
    ;

literal
    : integerLiteral
    | floatLiteral
    | CHAR_LITERAL
    | STRING_LITERAL
    | BOOL_LITERAL
    | NULL_LITERAL
    ;

integerLiteral
    : DECIMAL_LITERAL
    | HEX_LITERAL
    | OCT_LITERAL
    | BINARY_LITERAL
    ;

floatLiteral
    : FLOAT_LITERAL
    | HEX_FLOAT_LITERAL
    ;

// ANNOTATIONS

annotation
    : "@" qualifiedName ("(" ({1b}? elementValuePairs | elementValue )? ")")?
    ;

elementValuePairs
    : elementValuePair ("," elementValuePair)*
    ;

elementValuePair
    : IDENTIFIER "=" elementValue
    ;

elementValue
    : expression
    | annotation
    | elementValueArrayInitializer
    ;

elementValueArrayInitializer
    : "{" (elementValue ("," elementValue)*)? (",")? "}"
    ;

annotationTypeDeclaration
    : "@" INTERFACE IDENTIFIER annotationTypeBody
    ;

annotationTypeBody
    : "{" (annotationTypeElementDeclaration)* "}"
    ;

annotationTypeElementDeclaration
    : ({1b}? modifier)* annotationTypeElementRest
    | ";" // this is not allowed by the grammar, but apparently allowed by the actual compiler
    ;

annotationTypeElementRest
    : typeType annotationMethodOrConstantRest ";"
    | classDeclaration ({1b}? ";")?
    | interfaceDeclaration ({1b}? ";")?
    | enumDeclaration ({1b}? ";")?
    | annotationTypeDeclaration ({1b}? ";")?
    ;

annotationMethodOrConstantRest
    : annotationMethodRest
    | annotationConstantRest
    ;

annotationMethodRest
    : IDENTIFIER "(" ")" defaultValue?
    ;

annotationConstantRest
    : variableDeclarators
    ;

defaultValue
    : DEFAULT elementValue
    ;

// STATEMENTS / BLOCKS

block
    : "{" blockStatement* "}"
    ;

blockStatement
    : localVariableDeclaration ";"
    | statement
    | localTypeDeclaration
    ;

localVariableDeclaration
    : ({1b}? variableModifier)* typeType variableDeclarators
    ;

localTypeDeclaration
    : classOrInterfaceModifier*
      (classDeclaration | interfaceDeclaration)
/ error?   | ";"
    ;

statement
    : block
    | ASSERT expression (":" expression)? ";"
    | IF parExpression statement ({.ll.p_t[1;.ll.p_i]=`Java.ELSE}! ELSE statement)?
    | FOR "(" forControl ")" statement
    | WHILE parExpression statement
    | DO statement WHILE parExpression ";"
    | TRY block (catchClause+ finallyBlock? | finallyBlock)
    | TRY resourceSpecification block catchClause* finallyBlock?
    | SWITCH parExpression "{" switchBlockStatementGroup* switchLabel* "}"
    | SYNCHRONIZED parExpression block
    | RETURN expression? ";"
    | THROW expression ";"
    | BREAK IDENTIFIER? ";"
    | CONTINUE IDENTIFIER? ";"
    | SEMI
    | expression ";"
    | IDENTIFIER ":" statement
    ;

catchClause
    : CATCH "(" variableModifier* catchType IDENTIFIER ")" block
    ;

catchType
    : qualifiedName ("|" qualifiedName)*
    ;

finallyBlock
    : FINALLY block
    ;

resourceSpecification
    : "(" resources ";"? ")"
    ;

resources
    : resource (";" resource)*
    ;

resource
    : variableModifier* classOrInterfaceType variableDeclaratorId "=" expression
    ;

/ Matches cases then statements, both of which are mandatory.
/ To handle empty cases at the end, we add switchLabel* to statement.
switchBlockStatementGroup
    : switchLabel+ blockStatement+
    ;

switchLabel
    : CASE (expression | {1b}? IDENTIFIER) ":"
    | DEFAULT ":"
    ;

forControl
    : enhancedForControl
    | forInit? ";" expression? ";" expressionList?
    ;

forInit
    : localVariableDeclaration
    | expressionList
    ;

enhancedForControl
    : ({1b}? variableModifier)* typeType variableDeclaratorId ":" expression
    ;

// EXPRESSIONS

parExpression
    : "(" expression ")"
    ;

expressionList
    : expression ("," expression)*
    ;

expression
    : primary
    | expression ("[" expression "]" | "(" expressionList? ")" | "."
        (IDENTIFIER
        | THIS
        | NEW nonWildcardTypeArguments? innerCreator
        | SUPER superSuffix
        | explicitGenericInvocation
        )
      )
    | NEW creator
    | "(" typeType ")" expression
    | expression ("++" | "--")
    | ("+"|"-"|"++"|"--") expression
    | ("~"|"!") expression
    | expression ("*"|"/"|"%") expression
    | expression ("+"|"-") expression
    | expression ("<<" | ">>>" | ">>") expression
    | expression ("<=" | ">=" | ">" | "<") expression
    | expression INSTANCEOF typeType
    | expression ("==" | "!=") expression
    | expression "&" expression
    | expression "^" expression
    | expression "|" expression
    | expression "&&" expression
    | expression "||" expression
    | expression "?" expression ":" expression
    | expression
      ("=" | "+=" | "-=" | "*=" | "/=" | "&=" | "|=" | "^=" | ">>=" | ">>>=" | "<<=" | "%=")
      expression
/ ambuguity    | lambdaExpression // Java8
   | lambdaParameters "->" expression / explicit prefix operator form
   | lambdaParameters "->" block

    // Java 8 methodReference
    | expression "::" typeArguments? IDENTIFIER
    | typeType "::" (typeArguments? IDENTIFIER | NEW)
    | classType "::" typeArguments? NEW
    ;

// Java8
/ lambdaExpression
/    : lambdaParameters "->" lambdaBody
/    ;

// Java8
lambdaParameters
    : IDENTIFIER
    | "(" formalParameterList? ")"
    | "(" IDENTIFIER ("," IDENTIFIER)* ")"
    ;

// Java8
/ lambdaBody
/    : expression
/    | block
/    ;

primary
    : "(" expression ")"
    | THIS
    | SUPER
    | literal
    | IDENTIFIER
    | typeTypeOrVoid "." CLASS
    | nonWildcardTypeArguments (explicitGenericInvocationSuffix | THIS arguments)
    ;

classType
    : (classOrInterfaceType ".")? annotation* IDENTIFIER typeArguments?
    ;

creator
    : nonWildcardTypeArguments createdName classCreatorRest
    | createdName (arrayCreatorRest | classCreatorRest)
    ;

createdName
    : IDENTIFIER typeArgumentsOrDiamond? ("." IDENTIFIER typeArgumentsOrDiamond?)*
    | primitiveType
    ;

innerCreator
    : IDENTIFIER nonWildcardTypeArgumentsOrDiamond? classCreatorRest
    ;

arrayCreatorRest
    : "[" ("]" ("[" "]")* arrayInitializer | expression "]" ({1b}? "[" expression "]")* ({1b}? "[" "]")*)
    ;

classCreatorRest
    : arguments classBody?
    ;

explicitGenericInvocation
    : nonWildcardTypeArguments explicitGenericInvocationSuffix
    ;

typeArgumentsOrDiamond
    : "<" ">"
    | typeArguments
    ;

nonWildcardTypeArgumentsOrDiamond
    : "<" ">"
    | nonWildcardTypeArguments
    ;

nonWildcardTypeArguments
    : "<" typeList ">"
    ;

typeList
    : typeType ("," typeType)*
    ;

typeType
    : annotation? (classOrInterfaceType | primitiveType) ("[" "]")*
    ;

primitiveType
    : BOOLEAN
    | CHAR
    | BYTE
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    ;

typeArguments
    : "<" typeArgument ("," typeArgument)* ">"
    ;

superSuffix
    : arguments
    | "." IDENTIFIER arguments?
    ;

explicitGenericInvocationSuffix
    : SUPER superSuffix
    | IDENTIFIER arguments
    ;

arguments
    : "(" expressionList? ")"
  ;


PARSER compilationUnit
