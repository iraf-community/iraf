# SPP/Yacc parser for the image calculator task.  The function of the parser is
# to parse statements from the input until end of file is reached.  The inner
# machine of the image calculator is a virtual cpu with vector instructions and
# registers for all SPP datatypes.  Each input statement as it is parsed is
# compiled into a sequence of metacode instructions later used to drive the
# virtual cpu.  Compile time constant expressions are evaluated at compile
# time.  The operand image files are opened as they are encounted in the input.
# Vector registers are allocated as necessary during compilation.  No attempt
# is presently made to reuse vector registers.  All string data, e.g., image
# section names, is stored in a string buffer and accessed by pointer.

%{
include	"imcalc.h"
define	yyparse	imc_parse
%L
include	"imcalc.com"
%}

%token		EQUALS SEMICOLON NEWLINE CONSTANT IDENTIFIER YYEOF
%token		NOT PLUS MINUS STAR SLASH EXPON QUEST COLON
%token		LT GT LE GT EQ NE AND OR

%nonassoc	QUEST
%left		OR
%left 		AND
%nonassoc	EQ NE 
%nonassoc	LT GT LE GE
%left		PLUS MINUS
%left		STAR SLASH
%left		EXPON
%right		UMINUS NOT

%%

command	:	assign eost {
			return (OK)
		    }
	|	YYEOF {
			return (EOF)
		    }
	;

assign	:	image EQUALS opnl expr {
			# Put a line to the output image.
			call imc_store (O_VALC($1), $4, $$)
		    }
	;

eost	:	SEMICOLON
	|	NEWLINE
	;

expr	:	image {
			# Load the next line of an input image.
			call imc_load (O_VALC($1), $$)
		    }
	|	CONSTANT {
			# Numeric constant.
			call imc_putconst ($1, $$)
		    }
	|	MINUS expr %prec UMINUS {
			# Unary arithmetic minus.
			call imc_unop (OP_NEG, $2, $$)
		    }
	|	NOT expr {
			# Boolean not.
			call imc_unop (OP_NOT, $2, $$)
		    }
	|	expr PLUS opnl expr {
			# Addition.
			call imc_binop (OP_ADD, $1, $4, $$)
		    }
	|	expr MINUS opnl expr {
			# Subtraction.
			call imc_binop (OP_SUB, $1, $4, $$)
		    }
	| 	expr STAR opnl expr {
			# Multiplication.
			call imc_binop (OP_MUL, $1, $4, $$)
		    }
	|	expr SLASH opnl expr {
			# Division.
			call imc_binop (OP_DIV, $1, $4, $$)
		    }
	|	expr EXPON opnl expr {
			# Exponentiation.
			call imc_binop (OP_POW, $1, $4, $$)
		    }
	|	expr AND opnl expr {
			# Boolean and.
			call imc_boolop (OP_AND, $1, $4, $$)
		    }
	|	expr OR opnl expr {
			# Boolean or.
			call imc_boolop (OP_OR, $1, $4, $$)
		    }
	|	expr LT opnl expr {
			# Boolean less than.
			call imc_boolop (OP_LT, $1, $4, $$)
		    }
	|	expr GT opnl expr {
			# Boolean greater than.
			call imc_boolop (OP_GT, $1, $4, $$)
		    }
	|	expr LE opnl expr {
			# Boolean less than or equal.
			call imc_boolop (OP_LE, $1, $4, $$)
		    }
	|	expr GE opnl expr {
			# Boolean greater than or equal.
			call imc_boolop (OP_GE, $1, $4, $$)
		    }
	|	expr EQ opnl expr {
			# Boolean equal.
			call imc_boolop (OP_EQ, $1, $4, $$)
		    }
	|	expr NE opnl expr {
			# Boolean not equal.
			call imc_boolop (OP_NE, $1, $4, $$)
		    }
	|	expr QUEST opnl expr COLON opnl expr {
			# Conditional expression.
			call imc_quest ($1, $4, $7, $$)
		    }
	|	IDENTIFIER '(' arglist ')' {
			# Function call.
			call imc_call (OP_VALC($1), OP_VALI($3), $$)
		    }
	|	'(' expr ')' {
			YYMOVE ($2, $$)
		    }
	;

arglist	:	{
			# Empty.
			call imc_startarglist (NULL, $$)
		    }
	|	expr {
			# First arg; start a nonnull list.
			call imc_startarglist ($1, $$)
		    }
	|	arglist ',' expr {
			# Add an argument to an existing list.
			call imc_addarg ($1, $3, $$)
		    }
	;

image	:	IDENTIFIER {
			# Image or image section.
			YYMOVE ($1, $$)
		    }
	;

opnl	:	# Empty.
	|	opnl NEWLINE
	;

%%
