%{

include	<ctype.h>
include	<lexnum.h>
include "../lib/lexer.h"
include	"../lib/parser.h"
include	"../lib/prdefs.h"

# Parser stack and structure lengths
define	YYMAXDEPTH	128
define	YYOPLEN		LEN_LEX

# Redefine the name of the parser
define	yyparse		parse

%}


%token	OBSSECT CATSECT EXTSECT TRNSECT
%token	FITID CONSTID DELTAID
%token	ERRORID WEIGHTID MINID MAXID
%token	DERIVID PLOTID SETID
%token	F_ABS F_ACOS F_ASIN F_ATAN F_COS F_EXP
%token	F_LOG F_LOG10 F_SIN F_SQRT F_TAN
%token	IDENTIFIER INUMBER RNUMBER
%token	PLUS MINUS STAR SLASH EXPON
%token	COLON SEMICOLON COMMA EQUAL LPAR RPAR
%token	EOFILE

%left	PLUS MINUS
%left	STAR SLASH
%left	EXPON
%right	UPLUS UMINUS


%%


# Configuration file.

config		: catalog observation extinction transform EOFILE {
		    return (OK)
		}
		| error {
		    return (ERR)
		}
		;


# Observation variable section. Set equations are not allowed here to avoid
# a precedence problem in the parser.

observation	: OBSSECT obscols
		| OBSSECT obscols SEMICOLON
		| OBSSECT SEMICOLON {
		    call pr_error ("The observation section is empty",
				   PERR_WARNING)
		}
		| OBSSECT {
		    call pr_error ("The observation section is empty",
				   PERR_WARNING)
		}
		| empty {
		    call pr_error ("The observation section is undefined",
				   PERR_WARNING)
		}
		;

obscols		: obscol | obscol obscols 

obscol		: IDENTIFIER column {
		    call pr_obscol (LEX_ID ($1), LEX_ID ($2))
		}
		| ERRORID LPAR IDENTIFIER RPAR column {
		    call pr_errcol (LEX_ID ($3), LEX_ID ($5))
		}
		| WEIGHTID LPAR IDENTIFIER RPAR column {
		    call pr_wtscol (LEX_ID ($3), LEX_ID ($5))
		}
		;


# Catalog variable section. Set equations are not allowed here to avoid
# a precedence problem in the parser.

catalog		: CATSECT catminset catcols
		| CATSECT catminset catcols SEMICOLON
		| CATSECT catminset SEMICOLON {
		    call pr_error ("The catalog section is empty",
				   PERR_WARNING)
		}
		| CATSECT catminset {
		    call pr_error ("The catalog section is empty",
				   PERR_WARNING)
		}
		|empty
		;

catminset	: empty {
			call pr_puti (MINCOL, 2)
		}

catcols		: catcol | catcol catcols ;

catcol		: IDENTIFIER column {
		    call pr_catcol (LEX_ID ($1), LEX_ID ($2))
		}
		| ERRORID LPAR IDENTIFIER RPAR column {
		    call pr_errcol (LEX_ID ($3), LEX_ID ($5))
		}
		| WEIGHTID LPAR IDENTIFIER RPAR column {
		    call pr_wtscol (LEX_ID ($3), LEX_ID ($5))
		}
		;


column		: INUMBER {
		    YYMOVE ($1, $$)
		}
		;


# Extinction correction section (NOT YET IMPLEMENTED).

extinction	: EXTSECT SEMICOLON
		| EXTSECT
		| empty
		;


# Transformation section.

transform	: TRNSECT trneqlist
		| TRNSECT SEMICOLON {
		    call pr_error ("The transformation section is empty",
				   PERR_WARNING)
		}
		| TRNSECT {
		    call pr_error ("The transformation section is empty",
				   PERR_WARNING)
		}
		;

trneqlist	: trneq | trneq trneqlist ;

trneq		: trntrans | trnderiv | trnplot
		|fitconstdelta
		| seteq | erroreq | weighteq
		;


# Transformation equation

trntrans	: IDENTIFIER COLON trnrefset stmt EQUAL trnfitset stmt {
		    call pr_treq (LEX_ID ($1),
			LEX_ID ($4), LEX_ID ($7),
			LEX_CODE ($4), LEX_CLEN ($4),
			LEX_CODE ($7), LEX_CLEN ($7))
		}
		;

trnrefset	: empty {
		    call pr_section (PRS_TRNREF)
		}
		;

trnfitset	: empty {
		    call pr_section (PRS_TRNFIT)
		}
		;

# Transformation derivative equation.

trnderiv	: DERIVID LPAR IDENTIFIER COMMA IDENTIFIER RPAR EQUAL
		  trnderset stmt {
		    call pr_trder (LEX_ID ($3), LEX_ID ($5),
			LEX_ID ($9), LEX_CODE ($9), LEX_CLEN ($9))
		}
		;

trnderset	: empty {
		    call pr_section (PRS_TRNDER)
		}
		;

# Transformation plot equation.

trnplot		: PLOTID LPAR IDENTIFIER RPAR EQUAL
		  trnplotset stmt COMMA stmt {
		    call pr_trplot (LEX_ID ($3),
			LEX_ID ($7), LEX_ID ($9),
			LEX_CODE ($7), LEX_CLEN ($7),
			LEX_CODE ($9), LEX_CLEN ($9))
		}
		;

trnplotset	: empty {
		    call pr_section (PRS_TRNPLOT)
		}
		;


# Error equation. This equation is optionally followed by two
# expressions for the minimum and maximum values allowed.

erroreq		: ERRORID LPAR IDENTIFIER RPAR EQUAL erroreqset stmt
		  limitset limitmin limitmax {
		    call pr_erreq (LEX_ID ($3), LEX_ID ($7),
			LEX_ID ($9), LEX_ID ($10),
			LEX_CODE ($7), LEX_CLEN ($7),
			LEX_CODE ($9), LEX_CLEN ($9),
			LEX_CODE ($10), LEX_CLEN ($10))
		}
		;

erroreqset	: empty {
		    call pr_section (PRS_ERREQ)
		}
		;


# Weight equation. This equation is optionally followed by two
# expressions for the minimum and maximum values allowed.

weighteq	: WEIGHTID LPAR IDENTIFIER RPAR EQUAL weighteqset stmt
		  limitset limitmin limitmax {
		    call pr_wtseq (LEX_ID ($3), LEX_ID ($7),
			LEX_ID ($9), LEX_ID ($10),
			LEX_CODE ($7), LEX_CLEN ($7),
			LEX_CODE ($9), LEX_CLEN ($9),
			LEX_CODE ($10), LEX_CLEN ($10))
		}
		;

weighteqset	: empty {
		    call pr_section (PRS_WTSEQ)
		}
		;


# Limit equations for errors and weights.

limitset	: empty {
		    call pr_section (PRS_LMTEQ)
		}
		;

limitmin	: MINID EQUAL stmt {
		    YYMOVE ($3, $$)
		}
		| empty {
		    call strcpy ("", LEX_ID ($$), LEN_ID)
		    LEX_CLEN ($$) = 0
		}
		;

limitmax	: MAXID EQUAL stmt {
		    YYMOVE ($3, $$)
		}
		| empty {
		    call strcpy ("", LEX_ID ($$), LEN_ID)
		    LEX_CLEN ($$) = 0
		}
		;



# Fitting parameter, constant parameter, and parameter deltas definition.
# Although deltas are always positive, the parser allows for negative
# values to avoid a syntax error that would stop the parsing. Check for
# this is left to the symbol table handler procedures.

fitconstdelta	: fit | const | delta ;

fit		: FITID fitinitlist ;

fitinitlist	: fitinit | fitinit COMMA fitinitlist ;

fitinit		: IDENTIFIER EQUAL signedconst {
		    call pr_fitpar (LEX_ID ($1), LEX_ID ($3))
		}
		;

const		: CONSTID constinitlist ;

constinitlist	: constinit | constinit COMMA constinitlist ;

constinit	: IDENTIFIER EQUAL signedconst {
		    call pr_const (LEX_ID ($1), LEX_ID ($3))
		}
		;

delta		: DELTAID deltainitlist ;

deltainitlist	: deltainit | deltainit COMMA deltainitlist ;

deltainit	: IDENTIFIER EQUAL signedconst {
		    call pr_delta (LEX_ID ($1), LEX_ID ($3))
		}
		;


# Set equations.

seteq		: SETID IDENTIFIER EQUAL seteqset stmt {
		    call pr_seteq (LEX_ID ($2), LEX_ID ($5),
				   LEX_CODE ($5), LEX_CLEN ($5))
		}
		;

seteqset	: empty {
		    call pr_section (PRS_SETEQ)
		}
		;



# Statement list (not used for the moment, but it probably will)
#
#stmtlist	: stmt {
#		    YYMOVE ($1, $$)
#		} 
#		| stmt COMMA stmtlist {
#		    call pr_cat3 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($3),
#			LEX_ID ($$), LEN_ID)
#		} 
#		;

# Statement (expression).

stmt		: exprinit expr {
		    YYMOVE ($2, $$)
		    call pr_cend ($$)
		}
		;

exprinit	: empty {
		    call pr_cinit ()
		}
		;

expr		: expr PLUS expr {
		    call pr_cat3 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($3),
			LEX_ID ($$), LEN_ID)
		    call pr_cgen (PLUS, "", INDEFR)
		}
		| expr MINUS expr {
		    call pr_cat3 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($3),
			LEX_ID ($$), LEN_ID)
		    call pr_cgen (MINUS, "", INDEFR)
		}
		| expr STAR expr {
		    call pr_cat3 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($3),
			LEX_ID ($$), LEN_ID)
		    call pr_cgen (STAR, "", INDEFR)
		}
		| expr SLASH expr {
		    call pr_cat3 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($3),
		  	LEX_ID ($$), LEN_ID)
		    call pr_cgen (SLASH, "", INDEFR)
		}
		| expr EXPON expr {
		    call pr_cat3 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($3),
			LEX_ID ($$), LEN_ID)
		    call pr_cgen (EXPON, "", INDEFR)
		}
		| PLUS expr %prec UMINUS {
		    call pr_cat2 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($$), LEN_ID)
		    call pr_cgen (UPLUS, "", INDEFR)
		}
		| MINUS expr %prec UMINUS {
		    call pr_cat2 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($$), LEN_ID)
		    call pr_cgen (UMINUS, "", INDEFR)
		}
		| funct LPAR expr RPAR {
		    call pr_cat4 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($3),
			LEX_ID ($4), LEX_ID ($$), LEN_ID)
		    call pr_cgen (LEX_TOK ($1), "", INDEFR)
		}
		| LPAR expr RPAR {
		    call pr_cat3 (LEX_ID ($1), LEX_ID ($2), LEX_ID ($3),
			LEX_ID ($$), LEN_ID)
		    }
		| constant {
		    YYMOVE ($1, $$)
		    call pr_cgen (RNUMBER, "", LEX_VAL ($1))
		}
		| IDENTIFIER {
		    call pr_chkid (LEX_ID ($1))
		    YYMOVE ($1, $$)
		    call pr_cgen (IDENTIFIER, LEX_ID ($1), INDEFR)
		}
		;

funct		: F_ABS {
	    	    YYMOVE ($1, $$)
		}
      		| F_ACOS {
	    	    YYMOVE ($1, $$)
		}
      		| F_ASIN {
		    YYMOVE ($1, $$)
		}
		| F_ATAN {
		    YYMOVE ($1, $$)
		}
		| F_COS {
		    YYMOVE ($1, $$)
		}
		| F_EXP {
		    YYMOVE ($1, $$)
		}
		| F_LOG {
		    YYMOVE ($1, $$)
		}
		| F_LOG10 {
		    YYMOVE ($1, $$)
		}
		| F_SIN {
		    YYMOVE ($1, $$)
		}
		| F_SQRT {
		    YYMOVE ($1, $$)
		}
		| F_TAN {
		    YYMOVE ($1, $$)
		}
		;


signedconst	: sign constant {
		    call pr_cat2 (LEX_ID ($1), LEX_ID ($2),
			LEX_ID ($$), LEN_ID)
		    LEX_VAL ($$) = LEX_VAL ($2)
		}
		;

sign		: PLUS %prec UMINUS {
		    YYMOVE ($1, $$)
		}
		| MINUS %prec UMINUS {
		    YYMOVE ($1, $$)
		}
		| empty {
		    call strcpy ("", LEX_ID ($$), LEN_ID)
		}
		;

constant	: INUMBER {
		    YYMOVE ($1, $$)
		}
		| RNUMBER {
		    YYMOVE ($1, $$)
		}
		;


empty		: ;


%%
