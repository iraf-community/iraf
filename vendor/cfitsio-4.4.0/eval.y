/************************************************************************/
/*                                                                      */
/*                       CFITSIO Lexical Parser                         */
/*                                                                      */

/* All functions preceeded by fits_parser_yy for uniqueness             */
%define api.prefix {fits_parser_yy}

/* Pure reentrant parser                                                */
%define api.pure full
/* Lexer called with extra state variable                               */
%param { yyscan_t scanner }
/* Parser called with extra parse state variable                        */
%parse-param { ParseData *lParse }

%{
/* This file is one of 3 files containing code which parses an          */
/* arithmetic expression and evaluates it in the context of an input    */
/* FITS file table extension.  The CFITSIO lexical parser is divided    */
/* into the following 3 parts/files: the CFITSIO "front-end",           */
/* eval_f.c, contains the interface between the user/CFITSIO and the    */
/* real core of the parser; the FLEX interpreter, eval_l.c, takes the   */
/* input string and parses it into tokens and identifies the FITS       */
/* information required to evaluate the expression (ie, keywords and    */
/* columns); and, the BISON grammar and evaluation routines, eval_y.c,  */
/* receives the FLEX output and determines and performs the actual      */
/* operations.  The files eval_l.c and eval_y.c are produced from       */
/* running flex and bison on the files eval.l and eval.y, respectively. */
/* (flex and bison are available from any GNU archive: see www.gnu.org) */
/*                                                                      */
/* The grammar rules, rather than evaluating the expression in situ,    */
/* builds a tree, or Nodal, structure mapping out the order of          */
/* operations and expression dependencies.  This "compilation" process  */
/* allows for much faster processing of multiple rows.  This technique  */
/* was developed by Uwe Lammers of the XMM Science Analysis System,     */
/* although the CFITSIO implementation is entirely code original.       */
/*                                                                      */
/*                                                                      */
/* Modification History:                                                */
/*                                                                      */
/*   Kent Blackburn      c1992  Original parser code developed for the  */
/*                              FTOOLS software package, in particular, */
/*                              the fselect task.                       */
/*   Kent Blackburn      c1995  BIT column support added                */
/*   Peter D Wilson   Feb 1998  Vector column support added             */
/*   Peter D Wilson   May 1998  Ported to CFITSIO library.  User        */
/*                              interface routines written, in essence  */
/*                              making fselect, fcalc, and maketime     */
/*                              capabilities available to all tools     */
/*                              via single function calls.              */
/*   Peter D Wilson   Jun 1998  Major rewrite of parser core, so as to  */
/*                              create a run-time evaluation tree,      */
/*                              inspired by the work of Uwe Lammers,    */
/*                              resulting in a speed increase of        */
/*                              10-100 times.                           */
/*   Peter D Wilson   Jul 1998  gtifilter(a,b,c,d) function added       */
/*   Peter D Wilson   Aug 1998  regfilter(a,b,c,d) function added       */
/*   Peter D Wilson   Jul 1999  Make parser fitsfile-independent,       */
/*                              allowing a purely vector-based usage    */
/*  Craig B Markwardt Jun 2004  Add MEDIAN() function                   */
/*  Craig B Markwardt Jun 2004  Add SUM(), and MIN/MAX() for bit arrays */
/*  Craig B Markwardt Jun 2004  Allow subscripting of nX bit arrays     */
/*  Craig B Markwardt Jun 2004  Implement statistical functions         */
/*                              NVALID(), AVERAGE(), and STDDEV()       */
/*                              for integer and floating point vectors  */
/*  Craig B Markwardt Jun 2004  Use NULL values for range errors instead*/
/*                              of throwing a parse error               */
/*  Craig B Markwardt Oct 2004  Add ACCUM() and SEQDIFF() functions     */
/*  Craig B Markwardt Feb 2005  Add ANGSEP() function                   */
/*  Craig B Markwardt Aug 2005  CIRCLE, BOX, ELLIPSE, NEAR and REGFILTER*/
/*                              functions now accept vector arguments   */
/*  Craig B Markwardt Sum 2006  Add RANDOMN() and RANDOMP() functions   */
/*  Craig B Markwardt Mar 2007  Allow arguments to RANDOM and RANDOMN to*/
/*                              determine the output dimensions         */
/*  Craig B Markwardt Aug 2009  Add substring STRMID() and string search*/
/*                              STRSTR() functions; more overflow checks*/
/*  Craig B Markwardt Dec 2019  Add bit/hex/oct literal strings and     */
/*                              bitwise operatiosn between integers     */
/*  Craig B Markwardt Mar 2021  Add SETNULL() function                  */
/*                                                                      */
/************************************************************************/

#define  APPROX 1.0e-7
#include "eval_defs.h"
#include "region.h"
#include <time.h>

#include <stdlib.h>

#ifndef alloca
#define alloca malloc
#endif

/* Random number generators for various distributions */
#include "simplerng.h"

   /*  Shrink the initial stack depth to keep local data <32K (mac limit)  */
   /*  yacc will allocate more space if needed, though.                    */
#define  YYINITDEPTH   100

/***************************************************************/
/*  Replace Bison's BACKUP macro with one that fixes a bug --  */
/*  must update state after popping the stack -- and allows    */
/*  popping multiple terms at one time.                        */
/***************************************************************/

#define YYNEWBACKUP(token, value)                               \
   do								\
     if (yychar == YYEMPTY )   					\
       { yychar = (token);                                      \
         memcpy( &yylval, &(value), sizeof(value) );            \
         yychar1 = YYTRANSLATE (yychar);			\
         while (yylen--) YYPOPSTACK;				\
         yystate = *yyssp;					\
         goto yybackup;						\
       }							\
     else							\
       { yyerror ("syntax error: cannot back up"); YYERROR; }	\
   while (0)

/***************************************************************/
/*  Useful macros for accessing/testing Nodes                  */
/***************************************************************/

#define TEST(a)        if( (a)<0 ) YYERROR
#define SIZE(a)        lParse->Nodes[ a ].value.nelem
#define TYPE(a)        lParse->Nodes[ a ].type
#define OPER(a)        lParse->Nodes[ a ].operation
#define PROMOTE(a,b)   if( TYPE(a) > TYPE(b) )                  \
                          b = New_Unary( lParse, TYPE(a), 0, b );       \
                       else if( TYPE(a) < TYPE(b) )             \
	                  a = New_Unary( lParse, TYPE(b), 0, a );

/*****  Internal functions  *****/

#ifdef __cplusplus
extern "C" {
#endif

static int  Alloc_Node    ( ParseData * );
static void Free_Last_Node( ParseData * );
static void Evaluate_Node ( ParseData *, int thisNode );

static int  New_Const ( ParseData *, int returnType, void *value, long len );
static int  New_Column( ParseData *, int ColNum );
static int  New_Offset( ParseData *, int ColNum, int offset );
static int  New_Unary ( ParseData *, int returnType, int Op, int Node1 );
static int  New_BinOp ( ParseData *, int returnType, int Node1, int Op, int Node2 );
static int  New_Func  ( ParseData *, int returnType, funcOp Op, int nNodes,
			int Node1, int Node2, int Node3, int Node4, 
			int Node5, int Node6, int Node7 );
static int  New_FuncSize( ParseData *, int returnType, funcOp Op, int nNodes,
			int Node1, int Node2, int Node3, int Node4, 
			  int Node5, int Node6, int Node7, int Size);
static int  New_Deref ( ParseData *, int Var,  int nDim,
			int Dim1, int Dim2, int Dim3, int Dim4, int Dim5 );
static int  New_GTI   ( ParseData *, funcOp Op, char *fname, int Node1, int Node2, char *start, char *stop );
static int  New_REG   ( ParseData *, char *fname, int NodeX, int NodeY, char *colNames );
static int  New_Vector( ParseData *, int subNode );
static int  Close_Vec ( ParseData *, int vecNode );
static int  New_Array(  ParseData *, int valueNode, int dimNode );
static int  Locate_Col( ParseData *, Node *this );
static int  Test_Dims ( ParseData *, int Node1, int Node2 );
static void Copy_Dims ( ParseData *, int Node1, int Node2 );

static void Allocate_Ptrs( ParseData *, Node *this );
static void Do_Unary     ( ParseData *, Node *this );
static void Do_Offset    ( ParseData *, Node *this );
static void Do_BinOp_bit ( ParseData *, Node *this );
static void Do_BinOp_str ( ParseData *, Node *this );
static void Do_BinOp_log ( ParseData *, Node *this );
static void Do_BinOp_lng ( ParseData *, Node *this );
static void Do_BinOp_dbl ( ParseData *, Node *this );
static void Do_Func      ( ParseData *, Node *this );
static void Do_Deref     ( ParseData *, Node *this );
static void Do_GTI       ( ParseData *, Node *this );
static void Do_GTI_Over  ( ParseData *, Node *this );
static void Do_REG       ( ParseData *, Node *this );
static void Do_Vector    ( ParseData *, Node *this );
static void Do_Array     ( ParseData *, Node *this );

static long Search_GTI   ( double evtTime, long nGTI, double *start,
			   double *stop, int ordered, long *nextGTI );
static double GTI_Over(double evtStart, double evtStop,
		       long nGTI, double *start, double *stop,
		       long *gtiout);

static char  saobox (double xcen, double ycen, double xwid, double ywid,
		     double rot,  double xcol, double ycol);
static char  ellipse(double xcen, double ycen, double xrad, double yrad,
		     double rot, double xcol, double ycol);
static char  circle (double xcen, double ycen, double rad,
		     double xcol, double ycol);
static char  bnear  (double x, double y, double tolerance);
static char  bitcmp (char *bitstrm1, char *bitstrm2);
static char  bitlgte(char *bits1, int oper, char *bits2);

static void  bitand(char *result, char *bitstrm1, char *bitstrm2);
static void  bitor (char *result, char *bitstrm1, char *bitstrm2);
static void  bitnot(char *result, char *bits);
static int cstrmid(ParseData *lParse, char *dest_str, int dest_len,
		   char *src_str,  int src_len, int pos);

static void yyerror(yyscan_t scanner, ParseData *lParse, char *s);

#ifdef __cplusplus
    }
#endif

%}

%union {
    int    Node;        /* Index of Node */
    double dbl;         /* real value    */
    long   lng;         /* integer value */
    char   log;         /* logical value */
    char   str[MAX_STRLEN];    /* string value  */
}

%token <log>   BOOLEAN        /* First 3 must be in order of        */
%token <lng>   LONG           /* increasing promotion for later use */
%token <dbl>   DOUBLE
%token <str>   STRING
%token <str>   BITSTR
%token <str>   FUNCTION
%token <str>   BFUNCTION      /* Bit function */
%token <str>   IFUNCTION      /* Integer function */
%token <str>   GTIFILTER
%token <str>   GTIOVERLAP
%token <str>   GTIFIND
%token <str>   REGFILTER
%token <lng>   COLUMN
%token <lng>   BCOLUMN
%token <lng>   SCOLUMN
%token <lng>   BITCOL
%token <lng>   ROWREF
%token <lng>   NULLREF
%token <lng>   SNULLREF

%type <Node>  expr
%type <Node>  bexpr
%type <Node>  sexpr
%type <Node>  bits
%type <Node>  vector
%type <Node>  bvector

%left     ',' '=' ':' '{' '}'
%right    '?'
%left     OR
%left     AND
%left     EQ NE '~'
%left     GT LT LTE GTE
%left     '+' '-' '%'
%left     '*' '/'
%left     '|' '&' XOR
%right    POWER
%left     NOT
%left     INTCAST FLTCAST
%left     UMINUS
%left     '['

%right    ACCUM DIFF

%%

lines:   /* nothing ; was | lines line */
       | lines line
       ;

line:           '\n' {}
       | expr   '\n'
                { if( $1<0 ) {
		     yyerror(scanner, lParse, "Couldn't build node structure: out of memory?");
		     YYERROR;  }
                  lParse->resultNode = $1;
		}
       | bexpr  '\n'
                { if( $1<0 ) {
		     yyerror(scanner, lParse, "Couldn't build node structure: out of memory?");
		     YYERROR;  }
                  lParse->resultNode = $1;
		}
       | sexpr  '\n'
                { if( $1<0 ) {
		     yyerror(scanner, lParse, "Couldn't build node structure: out of memory?");
		     YYERROR;  } 
                  lParse->resultNode = $1;
		}
       | bits   '\n'
                { if( $1<0 ) {
		     yyerror(scanner, lParse, "Couldn't build node structure: out of memory?");
		     YYERROR;  }
                  lParse->resultNode = $1;
		}
       | error  '\n' {  yyerrok;  }
       ;

bvector: '{' bexpr
                { $$ = New_Vector(lParse,  $2 ); TEST($$); }
       | bvector ',' bexpr
                {
                  if( lParse->Nodes[$1].nSubNodes >= MAXSUBS ) {
		     $1 = Close_Vec(lParse,  $1 ); TEST($1);
		     $$ = New_Vector(lParse,  $1 ); TEST($$);
                  } else {
                     $$ = $1;
                  }
		  lParse->Nodes[$$].SubNodes[ lParse->Nodes[$$].nSubNodes++ ]
		     = $3;
                }
       ;

vector:  '{' expr
                { $$ = New_Vector(lParse,  $2 ); TEST($$); }
       | vector ',' expr
                {
                  if( TYPE($1) < TYPE($3) )
                     TYPE($1) = TYPE($3);
                  if( lParse->Nodes[$1].nSubNodes >= MAXSUBS ) {
		     $1 = Close_Vec(lParse,  $1 ); TEST($1);
		     $$ = New_Vector(lParse,  $1 ); TEST($$);
                  } else {
                     $$ = $1;
                  }
		  lParse->Nodes[$$].SubNodes[ lParse->Nodes[$$].nSubNodes++ ]
		     = $3;
                }
       | vector ',' bexpr
                {
                  if( lParse->Nodes[$1].nSubNodes >= MAXSUBS ) {
		     $1 = Close_Vec(lParse,  $1 ); TEST($1);
		     $$ = New_Vector(lParse,  $1 ); TEST($$);
                  } else {
                     $$ = $1;
                  }
		  lParse->Nodes[$$].SubNodes[ lParse->Nodes[$$].nSubNodes++ ]
		     = $3;
                }
       | bvector ',' expr
                {
                  TYPE($1) = TYPE($3);
                  if( lParse->Nodes[$1].nSubNodes >= MAXSUBS ) {
		     $1 = Close_Vec(lParse,  $1 ); TEST($1);
		     $$ = New_Vector(lParse,  $1 ); TEST($$);
                  } else {
                     $$ = $1;
                  }
		  lParse->Nodes[$$].SubNodes[ lParse->Nodes[$$].nSubNodes++ ]
		     = $3;
                }
       ;

expr:    vector '}'
                { $$ = Close_Vec(lParse,  $1 ); TEST($$); }
       ;

bexpr:   bvector '}'
                { $$ = Close_Vec(lParse,  $1 ); TEST($$); }
       ;

bits:	 BITSTR
                {
                  $$ = New_Const(lParse,  BITSTR, $1, strlen($1)+1 ); TEST($$);
		  SIZE($$) = strlen($1); }
       | BITCOL
                { $$ = New_Column(lParse,  $1 ); TEST($$); }
       | BITCOL '{' expr '}'
                {
                  if( TYPE($3) != LONG
		      || OPER($3) != CONST_OP ) {
		     yyerror(scanner, lParse, "Offset argument must be a constant integer");
		     YYERROR;
		  }
                  $$ = New_Offset(lParse,  $1, $3 ); TEST($$);
                }
       | bits '&' bits
                { $$ = New_BinOp(lParse,  BITSTR, $1, '&', $3 ); TEST($$);
                  SIZE($$) = ( SIZE($1)>SIZE($3) ? SIZE($1) : SIZE($3) );  }
       | bits '|' bits
                { $$ = New_BinOp(lParse,  BITSTR, $1, '|', $3 ); TEST($$);
                  SIZE($$) = ( SIZE($1)>SIZE($3) ? SIZE($1) : SIZE($3) );  }
       | bits '+' bits
                { 
		  if (SIZE($1)+SIZE($3) >= MAX_STRLEN) {
		    yyerror(scanner, lParse, "Combined bit string size exceeds " MAX_STRLEN_S " bits");
		    YYERROR;
		  }
		  $$ = New_BinOp(lParse,  BITSTR, $1, '+', $3 ); TEST($$);
                  SIZE($$) = SIZE($1) + SIZE($3); 
		}
       | bits '[' expr ']'
                { $$ = New_Deref(lParse,  $1, 1, $3,  0,  0,  0,   0 ); TEST($$); }
       | bits '[' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 2, $3, $5,  0,  0,   0 ); TEST($$); }
       | bits '[' expr ',' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 3, $3, $5, $7,  0,   0 ); TEST($$); }
       | bits '[' expr ',' expr ',' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 4, $3, $5, $7, $9,   0 ); TEST($$); }
       | bits '[' expr ',' expr ',' expr ',' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 5, $3, $5, $7, $9, $11 ); TEST($$); }
       | NOT bits
                { $$ = New_Unary(lParse,  BITSTR, NOT, $2 ); TEST($$);     }

       | '(' bits ')'
                { $$ = $2; }
       ;

expr:    LONG
                { $$ = New_Const(lParse,  LONG,   &($1), sizeof(long)   ); TEST($$); }
       | DOUBLE
                { $$ = New_Const(lParse,  DOUBLE, &($1), sizeof(double) ); TEST($$); }
       | COLUMN
                { $$ = New_Column(lParse,  $1 ); TEST($$); }
       | COLUMN '{' expr '}'
                {
                  if( TYPE($3) != LONG
		      || OPER($3) != CONST_OP ) {
		     yyerror(scanner, lParse, "Offset argument must be a constant integer");
		     YYERROR;
		  }
                  $$ = New_Offset(lParse,  $1, $3 ); TEST($$);
                }
       | ROWREF
                { $$ = New_Func(lParse,  LONG, row_fct,  0, 0, 0, 0, 0, 0, 0, 0 ); }
       | NULLREF
                { $$ = New_Func(lParse,  LONG, null_fct, 0, 0, 0, 0, 0, 0, 0, 0 ); }
       | expr '%' expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  TYPE($1), $1, '%', $3 );
		  TEST($$);                                                }
       | expr '+' expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  TYPE($1), $1, '+', $3 );
		  TEST($$);                                                }
       | expr '-' expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  TYPE($1), $1, '-', $3 ); 
		  TEST($$);                                                }
       | expr '*' expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  TYPE($1), $1, '*', $3 ); 
		  TEST($$);                                                }
       | expr '/' expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  TYPE($1), $1, '/', $3 ); 
		  TEST($$);                                                }
       | expr '&' expr
                { 
                   if (TYPE($1) != LONG ||
		       TYPE($3) != LONG) {
                     yyerror(scanner, lParse, "Bitwise operations with incompatible types; only (bit OP bit) and (int OP int) are allowed");
                      YYERROR;
                   }
                   $$ = New_BinOp(lParse,  TYPE($1), $1, '&', $3 );
                }
       | expr '|' expr
                { 
                   if (TYPE($1) != LONG ||
		       TYPE($3) != LONG) {
                     yyerror(scanner, lParse, "Bitwise operations with incompatible types; only (bit OP bit) and (int OP int) are allowed");
                      YYERROR;
                   }
                   $$ = New_BinOp(lParse,  TYPE($1), $1, '|', $3 );
                }
       | expr XOR expr
                { 
                   if (TYPE($1) != LONG ||
		       TYPE($3) != LONG) {
                     yyerror(scanner, lParse, "Bitwise operations with incompatible types; only (bit OP bit) and (int OP int) are allowed");
                      YYERROR;
                   }
                   $$ = New_BinOp(lParse,  TYPE($1), $1, '^', $3 );
                }
       | expr POWER expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  TYPE($1), $1, POWER, $3 );
		  TEST($$);                                                }
       | '+' expr %prec UMINUS
                { $$ = $2; }
       | '-' expr %prec UMINUS
                { $$ = New_Unary(lParse,  TYPE($2), UMINUS, $2 ); TEST($$); }
       |  '(' expr ')'
                { $$ = $2; }
       | expr '*' bexpr
                { $3 = New_Unary(lParse,  TYPE($1), 0, $3 );
                  $$ = New_BinOp(lParse,  TYPE($1), $1, '*', $3 ); 
		  TEST($$);                                }
       | bexpr '*' expr
                { $1 = New_Unary(lParse,  TYPE($3), 0, $1 );
                  $$ = New_BinOp(lParse,  TYPE($3), $1, '*', $3 );
                  TEST($$);                                }
       | bexpr '?' expr ':' expr
                {
                  PROMOTE($3,$5);
                  if( ! Test_Dims( lParse, $3,$5) ) {
                     yyerror(scanner, lParse, "Incompatible dimensions in '?:' arguments");
		     YYERROR;
                  }
                  $$ = New_Func(lParse,  0, ifthenelse_fct, 3, $3, $5, $1,
                                 0, 0, 0, 0 );
                  TEST($$);
                  if( SIZE($3)<SIZE($5) )  Copy_Dims( lParse,$$, $5);
                  TYPE($1) = TYPE($3);
                  if( ! Test_Dims( lParse, $1,$$) ) {
                     yyerror(scanner, lParse, "Incompatible dimensions in '?:' condition");
		     YYERROR;
                  }
                  TYPE($1) = BOOLEAN;
                  if( SIZE($$)<SIZE($1) )  Copy_Dims( lParse,$$, $1);
                }
       | bexpr '?' bexpr ':' expr
                {
                  PROMOTE($3,$5);
                  if( ! Test_Dims( lParse, $3,$5) ) {
                     yyerror(scanner, lParse, "Incompatible dimensions in '?:' arguments");
		     YYERROR;
                  }
                  $$ = New_Func(lParse,  0, ifthenelse_fct, 3, $3, $5, $1,
                                 0, 0, 0, 0 );
                  TEST($$);
                  if( SIZE($3)<SIZE($5) )  Copy_Dims( lParse,$$, $5);
                  TYPE($1) = TYPE($3);
                  if( ! Test_Dims( lParse, $1,$$) ) {
                     yyerror(scanner, lParse, "Incompatible dimensions in '?:' condition");
		     YYERROR;
                  }
                  TYPE($1) = BOOLEAN;
                  if( SIZE($$)<SIZE($1) )  Copy_Dims( lParse,$$, $1);
                }
       | bexpr '?' expr ':' bexpr
                {
                  PROMOTE($3,$5);
                  if( ! Test_Dims( lParse, $3,$5) ) {
                     yyerror(scanner, lParse, "Incompatible dimensions in '?:' arguments");
		     YYERROR;
                  }
                  $$ = New_Func(lParse,  0, ifthenelse_fct, 3, $3, $5, $1,
                                 0, 0, 0, 0 );
                  TEST($$);
                  if( SIZE($3)<SIZE($5) )  Copy_Dims( lParse,$$, $5);
                  TYPE($1) = TYPE($3);
                  if( ! Test_Dims( lParse, $1,$$) ) {
                     yyerror(scanner, lParse, "Incompatible dimensions in '?:' condition");
		     YYERROR;
                  }
                  TYPE($1) = BOOLEAN;
                  if( SIZE($$)<SIZE($1) )  Copy_Dims( lParse,$$, $1);
                }
       | FUNCTION ')'
                { if (FSTRCMP($1,"RANDOM(") == 0) {  /* Scalar RANDOM() */
                     $$ = New_Func(lParse,  DOUBLE, rnd_fct, 0, 0, 0, 0, 0, 0, 0, 0 );
		  } else if (FSTRCMP($1,"RANDOMN(") == 0) {/*Scalar RANDOMN()*/
		     $$ = New_Func(lParse,  DOUBLE, gasrnd_fct, 0, 0, 0, 0, 0, 0, 0, 0 );
                  } else {
                     yyerror(scanner, lParse, "Function() not supported");
		     YYERROR;
		  }
                  TEST($$); 
                }
       | FUNCTION bexpr ')'
                { if (FSTRCMP($1,"SUM(") == 0) {
		     $$ = New_Func(lParse,  LONG, sum_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
                  } else if (FSTRCMP($1,"NELEM(") == 0) {
                     $$ = New_Const(lParse,  LONG, &( SIZE($2) ), sizeof(long) );
                  } else if (FSTRCMP($1,"ACCUM(") == 0) {
		    long zero = 0;
		    $$ = New_BinOp(lParse,  LONG , $2, ACCUM, New_Const(lParse,  LONG, &zero, sizeof(zero) ));
		  } else {
                     yyerror(scanner, lParse, "Function(bool) not supported");
		     YYERROR;
		  }
                  TEST($$); 
		}
       | FUNCTION bexpr ',' expr ')'
                { if (FSTRCMP($1,"AXISELEM(") == 0) {  /* AXISELEM(V,n) */
		     if (OPER($4) != CONST_OP
			 || SIZE($4) != 1) {
		       yyerror(scanner, lParse, "AXISELEM second argument must be a scalar constant");
		       YYERROR;
		     }
		     if (OPER($2) == CONST_OP) {
		       long one = 1;
		       $$ = New_Const(lParse,  LONG, &one, sizeof(one) );
		     } else {
		       if ( TYPE($4) != LONG ) $4 = New_Unary(lParse, LONG, 0, $4);
		       $$ = New_Func(lParse, 0, axiselem_fct, 2, $2, $4, 0, 0, 0, 0, 0 );
		       TEST($$);
		       TYPE($$) = LONG;
		     }
		   } else if (FSTRCMP($1,"NAXES(") == 0) {  /* NAXES(V,n) */
		     if (OPER($4) != CONST_OP
			 || SIZE($4) != 1) {
		       yyerror(scanner, lParse, "NAXES second argument must be a scalar constant");
		       YYERROR;
		     }
		     if (OPER($2) == CONST_OP) { /* if V is constant, return 1 in every case */
		       long one = 1;
		       $$ = New_Const(lParse,  LONG, &one, sizeof(one) );
		     } else {                    /* determine now the dimension of the expression */
		       long iaxis;
		       int naxis;
		       if ( TYPE($4) != LONG ) $4 = New_Unary(lParse, LONG, 0, $4);
		       /* Since it is already constant, we can extract long value directly */
		       iaxis = (lParse->Nodes[$4].value.data.lng);
		       naxis = lParse->Nodes[$2].value.naxis;

		       if (iaxis == 0)          iaxis = naxis;   /* NAXIS(V,0) = NAXIS */
		       else if (iaxis <= naxis) iaxis = lParse->Nodes[$2].value.naxes[iaxis-1]; /* NAXIS(V,n) = NAXISn */
		       else                     iaxis = 1;       /* Out of bounds use 1 */

		       $$ = New_Const(lParse,  LONG, &iaxis, sizeof(iaxis) );
		       TEST($$);
		     }
		   } else if (FSTRCMP($1,"ARRAY(") == 0) {  /* NAXES(bexpr,n) */
		     $$ = New_Array(lParse, $2, $4);
		     TEST($$);
		  } else {
                     yyerror(scanner, lParse, "Function(bool,expr) not supported");
		     YYERROR;
		  }
                  TEST($$); 
		}
       | FUNCTION sexpr ')'
                { if (FSTRCMP($1,"NELEM(") == 0) {
                     $$ = New_Const(lParse,  LONG, &( SIZE($2) ), sizeof(long) );
		  } else if (FSTRCMP($1,"NVALID(") == 0) {
		     $$ = New_Func(lParse,  LONG, nonnull_fct, 1, $2,
				    0, 0, 0, 0, 0, 0 );
		  } else {
                     yyerror(scanner, lParse, "Function(str) not supported");
		     YYERROR;
		  }
                  TEST($$); 
		}
       | FUNCTION bits ')'
                { if (FSTRCMP($1,"NELEM(") == 0) {
                     $$ = New_Const(lParse,  LONG, &( SIZE($2) ), sizeof(long) );
		} else if (FSTRCMP($1,"NVALID(") == 0) { /* Bit arrays do not have NULL */
                     $$ = New_Const(lParse,  LONG, &( SIZE($2) ), sizeof(long) );
		} else if (FSTRCMP($1,"SUM(") == 0) {
		     $$ = New_Func(lParse,  LONG, sum_fct, 1, $2,
				    0, 0, 0, 0, 0, 0 );
		} else if (FSTRCMP($1,"MIN(") == 0) {
		     $$ = New_Func(lParse,  TYPE($2),  /* Force 1D result */
				    min1_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     /* Note: $2 is a vector so the result can never
		        be a constant.  Therefore it will never be set
		        inside New_Func(), and it is safe to set SIZE() */
		     SIZE($$) = 1;
		} else if (FSTRCMP($1,"ACCUM(") == 0) {
		    long zero = 0;
		    $$ = New_BinOp(lParse,  LONG , $2, ACCUM, New_Const(lParse,  LONG, &zero, sizeof(zero) ));
		} else if (FSTRCMP($1,"MAX(") == 0) {
		     $$ = New_Func(lParse,  TYPE($2),  /* Force 1D result */
				    max1_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     /* Note: $2 is a vector so the result can never
		        be a constant.  Therefore it will never be set
		        inside New_Func(), and it is safe to set SIZE() */
		     SIZE($$) = 1;
		} else {
                     yyerror(scanner, lParse, "Function(bits) not supported");
		     YYERROR;
		  }
                  TEST($$); 
		}
       | FUNCTION expr ')'
                { if (FSTRCMP($1,"SUM(") == 0)
		     $$ = New_Func(lParse,  TYPE($2), sum_fct, 1, $2,
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP($1,"AVERAGE(") == 0)
		     $$ = New_Func(lParse,  DOUBLE, average_fct, 1, $2,
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP($1,"STDDEV(") == 0)
		     $$ = New_Func(lParse,  DOUBLE, stddev_fct, 1, $2,
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP($1,"MEDIAN(") == 0)
		     $$ = New_Func(lParse,  TYPE($2), median_fct, 1, $2,
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP($1,"NELEM(") == 0)
                     $$ = New_Const(lParse,  LONG, &( SIZE($2) ), sizeof(long) );
		  else if (FSTRCMP($1,"NVALID(") == 0)
		     $$ = New_Func(lParse,  LONG, nonnull_fct, 1, $2,
				    0, 0, 0, 0, 0, 0 );
		  else if   ((FSTRCMP($1,"ACCUM(") == 0) && (TYPE($2) == LONG)) {
		    long zero = 0;
		    $$ = New_BinOp(lParse,  LONG ,   $2, ACCUM, New_Const(lParse,  LONG,   &zero, sizeof(zero) ));
		  } else if ((FSTRCMP($1,"ACCUM(") == 0) && (TYPE($2) == DOUBLE)) {
		    double zero = 0;
		    $$ = New_BinOp(lParse,  DOUBLE , $2, ACCUM, New_Const(lParse,  DOUBLE, &zero, sizeof(zero) ));
		  } else if ((FSTRCMP($1,"SEQDIFF(") == 0) && (TYPE($2) == LONG)) {
		    long zero = 0;
		    $$ = New_BinOp(lParse,  LONG ,   $2, DIFF, New_Const(lParse,  LONG,   &zero, sizeof(zero) ));
		  } else if ((FSTRCMP($1,"SEQDIFF(") == 0) && (TYPE($2) == DOUBLE)) {
		    double zero = 0;
		    $$ = New_BinOp(lParse,  DOUBLE , $2, DIFF, New_Const(lParse,  DOUBLE, &zero, sizeof(zero) ));
		  } else if (FSTRCMP($1,"ABS(") == 0)
		     $$ = New_Func(lParse,  0, abs_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
 		  else if (FSTRCMP($1,"MIN(") == 0)
		     $$ = New_Func(lParse,  TYPE($2),  /* Force 1D result */
				    min1_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP($1,"MAX(") == 0)
		     $$ = New_Func(lParse,  TYPE($2),  /* Force 1D result */
				    max1_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP($1,"RANDOM(") == 0) { /* Vector RANDOM() */
                     $$ = New_Func(lParse,  0, rnd_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     TEST($$);
		     TYPE($$) = DOUBLE;
		  } else if (FSTRCMP($1,"RANDOMN(") == 0) {
		     $$ = New_Func(lParse,  0, gasrnd_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     TEST($$);
		     TYPE($$) = DOUBLE;
		  } else if (FSTRCMP($1,"ELEMENTNUM(") == 0) {
		     if (OPER($2) == CONST_OP) {
		       long one = 1;
		       $$ = New_Const(lParse,  LONG, &one, sizeof(one) );
		     } else {
		       $$ = New_Func(lParse,  0, elemnum_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		       TEST($$);
		       TYPE($$) = LONG;
		     }
		  } else if (FSTRCMP($1,"NAXIS(") == 0) {  /* NAXIS(V) */
		     if (OPER($2) == CONST_OP) { /* if V is constant, return 1 in every case */
		       long one = 1;
		       $$ = New_Const(lParse,  LONG, &one, sizeof(one) );
		     } else {                    /* determine now the dimension of the expression */
		       long naxis = lParse->Nodes[$2].value.naxis;

		       $$ = New_Const(lParse,  LONG, &naxis, sizeof(naxis) );
		       TEST($$);
		     }
                  } 
  		  else {  /*  These all take DOUBLE arguments  */
		     if( TYPE($2) != DOUBLE ) $2 = New_Unary(lParse,  DOUBLE, 0, $2 );
                     if (FSTRCMP($1,"SIN(") == 0)
			$$ = New_Func(lParse,  0, sin_fct,  1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"COS(") == 0)
			$$ = New_Func(lParse,  0, cos_fct,  1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"TAN(") == 0)
			$$ = New_Func(lParse,  0, tan_fct,  1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"ARCSIN(") == 0
			      || FSTRCMP($1,"ASIN(") == 0)
			$$ = New_Func(lParse,  0, asin_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"ARCCOS(") == 0
			      || FSTRCMP($1,"ACOS(") == 0)
			$$ = New_Func(lParse,  0, acos_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"ARCTAN(") == 0
			      || FSTRCMP($1,"ATAN(") == 0)
			$$ = New_Func(lParse,  0, atan_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"SINH(") == 0)
			$$ = New_Func(lParse,  0, sinh_fct,  1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"COSH(") == 0)
			$$ = New_Func(lParse,  0, cosh_fct,  1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"TANH(") == 0)
			$$ = New_Func(lParse,  0, tanh_fct,  1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"EXP(") == 0)
			$$ = New_Func(lParse,  0, exp_fct,  1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"LOG(") == 0)
			$$ = New_Func(lParse,  0, log_fct,  1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"LOG10(") == 0)
			$$ = New_Func(lParse,  0, log10_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"SQRT(") == 0)
			$$ = New_Func(lParse,  0, sqrt_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"ROUND(") == 0)
			$$ = New_Func(lParse,  0, round_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"FLOOR(") == 0)
			$$ = New_Func(lParse,  0, floor_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"CEIL(") == 0)
			$$ = New_Func(lParse,  0, ceil_fct, 1, $2, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP($1,"RANDOMP(") == 0) {
		       $$ = New_Func(lParse,  0, poirnd_fct, 1, $2, 
				      0, 0, 0, 0, 0, 0 );
		       TYPE($$) = LONG;
		     } else {
			yyerror(scanner, lParse, "Function(expr) not supported");
			YYERROR;
		     }
		  }
                  TEST($$); 
                }
       | IFUNCTION sexpr ',' sexpr ')'
                { 
		  if (FSTRCMP($1,"STRSTR(") == 0) {
		    $$ = New_Func(lParse,  LONG, strpos_fct, 2, $2, $4, 0, 
				   0, 0, 0, 0 );
		    TEST($$);
		  }
                }
       | FUNCTION expr ',' expr ')'
                { 
		   if (FSTRCMP($1,"DEFNULL(") == 0) {
		      if( SIZE($2)>=SIZE($4) && Test_Dims( lParse,  $2, $4 ) ) {
			 PROMOTE($2,$4);
			 $$ = New_Func(lParse,  0, defnull_fct, 2, $2, $4, 0,
					0, 0, 0, 0 );
			 TEST($$); 
		      } else {
			 yyerror(scanner, lParse, "Dimensions of DEFNULL arguments "
				 "are not compatible");
			 YYERROR;
		      }
		   } else if (FSTRCMP($1,"ARCTAN2(") == 0) {
		     if( TYPE($2) != DOUBLE ) $2 = New_Unary(lParse,  DOUBLE, 0, $2 );
		     if( TYPE($4) != DOUBLE ) $4 = New_Unary(lParse,  DOUBLE, 0, $4 );
		     if( Test_Dims( lParse,  $2, $4 ) ) {
			$$ = New_Func(lParse,  0, atan2_fct, 2, $2, $4, 0, 0, 0, 0, 0 );
			TEST($$); 
			if( SIZE($2)<SIZE($4) ) Copy_Dims( lParse,$$, $4);
		     } else {
			yyerror(scanner, lParse, "Dimensions of arctan2 arguments "
				"are not compatible");
			YYERROR;
		     }
		   } else if (FSTRCMP($1,"MIN(") == 0) {
		      PROMOTE( $2, $4 );
		      if( Test_Dims( lParse,  $2, $4 ) ) {
			$$ = New_Func(lParse,  0, min2_fct, 2, $2, $4, 0, 0, 0, 0, 0 );
			TEST($$);
			if( SIZE($2)<SIZE($4) ) Copy_Dims( lParse,$$, $4);
		      } else {
			yyerror(scanner, lParse, "Dimensions of min(a,b) arguments "
				"are not compatible");
			YYERROR;
		      }
		   } else if (FSTRCMP($1,"MAX(") == 0) {
		      PROMOTE( $2, $4 );
		      if( Test_Dims( lParse,  $2, $4 ) ) {
			$$ = New_Func(lParse,  0, max2_fct, 2, $2, $4, 0, 0, 0, 0, 0 );
			TEST($$);
			if( SIZE($2)<SIZE($4) ) Copy_Dims( lParse,$$, $4);
		      } else {
			yyerror(scanner, lParse, "Dimensions of max(a,b) arguments "
				"are not compatible");
			YYERROR;
		      }
		   } else if (FSTRCMP($1,"SETNULL(") == 0) {
		     if (OPER($2) != CONST_OP
			 || SIZE($2) != 1) {
		       yyerror(scanner, lParse, "SETNULL first argument must be a scalar constant");
		       YYERROR;
		     }
		     /* Make sure first arg is same type as second arg */
		     if ( TYPE($2) != TYPE($4) ) $2 = New_Unary(lParse,  TYPE($4), 0, $2 );
		     $$ = New_Func(lParse,  0, setnull_fct, 2, $4, $2, 0, 0, 0, 0, 0 );
		   } else if (FSTRCMP($1,"AXISELEM(") == 0) {  /* AXISELEM(V,n) */
		     if (OPER($4) != CONST_OP
			 || SIZE($4) != 1) {
		       yyerror(scanner, lParse, "AXISELEM second argument must be a scalar constant");
		       YYERROR;
		     }
		     if (OPER($2) == CONST_OP) {
		       long one = 1;
		       $$ = New_Const(lParse,  LONG, &one, sizeof(one) );
		     } else {
		       if ( TYPE($4) != LONG ) $4 = New_Unary(lParse, LONG, 0, $4);
		       $$ = New_Func(lParse, 0, axiselem_fct, 2, $2, $4, 0, 0, 0, 0, 0 );
		       TEST($$);
		       TYPE($$) = LONG;
		     }
		   } else if (FSTRCMP($1,"NAXES(") == 0) {  /* NAXES(V,n) */
		     if (OPER($4) != CONST_OP
			 || SIZE($4) != 1) {
		       yyerror(scanner, lParse, "NAXES second argument must be a scalar constant");
		       YYERROR;
		     }
		     if (OPER($2) == CONST_OP) { /* if V is constant, return 1 in every case */
		       long one = 1;
		       $$ = New_Const(lParse,  LONG, &one, sizeof(one) );
		     } else {                    /* determine now the dimension of the expression */
		       long iaxis;
		       int naxis;
		       if ( TYPE($4) != LONG ) $4 = New_Unary(lParse, LONG, 0, $4);
		       /* Since it is already constant, we can extract long value directly */
		       iaxis = (lParse->Nodes[$4].value.data.lng);
		       naxis = lParse->Nodes[$2].value.naxis;

		       if (iaxis == 0)          iaxis = naxis;   /* NAXIS(V,0) = NAXIS */
		       else if (iaxis <= naxis) iaxis = lParse->Nodes[$2].value.naxes[iaxis-1]; /* NAXIS(V,n) = NAXISn */
		       else                     iaxis = 1;       /* Out of bounds use 1 */

		       $$ = New_Const(lParse,  LONG, &iaxis, sizeof(iaxis) );
		       TEST($$);
		     }
		   } else if (FSTRCMP($1,"ARRAY(") == 0) {  /* NAXES(expr,n) */
		     $$ = New_Array(lParse, $2, $4);
		     TEST($$);
		   } else {
		      yyerror(scanner, lParse, "Function(expr,expr) not supported");
		      YYERROR;
		   }
                }
       | FUNCTION expr ',' expr ',' expr ',' expr ')'
                { 
		  if (FSTRCMP($1,"ANGSEP(") == 0) {
		    if( TYPE($2) != DOUBLE ) $2 = New_Unary(lParse,  DOUBLE, 0, $2 );
		    if( TYPE($4) != DOUBLE ) $4 = New_Unary(lParse,  DOUBLE, 0, $4 );
		    if( TYPE($6) != DOUBLE ) $6 = New_Unary(lParse,  DOUBLE, 0, $6 );
		    if( TYPE($8) != DOUBLE ) $8 = New_Unary(lParse,  DOUBLE, 0, $8 );
		    if( Test_Dims( lParse,  $2, $4 ) && Test_Dims( lParse,  $4, $6 ) && 
			Test_Dims( lParse,  $6, $8 ) ) {
		      $$ = New_Func(lParse,  0, angsep_fct, 4, $2, $4, $6, $8,0,0,0 );
		      TEST($$); 
		      if( SIZE($2)<SIZE($4) ) Copy_Dims( lParse,$$, $4);
		      if( SIZE($4)<SIZE($6) ) Copy_Dims( lParse,$$, $6);
		      if( SIZE($6)<SIZE($8) ) Copy_Dims( lParse,$$, $8);
		    } else {
		      yyerror(scanner, lParse, "Dimensions of ANGSEP arguments "
			      "are not compatible");
		      YYERROR;
		    }
		   } else {
		      yyerror(scanner, lParse, "Function(expr,expr,expr,expr) not supported");
		      YYERROR;
		   }
                }


       | GTIOVERLAP STRING ',' expr ',' expr ')'
                {  $$ = New_GTI(lParse, gtiover_fct,  $2, $4, $6, "*START*", "*STOP*");
                   TEST($$);                                        }
       | GTIOVERLAP STRING ',' expr ',' expr ',' STRING ',' STRING ')'
                {  $$ = New_GTI(lParse, gtiover_fct,  $2, $4, $6, $8, $10 );
                   TEST($$);                                        }

       | expr '[' expr ']'
                { $$ = New_Deref(lParse,  $1, 1, $3,  0,  0,  0,   0 ); TEST($$); }
       | expr '[' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 2, $3, $5,  0,  0,   0 ); TEST($$); }
       | expr '[' expr ',' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 3, $3, $5, $7,  0,   0 ); TEST($$); }
       | expr '[' expr ',' expr ',' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 4, $3, $5, $7, $9,   0 ); TEST($$); }
       | expr '[' expr ',' expr ',' expr ',' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 5, $3, $5, $7, $9, $11 ); TEST($$); }
       | INTCAST expr
		{ $$ = New_Unary(lParse,  LONG,   INTCAST, $2 );  TEST($$);  }
       | INTCAST bexpr
                { $$ = New_Unary(lParse,  LONG,   INTCAST, $2 );  TEST($$);  }
       | FLTCAST expr
		{ $$ = New_Unary(lParse,  DOUBLE, FLTCAST, $2 );  TEST($$);  }
       | FLTCAST bexpr
                { $$ = New_Unary(lParse,  DOUBLE, FLTCAST, $2 );  TEST($$);  }
       ;

bexpr:   BOOLEAN
                { $$ = New_Const(lParse,  BOOLEAN, &($1), sizeof(char) ); TEST($$); }
       | BCOLUMN
                { $$ = New_Column(lParse,  $1 ); TEST($$); }
       | BCOLUMN '{' expr '}'
                {
                  if( TYPE($3) != LONG
		      || OPER($3) != CONST_OP ) {
		     yyerror(scanner, lParse, "Offset argument must be a constant integer");
		     YYERROR;
		  }
                  $$ = New_Offset(lParse,  $1, $3 ); TEST($$);
                }
       | bits EQ bits
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, EQ,  $3 ); TEST($$);
		  SIZE($$) = 1;                                     }
       | bits NE bits
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, NE,  $3 ); TEST($$); 
		  SIZE($$) = 1;                                     }
       | bits LT bits
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, LT,  $3 ); TEST($$); 
		  SIZE($$) = 1;                                     }
       | bits LTE bits
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, LTE, $3 ); TEST($$); 
		  SIZE($$) = 1;                                     }
       | bits GT bits
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, GT,  $3 ); TEST($$); 
		  SIZE($$) = 1;                                     }
       | bits GTE bits
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, GTE, $3 ); TEST($$); 
		  SIZE($$) = 1;                                     }
       | expr GT expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  BOOLEAN, $1, GT,  $3 );
                  TEST($$);                                               }
       | expr LT expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  BOOLEAN, $1, LT,  $3 );
                  TEST($$);                                               }
       | expr GTE expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  BOOLEAN, $1, GTE, $3 );
                  TEST($$);                                               }
       | expr LTE expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  BOOLEAN, $1, LTE, $3 );
                  TEST($$);                                               }
       | expr '~' expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  BOOLEAN, $1, '~', $3 );
                  TEST($$);                                               }
       | expr EQ expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  BOOLEAN, $1, EQ,  $3 );
                  TEST($$);                                               }
       | expr NE expr
                { PROMOTE($1,$3); $$ = New_BinOp(lParse,  BOOLEAN, $1, NE,  $3 );
                  TEST($$);                                               }
       | sexpr EQ sexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, EQ,  $3 ); TEST($$);
                  SIZE($$) = 1; }
       | sexpr NE sexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, NE,  $3 ); TEST($$);
                  SIZE($$) = 1; }
       | sexpr GT sexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, GT,  $3 ); TEST($$);
                  SIZE($$) = 1; }
       | sexpr GTE sexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, GTE, $3 ); TEST($$);
                  SIZE($$) = 1; }
       | sexpr LT sexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, LT,  $3 ); TEST($$);
                  SIZE($$) = 1; }
       | sexpr LTE sexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, LTE, $3 ); TEST($$);
                  SIZE($$) = 1; }
       | bexpr AND bexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, AND, $3 ); TEST($$); }
       | bexpr OR bexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, OR,  $3 ); TEST($$); }
       | bexpr EQ bexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, EQ,  $3 ); TEST($$); }
       | bexpr NE bexpr
                { $$ = New_BinOp(lParse,  BOOLEAN, $1, NE,  $3 ); TEST($$); }

       | expr '=' expr ':' expr
                { PROMOTE($1,$3); PROMOTE($1,$5); PROMOTE($3,$5);
		  $3 = New_BinOp(lParse,  BOOLEAN, $3, LTE, $1 );
                  $5 = New_BinOp(lParse,  BOOLEAN, $1, LTE, $5 );
                  $$ = New_BinOp(lParse,  BOOLEAN, $3, AND, $5 );
                  TEST($$);                                         }

       | bexpr '?' bexpr ':' bexpr
                {
                  if( ! Test_Dims( lParse, $3,$5) ) {
                     yyerror(scanner, lParse, "Incompatible dimensions in '?:' arguments");
		     YYERROR;
                  }
                  $$ = New_Func(lParse,  0, ifthenelse_fct, 3, $3, $5, $1,
                                 0, 0, 0, 0 );
                  TEST($$);
                  if( SIZE($3)<SIZE($5) )  Copy_Dims( lParse,$$, $5);
                  if( ! Test_Dims( lParse, $1,$$) ) {
                     yyerror(scanner, lParse, "Incompatible dimensions in '?:' condition");
		     YYERROR;
                  }
                  if( SIZE($$)<SIZE($1) )  Copy_Dims( lParse,$$, $1);
                }

       | BFUNCTION expr ')'
                {
		   if (FSTRCMP($1,"ISNULL(") == 0) {
		      $$ = New_Func(lParse,  0, isnull_fct, 1, $2, 0, 0,
				     0, 0, 0, 0 );
		      TEST($$); 
                      /* Use expression's size, but return BOOLEAN */
		      TYPE($$) = BOOLEAN;
		   } else {
		      yyerror(scanner, lParse, "Boolean Function(expr) not supported");
		      YYERROR;
		   }
		}
       | BFUNCTION bexpr ')'
                {
		   if (FSTRCMP($1,"ISNULL(") == 0) {
		      $$ = New_Func(lParse,  0, isnull_fct, 1, $2, 0, 0,
				     0, 0, 0, 0 );
		      TEST($$); 
                      /* Use expression's size, but return BOOLEAN */
		      TYPE($$) = BOOLEAN;
		   } else {
		      yyerror(scanner, lParse, "Boolean Function(expr) not supported");
		      YYERROR;
		   }
		}
       | BFUNCTION sexpr ')'
                {
		   if (FSTRCMP($1,"ISNULL(") == 0) {
		      $$ = New_Func(lParse,  BOOLEAN, isnull_fct, 1, $2, 0, 0,
				     0, 0, 0, 0 );
		      TEST($$); 
		   } else {
		      yyerror(scanner, lParse, "Boolean Function(expr) not supported");
		      YYERROR;
		   }
		}
       | FUNCTION bexpr ',' bexpr ')'
                {
		   if (FSTRCMP($1,"DEFNULL(") == 0) {
		      if( SIZE($2)>=SIZE($4) && Test_Dims( lParse,  $2, $4 ) ) {
			 $$ = New_Func(lParse,  0, defnull_fct, 2, $2, $4, 0,
					0, 0, 0, 0 );
			 TEST($$); 
		      } else {
			 yyerror(scanner, lParse, "Dimensions of DEFNULL arguments are not compatible");
			 YYERROR;
		      }
		   } else {
		      yyerror(scanner, lParse, "Boolean Function(expr,expr) not supported");
		      YYERROR;
		   }
		}
       | BFUNCTION expr ',' expr ',' expr ')'
		{
		   if( TYPE($2) != DOUBLE ) $2 = New_Unary(lParse,  DOUBLE, 0, $2 );
		   if( TYPE($4) != DOUBLE ) $4 = New_Unary(lParse,  DOUBLE, 0, $4 );
		   if( TYPE($6) != DOUBLE ) $6 = New_Unary(lParse,  DOUBLE, 0, $6 );
		   if( ! (Test_Dims( lParse,  $2, $4 ) && Test_Dims( lParse,  $4, $6 ) ) ) {
		       yyerror(scanner, lParse, "Dimensions of NEAR arguments "
			       "are not compatible");
		       YYERROR;
		   } else {
		     if (FSTRCMP($1,"NEAR(") == 0) {
		       $$ = New_Func(lParse,  BOOLEAN, near_fct, 3, $2, $4, $6,
				      0, 0, 0, 0 );
		     } else {
		       yyerror(scanner, lParse, "Boolean Function not supported");
		       YYERROR;
		     }
		     TEST($$); 

		     if( SIZE($$)<SIZE($2) )  Copy_Dims( lParse,$$, $2);
		     if( SIZE($2)<SIZE($4) )  Copy_Dims( lParse,$$, $4);
		     if( SIZE($4)<SIZE($6) )  Copy_Dims( lParse,$$, $6);
		   }
		}
       | BFUNCTION expr ',' expr ',' expr ',' expr ',' expr ')'
	        {
		   if( TYPE($2) != DOUBLE ) $2 = New_Unary(lParse,  DOUBLE, 0, $2 );
		   if( TYPE($4) != DOUBLE ) $4 = New_Unary(lParse,  DOUBLE, 0, $4 );
		   if( TYPE($6) != DOUBLE ) $6 = New_Unary(lParse,  DOUBLE, 0, $6 );
		   if( TYPE($8) != DOUBLE ) $8 = New_Unary(lParse,  DOUBLE, 0, $8 );
		   if( TYPE($10)!= DOUBLE ) $10= New_Unary(lParse,  DOUBLE, 0, $10);
		   if( ! (Test_Dims( lParse,  $2, $4 ) && Test_Dims( lParse,  $4, $6 ) && 
			  Test_Dims( lParse,  $6, $8 ) && Test_Dims( lParse,  $8, $10 )) ) {
		     yyerror(scanner, lParse, "Dimensions of CIRCLE arguments "
			     "are not compatible");
		     YYERROR;
		   } else {
		     if (FSTRCMP($1,"CIRCLE(") == 0) {
		       $$ = New_Func(lParse,  BOOLEAN, circle_fct, 5, $2, $4, $6, $8,
				      $10, 0, 0 );
		     } else {
		       yyerror(scanner, lParse, "Boolean Function not supported");
		       YYERROR;
		     }
		     TEST($$); 
		     if( SIZE($$)<SIZE($2) )  Copy_Dims( lParse,$$, $2);
		     if( SIZE($2)<SIZE($4) )  Copy_Dims( lParse,$$, $4);
		     if( SIZE($4)<SIZE($6) )  Copy_Dims( lParse,$$, $6);
		     if( SIZE($6)<SIZE($8) )  Copy_Dims( lParse,$$, $8);
		     if( SIZE($8)<SIZE($10) ) Copy_Dims( lParse,$$, $10);
		   }
		}
       | BFUNCTION expr ',' expr ',' expr ',' expr ',' expr ',' expr ',' expr ')'
                {
		   if( TYPE($2) != DOUBLE ) $2 = New_Unary(lParse,  DOUBLE, 0, $2 );
		   if( TYPE($4) != DOUBLE ) $4 = New_Unary(lParse,  DOUBLE, 0, $4 );
		   if( TYPE($6) != DOUBLE ) $6 = New_Unary(lParse,  DOUBLE, 0, $6 );
		   if( TYPE($8) != DOUBLE ) $8 = New_Unary(lParse,  DOUBLE, 0, $8 );
		   if( TYPE($10)!= DOUBLE ) $10= New_Unary(lParse,  DOUBLE, 0, $10);
		   if( TYPE($12)!= DOUBLE ) $12= New_Unary(lParse,  DOUBLE, 0, $12);
		   if( TYPE($14)!= DOUBLE ) $14= New_Unary(lParse,  DOUBLE, 0, $14);
		   if( ! (Test_Dims( lParse,  $2, $4 ) && Test_Dims( lParse,  $4, $6 ) && 
			  Test_Dims( lParse,  $6, $8 ) && Test_Dims( lParse,  $8, $10 ) &&
			  Test_Dims( lParse, $10,$12 ) && Test_Dims( lParse, $12, $14 ) ) ) {
		     yyerror(scanner, lParse, "Dimensions of BOX or ELLIPSE arguments "
			     "are not compatible");
		     YYERROR;
		   } else {
		     if (FSTRCMP($1,"BOX(") == 0) {
		       $$ = New_Func(lParse,  BOOLEAN, box_fct, 7, $2, $4, $6, $8,
				      $10, $12, $14 );
		     } else if (FSTRCMP($1,"ELLIPSE(") == 0) {
		       $$ = New_Func(lParse,  BOOLEAN, elps_fct, 7, $2, $4, $6, $8,
				      $10, $12, $14 );
		     } else {
		       yyerror(scanner, lParse, "SAO Image Function not supported");
		       YYERROR;
		     }
		     TEST($$); 
		     if( SIZE($$)<SIZE($2) )  Copy_Dims( lParse,$$, $2);
		     if( SIZE($2)<SIZE($4) )  Copy_Dims( lParse,$$, $4);
		     if( SIZE($4)<SIZE($6) )  Copy_Dims( lParse,$$, $6);
		     if( SIZE($6)<SIZE($8) )  Copy_Dims( lParse,$$, $8);
		     if( SIZE($8)<SIZE($10) ) Copy_Dims( lParse,$$, $10);
		     if( SIZE($10)<SIZE($12) ) Copy_Dims( lParse,$$, $12);
		     if( SIZE($12)<SIZE($14) ) Copy_Dims( lParse,$$, $14);
		   }
		}

       | GTIFILTER ')'
                { /* Use defaults for all elements */
		   $$ = New_GTI(lParse, gtifilt_fct,  "", -99, -99, "*START*", "*STOP*" );
                   TEST($$);                                        }
       | GTIFILTER STRING ')'
                { /* Use defaults for all except filename */
		  $$ = New_GTI(lParse, gtifilt_fct,  $2, -99, -99, "*START*", "*STOP*" );
                   TEST($$);                                        }
       | GTIFILTER STRING ',' expr ')'
                {  $$ = New_GTI(lParse, gtifilt_fct,  $2, $4, -99, "*START*", "*STOP*" );
                   TEST($$);                                        }
       | GTIFILTER STRING ',' expr ',' STRING ',' STRING ')'
                {  $$ = New_GTI(lParse, gtifilt_fct,  $2, $4, -99, $6, $8 );
                   TEST($$);                                        }


       /* GTIFIND('myfile.gti', TIME_EXPR, 'START', 'STOP') */
       | GTIFIND ')'
                { /* Use defaults for all elements */
		   $$ = New_GTI(lParse, gtifind_fct,  "", -99, -99, "*START*", "*STOP*" );
                   TEST($$);                                        }
       | GTIFIND STRING ')'
                { /* Use defaults for all except filename */
		  $$ = New_GTI(lParse, gtifind_fct,  $2, -99, -99, "*START*", "*STOP*" );
                   TEST($$);                                        }
       | GTIFIND STRING ',' expr ')'
                {  $$ = New_GTI(lParse, gtifind_fct,  $2, $4, -99, "*START*", "*STOP*" );
                   TEST($$);                                        }
       | GTIFIND STRING ',' expr ',' STRING ',' STRING ')'
                {  $$ = New_GTI(lParse, gtifind_fct,  $2, $4, -99, $6, $8 );
                   TEST($$);                                        }


       | REGFILTER STRING ')'
                { /* Use defaults for all except filename */
                   $$ = New_REG(lParse,  $2, -99, -99, "" );
                   TEST($$);                                        }
       | REGFILTER STRING ',' expr ',' expr ')'
                {  $$ = New_REG(lParse,  $2, $4, $6, "" );
                   TEST($$);                                        }
       | REGFILTER STRING ',' expr ',' expr ',' STRING ')'
                {  $$ = New_REG(lParse,  $2, $4, $6, $8 );
                   TEST($$);                                        }

       | bexpr '[' expr ']'
                { $$ = New_Deref(lParse,  $1, 1, $3,  0,  0,  0,   0 ); TEST($$); }
       | bexpr '[' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 2, $3, $5,  0,  0,   0 ); TEST($$); }
       | bexpr '[' expr ',' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 3, $3, $5, $7,  0,   0 ); TEST($$); }
       | bexpr '[' expr ',' expr ',' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 4, $3, $5, $7, $9,   0 ); TEST($$); }
       | bexpr '[' expr ',' expr ',' expr ',' expr ',' expr ']'
                { $$ = New_Deref(lParse,  $1, 5, $3, $5, $7, $9, $11 ); TEST($$); }
       | NOT bexpr
                { $$ = New_Unary(lParse,  BOOLEAN, NOT, $2 ); TEST($$); }
       | '(' bexpr ')'
                { $$ = $2; }
       ;

sexpr:   STRING
                { $$ = New_Const(lParse,  STRING, $1, strlen($1)+1 ); TEST($$);
                  SIZE($$) = strlen($1); }
       | SCOLUMN
                { $$ = New_Column(lParse,  $1 ); TEST($$); }
       | SCOLUMN '{' expr '}'
                {
                  if( TYPE($3) != LONG
		      || OPER($3) != CONST_OP ) {
		     yyerror(scanner, lParse, "Offset argument must be a constant integer");
		     YYERROR;
		  }
                  $$ = New_Offset(lParse,  $1, $3 ); TEST($$);
                }
       | SNULLREF
                { $$ = New_Func(lParse,  STRING, null_fct, 0, 0, 0, 0, 0, 0, 0, 0 ); }
       | '(' sexpr ')'
                { $$ = $2; }
       | sexpr '+' sexpr
                { 
		  if (SIZE($1)+SIZE($3) >= MAX_STRLEN) {
		    yyerror(scanner, lParse, "Combined string size exceeds " MAX_STRLEN_S " characters");
		    YYERROR;
		  }
		  $$ = New_BinOp(lParse,  STRING, $1, '+', $3 );  TEST($$);
		  SIZE($$) = SIZE($1) + SIZE($3);
		}
       | bexpr '?' sexpr ':' sexpr
                {
		  int outSize;
                  if( SIZE($1)!=1 ) {
                     yyerror(scanner, lParse, "Cannot have a vector string column");
		     YYERROR;
                  }
		  /* Since the output can be calculated now, as a constant
		     scalar, we must precalculate the output size, in
		     order to avoid an overflow. */
		  outSize = SIZE($3);
		  if (SIZE($5) > outSize) outSize = SIZE($5);
                  $$ = New_FuncSize(lParse,  0, ifthenelse_fct, 3, $3, $5, $1,
				     0, 0, 0, 0, outSize);
		  
                  TEST($$);
                  if( SIZE($3)<SIZE($5) )  Copy_Dims( lParse,$$, $5);
                }

       | FUNCTION sexpr ',' sexpr ')'
                { 
		  if (FSTRCMP($1,"DEFNULL(") == 0) {
		     int outSize;
		     /* Since the output can be calculated now, as a constant
			scalar, we must precalculate the output size, in
			order to avoid an overflow. */
		     outSize = SIZE($2);
		     if (SIZE($4) > outSize) outSize = SIZE($4);
		     
		     $$ = New_FuncSize(lParse,  0, defnull_fct, 2, $2, $4, 0,
					0, 0, 0, 0, outSize );
		     TEST($$); 
		     if( SIZE($4)>SIZE($2) ) SIZE($$) = SIZE($4);
		  } else {
		     yyerror(scanner, lParse, "Function(string,string) not supported");
		     YYERROR;
		  }
		}
       | FUNCTION sexpr ',' expr ',' expr ')'
                { 
		  if (FSTRCMP($1,"STRMID(") == 0) {
		    int len;
		    if( TYPE($4) != LONG || SIZE($4) != 1 ||
			TYPE($6) != LONG || SIZE($6) != 1) {
		      yyerror(scanner, lParse, "When using STRMID(S,P,N), P and N must be integers (and not vector columns)");
		      YYERROR;
		    }
		    if (OPER($6) == CONST_OP) {
		      /* Constant value: use that directly */
		      len = (lParse->Nodes[$6].value.data.lng);
		    } else {
		      /* Variable value: use the maximum possible (from $2) */
		      len = SIZE($2);
		    }
		    if (len <= 0 || len >= MAX_STRLEN) {
		      yyerror(scanner, lParse, "STRMID(S,P,N), N must be 1-" MAX_STRLEN_S);
		      YYERROR;
		    }
		    $$ = New_FuncSize(lParse,  0, strmid_fct, 3, $2, $4,$6,0,0,0,0,len);
		    TEST($$);
		  } else {
		     yyerror(scanner, lParse, "Function(string,expr,expr) not supported");
		     YYERROR;
		  }
		}

	;

%%

/*************************************************************************/
/*  Start of "New" routines which build the expression Nodal structure   */
/*************************************************************************/

static int Alloc_Node( ParseData *lParse )
{
                      /* Use this for allocation to guarantee *Nodes */
   Node *newNodePtr;  /* survives on failure, making it still valid  */
                      /* while working our way out of this error     */

   if( lParse->nNodes == lParse->nNodesAlloc ) {
      if( lParse->Nodes ) {
	 lParse->nNodesAlloc += lParse->nNodesAlloc;
	 newNodePtr = (Node *)realloc( lParse->Nodes,
				       sizeof(Node)*lParse->nNodesAlloc );
      } else {
	 lParse->nNodesAlloc = 100;
	 newNodePtr = (Node *)malloc ( sizeof(Node)*lParse->nNodesAlloc );
      }	 

      if( newNodePtr ) {
	 lParse->Nodes = newNodePtr;
      } else {
	 lParse->status = MEMORY_ALLOCATION;
	 return( -1 );
      }
   }

   return ( lParse->nNodes++ );
}

static void Free_Last_Node( ParseData *lParse )
{
   if( lParse->nNodes ) lParse->nNodes--;
}

static int New_Const( ParseData *lParse, int returnType, void *value, long len )
{
   Node *this;
   int n;

   n = Alloc_Node(lParse);
   if( n>=0 ) {
      this             = lParse->Nodes + n;
      this->operation  = CONST_OP;             /* Flag a constant */
      this->DoOp       = NULL;
      this->nSubNodes  = 0;
      this->type       = returnType;
      memcpy( &(this->value.data), value, len );
      this->value.undef = NULL;
      this->value.nelem = 1;
      this->value.naxis = 1;
      this->value.naxes[0] = 1;
   }
   return(n);
}

static int New_Column( ParseData *lParse, int ColNum )
{
   Node *this;
   int  n, i;

   n = Alloc_Node(lParse);
   if( n>=0 ) {
      this              = lParse->Nodes + n;
      this->operation   = -ColNum;
      this->DoOp        = NULL;
      this->nSubNodes   = 0;
      this->type        = lParse->varData[ColNum].type;
      this->value.nelem = lParse->varData[ColNum].nelem;
      this->value.naxis = lParse->varData[ColNum].naxis;
      for( i=0; i<lParse->varData[ColNum].naxis; i++ )
	 this->value.naxes[i] = lParse->varData[ColNum].naxes[i];
   }
   return(n);
}

static int New_Offset( ParseData *lParse, int ColNum, int offsetNode )
{
   Node *this;
   int  n, i, colNode;

   colNode = New_Column( lParse, ColNum );
   if( colNode<0 ) return(-1);

   n = Alloc_Node(lParse);
   if( n>=0 ) {
      this              = lParse->Nodes + n;
      this->operation   = '{';
      this->DoOp        = Do_Offset;
      this->nSubNodes   = 2;
      this->SubNodes[0] = colNode;
      this->SubNodes[1] = offsetNode;
      this->type        = lParse->varData[ColNum].type;
      this->value.nelem = lParse->varData[ColNum].nelem;
      this->value.naxis = lParse->varData[ColNum].naxis;
      for( i=0; i<lParse->varData[ColNum].naxis; i++ )
	 this->value.naxes[i] = lParse->varData[ColNum].naxes[i];
   }
   return(n);
}

static int New_Unary( ParseData *lParse, int returnType, int Op, int Node1 )
{
   Node *this, *that;
   int  i,n;

   if( Node1<0 ) return(-1);
   that = lParse->Nodes + Node1;

   if( !Op ) Op = returnType;

   if( (Op==DOUBLE || Op==FLTCAST) && that->type==DOUBLE  ) return( Node1 );
   if( (Op==LONG   || Op==INTCAST) && that->type==LONG    ) return( Node1 );
   if( (Op==BOOLEAN              ) && that->type==BOOLEAN ) return( Node1 );
   
   n = Alloc_Node(lParse);
   if( n>=0 ) {
      this              = lParse->Nodes + n;
      this->operation   = Op;
      this->DoOp        = Do_Unary;
      this->nSubNodes   = 1;
      this->SubNodes[0] = Node1;
      this->type        = returnType;

      that              = lParse->Nodes + Node1; /* Reset in case .Nodes mv'd */
      this->value.nelem = that->value.nelem;
      this->value.naxis = that->value.naxis;
      for( i=0; i<that->value.naxis; i++ )
	 this->value.naxes[i] = that->value.naxes[i];

      if( that->operation==CONST_OP ) this->DoOp( lParse, this );
   }
   return( n );
}

static int New_BinOp( ParseData *lParse, int returnType, int Node1, int Op, int Node2 )
{
   Node *this,*that1,*that2;
   int  n,i,constant;

   if( Node1<0 || Node2<0 ) return(-1);

   n = Alloc_Node(lParse);
   if( n>=0 ) {
      this             = lParse->Nodes + n;
      this->operation  = Op;
      this->nSubNodes  = 2;
      this->SubNodes[0]= Node1;
      this->SubNodes[1]= Node2;
      this->type       = returnType;

      that1            = lParse->Nodes + Node1;
      that2            = lParse->Nodes + Node2;
      constant         = (that1->operation==CONST_OP
                          && that2->operation==CONST_OP);
      if( that1->type!=STRING && that1->type!=BITSTR )
	if( !Test_Dims( lParse, Node1, Node2 ) ) {
	    Free_Last_Node(lParse);
	    yyerror(0, lParse, "Array sizes/dims do not match for binary operator");
	    return(-1);
	 }
      if( that1->value.nelem == 1 ) that1 = that2;

      this->value.nelem = that1->value.nelem;
      this->value.naxis = that1->value.naxis;
      for( i=0; i<that1->value.naxis; i++ )
	 this->value.naxes[i] = that1->value.naxes[i];

      if ( Op == ACCUM && that1->type == BITSTR ) {
	/* ACCUM is rank-reducing on bit strings */
	this->value.nelem = 1;
	this->value.naxis = 1;
	this->value.naxes[0] = 1;
      }

      /*  Both subnodes should be of same time  */
      switch( that1->type ) {
      case BITSTR:  this->DoOp = Do_BinOp_bit;  break;
      case STRING:  this->DoOp = Do_BinOp_str;  break;
      case BOOLEAN: this->DoOp = Do_BinOp_log;  break;
      case LONG:    this->DoOp = Do_BinOp_lng;  break;
      case DOUBLE:  this->DoOp = Do_BinOp_dbl;  break;
      }
      if( constant ) this->DoOp( lParse, this );
   }
   return( n );
}

static int New_Func( ParseData *lParse,
		     int returnType, funcOp Op, int nNodes,
		     int Node1, int Node2, int Node3, int Node4, 
		     int Node5, int Node6, int Node7 )
{
  return New_FuncSize(lParse,
		      returnType, Op, nNodes,
		      Node1, Node2, Node3, Node4, 
		      Node5, Node6, Node7, 0);
}

static int New_FuncSize( ParseData *lParse,
			 int returnType, funcOp Op, int nNodes,
			 int Node1, int Node2, int Node3, int Node4, 
			 int Node5, int Node6, int Node7, int Size )
/* If returnType==0 , use Node1's type and vector sizes as returnType, */
/* else return a single value of type returnType                       */
{
   Node *this, *that;
   int  i,n,constant;

   if( Node1<0 || Node2<0 || Node3<0 || Node4<0 || 
       Node5<0 || Node6<0 || Node7<0 ) return(-1);

   n = Alloc_Node(lParse);
   if( n>=0 ) {
      this              = lParse->Nodes + n;
      this->operation   = (int)Op;
      this->DoOp        = Do_Func;
      this->nSubNodes   = nNodes;
      this->SubNodes[0] = Node1;
      this->SubNodes[1] = Node2;
      this->SubNodes[2] = Node3;
      this->SubNodes[3] = Node4;
      this->SubNodes[4] = Node5;
      this->SubNodes[5] = Node6;
      this->SubNodes[6] = Node7;
      i = constant = nNodes;    /* Functions with zero params are not const */
      if (Op == poirnd_fct) constant = 0; /* Nor is Poisson deviate */

      while( i-- )
	constant = ( constant && OPER(this->SubNodes[i]) == CONST_OP );
      
      if( returnType ) {
	 this->type           = returnType;
	 this->value.nelem    = 1;
	 this->value.naxis    = 1;
	 this->value.naxes[0] = 1;
      } else {
	 that              = lParse->Nodes + Node1;
	 this->type        = that->type;
	 this->value.nelem = that->value.nelem;
	 this->value.naxis = that->value.naxis;
	 for( i=0; i<that->value.naxis; i++ )
	    this->value.naxes[i] = that->value.naxes[i];
      }
      /* Force explicit size before evaluating */
      if (Size > 0) this->value.nelem = Size;

      if( constant ) this->DoOp( lParse, this );
   }
   return( n );
}

static int New_Deref( ParseData *lParse, int Var,  int nDim,
		      int Dim1, int Dim2, int Dim3, int Dim4, int Dim5 )
{
   int n, idx, constant;
   long elem=0;
   Node *this, *theVar, *theDim[MAXDIMS];

   if( Var<0 || Dim1<0 || Dim2<0 || Dim3<0 || Dim4<0 || Dim5<0 ) return(-1);

   theVar = lParse->Nodes + Var;
   if( theVar->operation==CONST_OP || theVar->value.nelem==1 ) {
      yyerror(0, lParse, "Cannot index a scalar value");
      return(-1);
   }

   n = Alloc_Node(lParse);
   if( n>=0 ) {
      this              = lParse->Nodes + n;
      this->nSubNodes   = nDim+1;
      theVar            = lParse->Nodes + (this->SubNodes[0]=Var);
      theDim[0]         = lParse->Nodes + (this->SubNodes[1]=Dim1);
      theDim[1]         = lParse->Nodes + (this->SubNodes[2]=Dim2);
      theDim[2]         = lParse->Nodes + (this->SubNodes[3]=Dim3);
      theDim[3]         = lParse->Nodes + (this->SubNodes[4]=Dim4);
      theDim[4]         = lParse->Nodes + (this->SubNodes[5]=Dim5);
      constant          = theVar->operation==CONST_OP;
      for( idx=0; idx<nDim; idx++ )
	 constant = (constant && theDim[idx]->operation==CONST_OP);

      for( idx=0; idx<nDim; idx++ )
	 if( theDim[idx]->value.nelem>1 ) {
	    Free_Last_Node(lParse);
	    yyerror(0, lParse, "Cannot use an array as an index value");
	    return(-1);
	 } else if( theDim[idx]->type!=LONG ) {
	    Free_Last_Node(lParse);
	    yyerror(0, lParse, "Index value must be an integer type");
	    return(-1);
	 }

      this->operation   = '[';
      this->DoOp        = Do_Deref;
      this->type        = theVar->type;

      if( theVar->value.naxis == nDim ) { /* All dimensions specified */
	 this->value.nelem    = 1;
	 this->value.naxis    = 1;
	 this->value.naxes[0] = 1;
      } else if( nDim==1 ) { /* Dereference only one dimension */
	 elem=1;
	 this->value.naxis = theVar->value.naxis-1;
	 for( idx=0; idx<this->value.naxis; idx++ ) {
	    elem *= ( this->value.naxes[idx] = theVar->value.naxes[idx] );
	 }
	 this->value.nelem = elem;
      } else {
	 Free_Last_Node(lParse);
	 yyerror(0, lParse, "Must specify just one or all indices for vector");
	 return(-1);
      }
      if( constant ) this->DoOp( lParse, this );
   }
   return(n);
}

extern int fits_parser_yyGetVariable( ParseData *lParse, char *varName, YYSTYPE *varVal );

static int New_GTI( ParseData *lParse, funcOp Op, char *fname, int Node1, int Node2, char *start, char *stop )
{
   fitsfile *fptr;
   Node *this, *that0, *that1, *that2;
   int  type,i,n, startCol, stopCol, Node0;
   int  hdutype, hdunum, evthdu, samefile, extvers, movetotype, tstat;
   char extname[100];
   long nrows;
   double timeZeroI[2], timeZeroF[2], dt, timeSpan;
   char xcol[20], xexpr[20];
   YYSTYPE colVal;

   if( (Op == gtifilt_fct || Op == gtifind_fct) && Node1==-99 ) {
      type = fits_parser_yyGetVariable( lParse,  "TIME", &colVal );
      if( type==COLUMN ) {
	 Node1 = New_Column( lParse, (int)colVal.lng );
      } else {
	 yyerror(0, lParse, "Could not build TIME column for GTIFILTER/GTIFIND");
	 return(-1);
      }
   }

   if (Op == gtiover_fct) {
     if (Node1 == -99 || Node2 == -99) {
       yyerror(0, lParse, "startExpr and stopExpr values must be defined for GTIOVERLAP");
       return(-1);
     }
     /* Also case TIME_STOP to double precision */
     Node2 = New_Unary( lParse, DOUBLE, 0, Node2 );
     if (Node2 < 0) return(-1);

   }

   /* Type cast TIME to double precision */
   Node1 = New_Unary( lParse, DOUBLE, 0, Node1 );
   Node0 = Alloc_Node(lParse); /* This will hold the START/STOP times */
   if( Node1<0 || Node0<0 ) return(-1);

   /*  Record current HDU number in case we need to move within this file  */

   fptr = lParse->def_fptr;
   ffghdn( fptr, &evthdu );

   /*  Look for TIMEZERO keywords in current extension  */

   tstat = 0;
   if( ffgkyd( fptr, "TIMEZERO", timeZeroI, NULL, &tstat ) ) {
      tstat = 0;
      if( ffgkyd( fptr, "TIMEZERI", timeZeroI, NULL, &tstat ) ) {
	 timeZeroI[0] = timeZeroF[0] = 0.0;
      } else if( ffgkyd( fptr, "TIMEZERF", timeZeroF, NULL, &tstat ) ) {
	 timeZeroF[0] = 0.0;
      }
   } else {
      timeZeroF[0] = 0.0;
   }

   /*  Resolve filename parameter  */

   switch( fname[0] ) {
   case '\0':
      samefile = 1;
      hdunum = 1;
      break;
   case '[':
      samefile = 1;
      i = 1;
      while( fname[i] != '\0' && fname[i] != ']' ) i++;
      if( fname[i] ) {
	 fname[i] = '\0';
	 fname++;
	 ffexts( fname, &hdunum, extname, &extvers, &movetotype,
		 xcol, xexpr, &lParse->status );
         if( *extname ) {
	    ffmnhd( fptr, movetotype, extname, extvers, &lParse->status );
	    ffghdn( fptr, &hdunum );
	 } else if( hdunum ) {
	    ffmahd( fptr, ++hdunum, &hdutype, &lParse->status );
	 } else if( !lParse->status ) {
	    yyerror(0, lParse, "Cannot use primary array for GTI filter");
	    return( -1 );
	 }
      } else {
	 yyerror(0, lParse, "File extension specifier lacks closing ']'");
	 return( -1 );
      }
      break;
   case '+':
      samefile = 1;
      hdunum = atoi( fname ) + 1;
      if( hdunum>1 )
	 ffmahd( fptr, hdunum, &hdutype, &lParse->status );
      else {
	 yyerror(0, lParse, "Cannot use primary array for GTI filter / GTIFIND");
	 return( -1 );
      }
      break;
   default:
      samefile = 0;
      if( ! ffopen( &fptr, fname, READONLY, &lParse->status ) )
	 ffghdn( fptr, &hdunum );
      break;
   }
   if( lParse->status ) return(-1);

   /*  If at primary, search for GTI extension  */

   if( hdunum==1 ) {
      while( 1 ) {
	 hdunum++;
	 if( ffmahd( fptr, hdunum, &hdutype, &lParse->status ) ) break;
	 if( hdutype==IMAGE_HDU ) continue;
	 tstat = 0;
	 if( ffgkys( fptr, "EXTNAME", extname, NULL, &tstat ) ) continue;
	 ffupch( extname );
	 if( strstr( extname, "GTI" ) ) break;
      }
      if( lParse->status ) {
	 if( lParse->status==END_OF_FILE )
	    yyerror(0, lParse, "GTI extension not found in this file");
	 return(-1);
      }
   }

   /*  Locate START/STOP Columns  */

   ffgcno( fptr, CASEINSEN, start, &startCol, &lParse->status );
   ffgcno( fptr, CASEINSEN, stop,  &stopCol,  &lParse->status );
   if( lParse->status ) return(-1);

   /*  Look for TIMEZERO keywords in GTI extension  */

   tstat = 0;
   if( ffgkyd( fptr, "TIMEZERO", timeZeroI+1, NULL, &tstat ) ) {
      tstat = 0;
      if( ffgkyd( fptr, "TIMEZERI", timeZeroI+1, NULL, &tstat ) ) {
	 timeZeroI[1] = timeZeroF[1] = 0.0;
      } else if( ffgkyd( fptr, "TIMEZERF", timeZeroF+1, NULL, &tstat ) ) {
	 timeZeroF[1] = 0.0;
      }
   } else {
      timeZeroF[1] = 0.0;
   }

   n = Alloc_Node(lParse);
   if( n >= 0 ) {
      this                 = lParse->Nodes + n;
      this->SubNodes[1]    = Node1;
      this->operation      = (int) Op;
      if (Op == gtifilt_fct) {
	this->nSubNodes      = 2;
	this->DoOp           = Do_GTI;
	this->type           = BOOLEAN;
      } else if (Op == gtifind_fct) {
	this->nSubNodes      = 2;
	this->DoOp           = Do_GTI;
	this->type           = LONG;
      } else {
	this->nSubNodes      = 3;
	this->DoOp           = Do_GTI_Over;
	this->type           = DOUBLE;
      }
      that1                = lParse->Nodes + Node1;
      this->value.nelem    = that1->value.nelem;
      this->value.naxis    = that1->value.naxis;
      for( i=0; i < that1->value.naxis; i++ )
	 this->value.naxes[i] = that1->value.naxes[i];
      if (Op == gtiover_fct) {
	this->SubNodes[2]  = Node2;
	that2 = lParse->Nodes + Node2;
	if (that1->value.nelem != that2->value.nelem) {
	  yyerror(0, lParse, "Dimensions of TIME and TIME_STOP must match for GTIOVERLAP");
	  return(-1);
	}
      }

      /* Init START/STOP node to be treated as a "constant" */

      this->SubNodes[0]    = Node0;
      that0                = lParse->Nodes + Node0;
      that0->operation     = CONST_OP;
      that0->DoOp          = NULL;
      that0->value.data.ptr= NULL;

      /*  Read in START/STOP times  */

      if( ffgkyj( fptr, "NAXIS2", &nrows, NULL, &lParse->status ) )
	 return(-1);
      that0->value.nelem = nrows;
      if( nrows ) {
	 double *startptr = 0, *stopptr = 0;

	 /* We are allocating storage for both START and STOP with one pointer
	    and stop is stored at dblptr+nrows, we will use aliases below to
	    make this easier to read */
	 that0->value.data.dblptr = (double*)malloc( 2*nrows*sizeof(double) );
	 if( !that0->value.data.dblptr ) {
	    lParse->status = MEMORY_ALLOCATION;
	    return(-1);
	 }
	 startptr = that0->value.data.dblptr;
	 stopptr  = that0->value.data.dblptr + nptr;
	 
	 ffgcvd( fptr, startCol, 1L, 1L, nrows, 0.0,
		 startptr, &i, &lParse->status );
	 ffgcvd( fptr, stopCol, 1L, 1L, nrows, 0.0,
		 stopptr, &i, &lParse->status );
	 if( lParse->status ) {
	    free( that0->value.data.dblptr );
	    return(-1);
	 }

	 /*  Test for fully time-ordered GTI... both START && STOP  */

	 that0->type = 1; /*  Assume yes  */
	 i = nrows;
	 while( --i ) { /* the following are failure conditions for GTI ordering */
	   if( (startptr[i] > stopptr [i]) ||      /* START{i} > STOP{i} */
	       (starptr[i]  < stopptr[i-1]) ) {     /* START{i} < STOP{i-1} */
	     that0->type = 0;
	     break;
	   }
	 }

	 /* GTIOVERLAP() requires ordered GTI */
	 if (that0->type != 1 && Op == gtiover_fct) {
	   yyerror(0, lParse, "Input GTI must be time-ordered for GTIOVERLAP");
	   return(-1);
	 }
	 
	 /*  Handle TIMEZERO offset, if any  */
	 
	 dt = (timeZeroI[1] - timeZeroI[0]) + (timeZeroF[1] - timeZeroF[0]);
	 timeSpan = stopptr[nrows-1] - startptr[0];
	 if (timeSpan == 0) timeSpan = 1.0;
	 
	 if( fabs( dt / timeSpan ) > 1e-12 ) {
	   for( i=0; i<nrows; i++ ) {
	     startptr[i] += dt;
	     stopptr[i]  += dt;
	 }
      }
      /* If Node1 is constant (gtifilt_fct) or
	 Node1 and Node2 are constant (gtiover_fct), then evaluate now */
      if( OPER(Node1)==CONST_OP && (Op == gtifilt_fct || OPER(Node2)==CONST_OP)) {
	this->DoOp( lParse, this );
      }
   }

   if( samefile )
      ffmahd( fptr, evthdu, &hdutype, &lParse->status );
   else
      ffclos( fptr, &lParse->status );

   return( n );
}

static int New_REG( ParseData *lParse, char *fname, int NodeX, int NodeY, char *colNames )
{
   Node *this, *that0;
   int  type, n, Node0;
   int  Xcol, Ycol, tstat;
   WCSdata wcs;
   SAORegion *Rgn;
   char *cX, *cY;
   YYSTYPE colVal;

   if( NodeX==-99 ) {
      type = fits_parser_yyGetVariable( lParse,  "X", &colVal );
      if( type==COLUMN ) {
	 NodeX = New_Column( lParse, (int)colVal.lng );
      } else {
	 yyerror(0, lParse, "Could not build X column for REGFILTER");
	 return(-1);
      }
   }
   if( NodeY==-99 ) {
      type = fits_parser_yyGetVariable( lParse, "Y", &colVal );
      if( type==COLUMN ) {
 	 NodeY = New_Column( lParse, (int)colVal.lng );
      } else {
	 yyerror(0, lParse, "Could not build Y column for REGFILTER");
	 return(-1);
      }
   }
   NodeX = New_Unary( lParse, DOUBLE, 0, NodeX );
   NodeY = New_Unary( lParse, DOUBLE, 0, NodeY );
   Node0 = Alloc_Node(lParse); /* This will hold the Region Data */
   if( NodeX<0 || NodeY<0 || Node0<0 ) return(-1);

   if( ! (Test_Dims( lParse, NodeX, NodeY ) ) ) {
     yyerror(0, lParse, "Dimensions of REGFILTER arguments are not compatible");
     return (-1);
   }

   n = Alloc_Node(lParse);
   if( n >= 0 ) {
      this                 = lParse->Nodes + n;
      this->nSubNodes      = 3;
      this->SubNodes[0]    = Node0;
      this->SubNodes[1]    = NodeX;
      this->SubNodes[2]    = NodeY;
      this->operation      = (int)regfilt_fct;
      this->DoOp           = Do_REG;
      this->type           = BOOLEAN;
      this->value.nelem    = 1;
      this->value.naxis    = 1;
      this->value.naxes[0] = 1;
      
      Copy_Dims(lParse, n, NodeX);
      if( SIZE(NodeX)<SIZE(NodeY) )  Copy_Dims(lParse, n, NodeY);

      /* Init Region node to be treated as a "constant" */

      that0                = lParse->Nodes + Node0;
      that0->operation     = CONST_OP;
      that0->DoOp          = NULL;

      /*  Identify what columns to use for WCS information  */

      Xcol = Ycol = 0;
      if( *colNames ) {
	 /*  Use the column names in this string for WCS info  */
	 while( *colNames==' ' ) colNames++;
	 cX = cY = colNames;
	 while( *cY && *cY!=' ' && *cY!=',' ) cY++;
	 if( *cY )
	    *(cY++) = '\0';
	 while( *cY==' ' ) cY++;
	 if( !*cY ) {
	    yyerror(0, lParse, "Could not extract valid pair of column names from REGFILTER");
	    Free_Last_Node(lParse);
	    return( -1 );
	 }
	 fits_get_colnum( lParse->def_fptr, CASEINSEN, cX, &Xcol,
			  &lParse->status );
	 fits_get_colnum( lParse->def_fptr, CASEINSEN, cY, &Ycol,
			  &lParse->status );
	 if( lParse->status ) {
	    yyerror(0, lParse, "Could not locate columns indicated for WCS info");
	    Free_Last_Node(lParse);
	    return( -1 );
	 }

      } else {
	 /*  Try to find columns used in X/Y expressions  */
	 Xcol = Locate_Col( lParse, lParse->Nodes + NodeX );
	 Ycol = Locate_Col( lParse, lParse->Nodes + NodeY );
	 if( Xcol<0 || Ycol<0 ) {
	    yyerror(0, lParse, "Found multiple X/Y column references in REGFILTER");
	    Free_Last_Node(lParse);
	    return( -1 );
	 }
      }

      /*  Now, get the WCS info, if it exists, from the indicated columns  */
      wcs.exists = 0;
      if( Xcol>0 && Ycol>0 ) {
	 tstat = 0;
	 ffgtcs( lParse->def_fptr, Xcol, Ycol,
		 &wcs.xrefval, &wcs.yrefval,
		 &wcs.xrefpix, &wcs.yrefpix,
		 &wcs.xinc,    &wcs.yinc,
		 &wcs.rot,      wcs.type,
		 &tstat );
	 if( tstat==NO_WCS_KEY ) {
	    wcs.exists = 0;
	 } else if( tstat ) {
	    lParse->status = tstat;
	    Free_Last_Node(lParse);
	    return( -1 );
	 } else {
	    wcs.exists = 1;
	 }
      }

      /*  Read in Region file  */

      fits_read_rgnfile( fname, &wcs, &Rgn, &lParse->status );
      if( lParse->status ) {
	 Free_Last_Node(lParse);
	 return( -1 );
      }

      that0->value.data.ptr = Rgn;

      if( OPER(NodeX)==CONST_OP && OPER(NodeY)==CONST_OP )
	 this->DoOp( lParse, this );
   }

   return( n );
}

static int New_Vector( ParseData *lParse, int subNode )
{
   Node *this, *that;
   int n;

   n = Alloc_Node(lParse);
   if( n >= 0 ) {
      this              = lParse->Nodes + n;
      that              = lParse->Nodes + subNode;
      this->type        = that->type;
      this->nSubNodes   = 1;
      this->SubNodes[0] = subNode;
      this->operation   = '{';
      this->DoOp        = Do_Vector;
   }

   return( n );
}

static int Close_Vec( ParseData *lParse, int vecNode )
{
   Node *this;
   int n, nelem=0;

   this = lParse->Nodes + vecNode;
   for( n=0; n < this->nSubNodes; n++ ) {
      if( TYPE( this->SubNodes[n] ) != this->type ) {
	 this->SubNodes[n] = New_Unary( lParse, this->type, 0, this->SubNodes[n] );
	 if( this->SubNodes[n]<0 ) return(-1);
      }
      nelem += SIZE(this->SubNodes[n]);
   }
   this->value.naxis    = 1;
   this->value.nelem    = nelem;
   this->value.naxes[0] = nelem;

   return( vecNode );
}

static int New_Array( ParseData *lParse, int valueNode, int dimNode )
{
  Node *dims;
  long naxis, nelem;
  long naxes[MAXDIMS];
  Node *this;
  int  n,i;

   if( valueNode<0 || dimNode<0 ) return(-1);

   /* Check that dimensions are {a,b,c,d}
        - vector
	- every element is constant integer
	- 5 or fewer dimensions 
   */

   dims = &(lParse->Nodes[dimNode]);
   for (i=0; i<MAXDIMS; i++) naxes[i] = 1;

   if (OPER(dimNode) == CONST_OP) { /* ARRAY(V,n) is a constant integer */
     if ( TYPE(dimNode) != LONG ) dimNode = New_Unary(lParse, LONG, 0, dimNode);
     if (dimNode < 0) return (-1);
     naxis = 1;
     naxes[0] = lParse->Nodes[dimNode].value.data.lng;

   } else if (OPER(dimNode) == '{') { /* ARRAY(V,{a,b,c,d,e}) up to 5 dimensions */
     if (dims->nSubNodes > MAXDIMS) {
       yyerror(0, lParse, "ARRAY(V,{...}) number of dimensions must not exceed 5");
       return (-1);
     }
     naxis = dims->nSubNodes;
     for (i=0; i<dims->nSubNodes; i++) {
       if ( TYPE(dims->SubNodes[i]) != LONG ) {
	 dims->SubNodes[i] = New_Unary(lParse, LONG, 0, dims->SubNodes[i]);
	 if (dims->SubNodes[i] < 0) return (-1);
       }
       naxes[i] = lParse->Nodes[ dims->SubNodes[i] ].value.data.lng;
     }
   } else {
     yyerror(0, lParse, "ARRAY(V,dims) dims must be either integer or const vector");
     return (-1);
   }

   nelem = 1;
   for (i=0; i<naxis; i++) {
     if (naxes[i] <= 0) {
       yyerror(0, lParse, "ARRAY(V,dims) must have positive dimensions");
       return (-1);
     }
     nelem *= naxes[i];
   }

   if (SIZE(valueNode) == nelem && nelem > 1) {
     /* "reform" operation - do nothing */
   } else if (SIZE(valueNode) > 1 && nelem > 1) {
     yyerror(0, lParse, "ARRAY(V,d) mismatch between number of elements in V and d");
     return (-1);
   } else if (SIZE(valueNode) > 1) {
     yyerror(0, lParse, "ARRAY(V,n) value V must have vector dimension of 1");
     return (-1);
   }
   
   n = Alloc_Node(lParse);
   if( n>=0 ) {
      this             = lParse->Nodes + n;
      this->operation  = array_fct;
      this->nSubNodes  = 1;
      this->SubNodes[0]= valueNode;
      this->type       = TYPE(valueNode);

      this->value.nelem = nelem;
      this->value.naxis = naxis;
      for( i=0; i<naxis; i++ )
	this->value.naxes[i] = naxes[i];

      this->DoOp = Do_Array;
   }
   return( n );
}

static int Locate_Col( ParseData *lParse, Node *this )
/*  Locate the TABLE column number of any columns in "this" calculation.  */
/*  Return ZERO if none found, or negative if more than 1 found.          */
{
   Node *that;
   int  i, col=0, newCol, nfound=0;
   
   if( this->nSubNodes==0
       && this->operation<=0 && this->operation!=CONST_OP )
      return lParse->colData[ - this->operation].colnum;

   for( i=0; i<this->nSubNodes; i++ ) {
      that = lParse->Nodes + this->SubNodes[i];
      if( that->operation>0 ) {
	 newCol = Locate_Col( lParse, that );
	 if( newCol<=0 ) {
	    nfound += -newCol;
	 } else {
	    if( !nfound ) {
	       col = newCol;
	       nfound++;
	    } else if( col != newCol ) {
	       nfound++;
	    }
	 }
      } else if( that->operation!=CONST_OP ) {
	 /*  Found a Column  */
	 newCol = lParse->colData[- that->operation].colnum;
	 if( !nfound ) {
	    col = newCol;
	    nfound++;
	 } else if( col != newCol ) {
	    nfound++;
	 }
      }
   }
   if( nfound!=1 )
      return( - nfound );
   else
      return( col );
}

static int Test_Dims( ParseData *lParse, int Node1, int Node2 )
{
   Node *that1, *that2;
   int valid, i;

   if( Node1<0 || Node2<0 ) return(0);

   that1 = lParse->Nodes + Node1;
   that2 = lParse->Nodes + Node2;

   if( that1->value.nelem==1 || that2->value.nelem==1 )
      valid = 1;
   else if( that1->type==that2->type
	    && that1->value.nelem==that2->value.nelem
	    && that1->value.naxis==that2->value.naxis ) {
      valid = 1;
      for( i=0; i<that1->value.naxis; i++ ) {
	 if( that1->value.naxes[i]!=that2->value.naxes[i] )
	    valid = 0;
      }
   } else
      valid = 0;
   return( valid );
}   

static void Copy_Dims( ParseData *lParse, int Node1, int Node2 )
{
   Node *that1, *that2;
   int i;

   if( Node1<0 || Node2<0 ) return;

   that1 = lParse->Nodes + Node1;
   that2 = lParse->Nodes + Node2;

   that1->value.nelem = that2->value.nelem;
   that1->value.naxis = that2->value.naxis;
   for( i=0; i<that2->value.naxis; i++ )
      that1->value.naxes[i] = that2->value.naxes[i];
}

/********************************************************************/
/*    Routines for actually evaluating the expression start here    */
/********************************************************************/

void Evaluate_Parser( ParseData *lParse, long firstRow, long nRows )
    /***********************************************************************/
    /*  Reset the parser for processing another batch of data...           */
    /*    firstRow:  Row number of the first element to evaluate           */
    /*    nRows:     Number of rows to be processed                        */
    /*  Initialize each COLUMN node so that its UNDEF and DATA pointers    */
    /*  point to the appropriate column arrays.                            */
    /*  Finally, call Evaluate_Node for final node.                        */
    /***********************************************************************/
{
   int     i, column;
   long    offset, rowOffset;
   static int rand_initialized = 0;

   /* Initialize the random number generator once and only once */
   if (rand_initialized == 0) {
     simplerng_srand( (unsigned int) time(NULL) );
     rand_initialized = 1;
   }

   lParse->firstRow = firstRow;
   lParse->nRows    = nRows;

   /*  Reset Column Nodes' pointers to point to right data and UNDEF arrays  */

   rowOffset = firstRow - lParse->firstDataRow;
   for( i=0; i<lParse->nNodes; i++ ) {
     if(    OPER(i) >  0 || OPER(i) == CONST_OP ) continue;

      column = -OPER(i);
      offset = lParse->varData[column].nelem * rowOffset;

      lParse->Nodes[i].value.undef = lParse->varData[column].undef + offset;

      switch( lParse->Nodes[i].type ) {
      case BITSTR:
	 lParse->Nodes[i].value.data.strptr =
	    (char**)lParse->varData[column].data + rowOffset;
	 lParse->Nodes[i].value.undef       = NULL;
	 break;
      case STRING:
	 lParse->Nodes[i].value.data.strptr = 
	    (char**)lParse->varData[column].data + rowOffset;
	 lParse->Nodes[i].value.undef = lParse->varData[column].undef + rowOffset;
	 break;
      case BOOLEAN:
	 lParse->Nodes[i].value.data.logptr = 
	    (char*)lParse->varData[column].data + offset;
	 break;
      case LONG:
	 lParse->Nodes[i].value.data.lngptr = 
	    (long*)lParse->varData[column].data + offset;
	 break;
      case DOUBLE:
	 lParse->Nodes[i].value.data.dblptr = 
	    (double*)lParse->varData[column].data + offset;
	 break;
      }
   }

   Evaluate_Node( lParse, lParse->resultNode );
}

static void Evaluate_Node( ParseData *lParse, int thisNode )
    /**********************************************************************/
    /*  Recursively evaluate thisNode's subNodes, then call one of the    */
    /*  Do_<Action> functions pointed to by thisNode's DoOp element.      */
    /**********************************************************************/
{
   Node *this;
   int i;
   
   if( lParse->status ) return;

   this = lParse->Nodes + thisNode;
   if( this->operation>0 ) {  /* <=0 indicate constants and columns */
      i = this->nSubNodes;
      while( i-- ) {
	 Evaluate_Node( lParse, this->SubNodes[i] );
	 if( lParse->status ) return;
      }
      this->DoOp( lParse, this );
   }
}

static void Allocate_Ptrs( ParseData *lParse, Node *this )
{
   long elem, row, size;

   if( this->type==BITSTR || this->type==STRING ) {

      this->value.data.strptr = (char**)malloc( lParse->nRows
						* sizeof(char*) );
      if( this->value.data.strptr ) {
	 this->value.data.strptr[0] = (char*)malloc( lParse->nRows
						     * (this->value.nelem+2)
						     * sizeof(char) );
	 if( this->value.data.strptr[0] ) {
	    row = 0;
	    while( (++row)<lParse->nRows ) {
	       this->value.data.strptr[row] =
		  this->value.data.strptr[row-1] + this->value.nelem+1;
	    }
	    if( this->type==STRING ) {
	       this->value.undef = this->value.data.strptr[row-1]
                                   + this->value.nelem+1;
	    } else {
	       this->value.undef = NULL;  /* BITSTRs don't use undef array */
	    }
	 } else {
	    lParse->status = MEMORY_ALLOCATION;
	    free( this->value.data.strptr );
	 }
      } else {
	 lParse->status = MEMORY_ALLOCATION;
      }

   } else {

      elem = this->value.nelem * lParse->nRows;
      switch( this->type ) {
      case DOUBLE:  size = sizeof( double ); break;
      case LONG:    size = sizeof( long   ); break;
      case BOOLEAN: size = sizeof( char   ); break;
      default:      size = 1;                break;
      }

      this->value.data.ptr = calloc(size+1, elem);

      if( this->value.data.ptr==NULL ) {
	 lParse->status = MEMORY_ALLOCATION;
      } else {
	 this->value.undef = (char *)this->value.data.ptr + elem*size;
      }
   }
}

static void Do_Unary( ParseData *lParse, Node *this )
{
   Node *that;
   long elem;

   that = lParse->Nodes + this->SubNodes[0];

   if( that->operation==CONST_OP ) {  /* Operating on a constant! */
      switch( this->operation ) {
      case DOUBLE:
      case FLTCAST:
	 if( that->type==LONG )
	    this->value.data.dbl = (double)that->value.data.lng;
	 else if( that->type==BOOLEAN )
	    this->value.data.dbl = ( that->value.data.log ? 1.0 : 0.0 );
	 break;
      case LONG:
      case INTCAST:
	 if( that->type==DOUBLE )
	    this->value.data.lng = (long)that->value.data.dbl;
	 else if( that->type==BOOLEAN )
	    this->value.data.lng = ( that->value.data.log ? 1L : 0L );
	 break;
      case BOOLEAN:
	 if( that->type==DOUBLE )
	    this->value.data.log = ( that->value.data.dbl != 0.0 );
	 else if( that->type==LONG )
	    this->value.data.log = ( that->value.data.lng != 0L );
	 break;
      case UMINUS:
	 if( that->type==DOUBLE )
	    this->value.data.dbl = - that->value.data.dbl;
	 else if( that->type==LONG )
	    this->value.data.lng = - that->value.data.lng;
	 break;
      case NOT:
	 if( that->type==BOOLEAN )
	    this->value.data.log = ( ! that->value.data.log );
	 else if( that->type==BITSTR )
	    bitnot( this->value.data.str, that->value.data.str );
	 break;
      }
      this->operation = CONST_OP;

   } else {

      Allocate_Ptrs( lParse, this );

      if( !lParse->status ) {

	 if( this->type!=BITSTR ) {
	    elem = lParse->nRows;
	    if( this->type!=STRING )
	       elem *= this->value.nelem;
	    while( elem-- )
	       this->value.undef[elem] = that->value.undef[elem];
	 }

	 elem = lParse->nRows * this->value.nelem;

	 switch( this->operation ) {

	 case BOOLEAN:
	    if( that->type==DOUBLE )
	       while( elem-- )
		  this->value.data.logptr[elem] =
		     ( that->value.data.dblptr[elem] != 0.0 );
	    else if( that->type==LONG )
	       while( elem-- )
		  this->value.data.logptr[elem] =
		     ( that->value.data.lngptr[elem] != 0L );
	    break;

	 case DOUBLE:
	 case FLTCAST:
	    if( that->type==LONG )
	       while( elem-- )
		  this->value.data.dblptr[elem] =
		     (double)that->value.data.lngptr[elem];
	    else if( that->type==BOOLEAN )
	       while( elem-- )
		  this->value.data.dblptr[elem] =
		     ( that->value.data.logptr[elem] ? 1.0 : 0.0 );
	    break;

	 case LONG:
	 case INTCAST:
	    if( that->type==DOUBLE )
	       while( elem-- )
		  this->value.data.lngptr[elem] =
		     (long)that->value.data.dblptr[elem];
	    else if( that->type==BOOLEAN )
	       while( elem-- )
		  this->value.data.lngptr[elem] =
		     ( that->value.data.logptr[elem] ? 1L : 0L );
	    break;

	 case UMINUS:
	    if( that->type==DOUBLE ) {
	       while( elem-- )
		  this->value.data.dblptr[elem] =
		     - that->value.data.dblptr[elem];
	    } else if( that->type==LONG ) {
	       while( elem-- )
		  this->value.data.lngptr[elem] =
		     - that->value.data.lngptr[elem];
	    }
	    break;

	 case NOT:
	    if( that->type==BOOLEAN ) {
	       while( elem-- )
		  this->value.data.logptr[elem] =
		     ( ! that->value.data.logptr[elem] );
	    } else if( that->type==BITSTR ) {
	       elem = lParse->nRows;
	       while( elem-- )
		  bitnot( this->value.data.strptr[elem],
			  that->value.data.strptr[elem] );
	    }
	    break;
	 }
      }
   }

   if( that->operation>0 ) {
      free( that->value.data.ptr );
   }
}

static void Do_Offset( ParseData *lParse, Node *this )
{
   Node *col;
   long fRow, nRowOverlap, nRowReload, rowOffset;
   long nelem, elem, offset, nRealElem;
   int status;

   col       = lParse->Nodes + this->SubNodes[0];
   rowOffset = lParse->Nodes[  this->SubNodes[1] ].value.data.lng;

   Allocate_Ptrs( lParse, this );

   fRow   = lParse->firstRow + rowOffset;
   if( this->type==STRING || this->type==BITSTR )
      nRealElem = 1;
   else
      nRealElem = this->value.nelem;

   nelem = nRealElem;

   if( fRow < lParse->firstDataRow ) {

      /* Must fill in data at start of array */

      nRowReload = lParse->firstDataRow - fRow;
      if( nRowReload > lParse->nRows ) nRowReload = lParse->nRows;
      nRowOverlap = lParse->nRows - nRowReload;

      offset = 0;

      /*  NULLify any values falling out of bounds  */

      while( fRow<1 && nRowReload>0 ) {
	 if( this->type == BITSTR ) {
	    nelem = this->value.nelem;
	    this->value.data.strptr[offset][ nelem ] = '\0';
	    while( nelem-- ) this->value.data.strptr[offset][nelem] = '0';
	    offset++;
	 } else {
	    while( nelem-- )
	       this->value.undef[offset++] = 1;
	 }
	 nelem = nRealElem;
	 fRow++;
	 nRowReload--;
      }

   } else if( fRow + lParse->nRows > lParse->firstDataRow + lParse->nDataRows ) {

      /* Must fill in data at end of array */

      nRowReload = (fRow+lParse->nRows) - (lParse->firstDataRow+lParse->nDataRows);
      if( nRowReload>lParse->nRows ) {
	 nRowReload = lParse->nRows;
      } else {
	 fRow = lParse->firstDataRow + lParse->nDataRows;
      }
      nRowOverlap = lParse->nRows - nRowReload;

      offset = nRowOverlap * nelem;

      /*  NULLify any values falling out of bounds  */

      elem = lParse->nRows * nelem;
      while( fRow+nRowReload>lParse->totalRows && nRowReload>0 ) {
	 if( this->type == BITSTR ) {
	    nelem = this->value.nelem;
	    elem--;
	    this->value.data.strptr[elem][ nelem ] = '\0';
	    while( nelem-- ) this->value.data.strptr[elem][nelem] = '0';
	 } else {
	    while( nelem-- )
	       this->value.undef[--elem] = 1;
	 }
	 nelem = nRealElem;
	 nRowReload--;
      }

   } else {

      nRowReload  = 0;
      nRowOverlap = lParse->nRows;
      offset      = 0;

   }

   if( nRowReload>0 ) {
      switch( this->type ) {
      case BITSTR:
      case STRING:
	 status = (*lParse->loadData)( lParse, -col->operation, fRow, nRowReload,
				      this->value.data.strptr+offset,
				      this->value.undef+offset );
	 break;
      case BOOLEAN:
	 status = (*lParse->loadData)( lParse, -col->operation, fRow, nRowReload,
				      this->value.data.logptr+offset,
				      this->value.undef+offset );
	 break;
      case LONG:
	 status = (*lParse->loadData)( lParse, -col->operation, fRow, nRowReload,
				      this->value.data.lngptr+offset,
				      this->value.undef+offset );
	 break;
      case DOUBLE:
	 status = (*lParse->loadData)( lParse, -col->operation, fRow, nRowReload,
				      this->value.data.dblptr+offset,
				      this->value.undef+offset );
	 break;
      }
   }

   /*  Now copy over the overlapping region, if any  */

   if( nRowOverlap <= 0 ) return;

   if( rowOffset>0 )
      elem = nRowOverlap * nelem;
   else
      elem = lParse->nRows * nelem;

   offset = nelem * rowOffset;
   while( nRowOverlap-- && !lParse->status ) {
      while( nelem-- && !lParse->status ) {
	 elem--;
	 if( this->type != BITSTR )
	    this->value.undef[elem] = col->value.undef[elem+offset];
	 switch( this->type ) {
	 case BITSTR:
	    strcpy( this->value.data.strptr[elem       ],
                     col->value.data.strptr[elem+offset] );
	    break;
	 case STRING:
	    strcpy( this->value.data.strptr[elem       ],
                     col->value.data.strptr[elem+offset] );
	    break;
	 case BOOLEAN:
	    this->value.data.logptr[elem] = col->value.data.logptr[elem+offset];
	    break;
	 case LONG:
	    this->value.data.lngptr[elem] = col->value.data.lngptr[elem+offset];
	    break;
	 case DOUBLE:
	    this->value.data.dblptr[elem] = col->value.data.dblptr[elem+offset];
	    break;
	 }
      }
      nelem = nRealElem;
   }
}

static void Do_BinOp_bit( ParseData *lParse, Node *this )
{
   Node *that1, *that2;
   char *sptr1=NULL, *sptr2=NULL;
   int  const1, const2;
   long rows;

   that1 = lParse->Nodes + this->SubNodes[0];
   that2 = lParse->Nodes + this->SubNodes[1];

   const1 = ( that1->operation==CONST_OP );
   const2 = ( that2->operation==CONST_OP );
   sptr1  = ( const1 ? that1->value.data.str : NULL );
   sptr2  = ( const2 ? that2->value.data.str : NULL );

   if( const1 && const2 ) {
      switch( this->operation ) {
      case NE:
	 this->value.data.log = !bitcmp( sptr1, sptr2 );
	 break;
      case EQ:
	 this->value.data.log =  bitcmp( sptr1, sptr2 );
	 break;
      case GT:
      case LT:
      case LTE:
      case GTE:
	 this->value.data.log = bitlgte( sptr1, this->operation, sptr2 );
	 break;
      case '|': 
	 bitor( this->value.data.str, sptr1, sptr2 );
	 break;
      case '&': 
	 bitand( this->value.data.str, sptr1, sptr2 );
	 break;
      case '+':
	 strcpy( this->value.data.str, sptr1 );
	 strcat( this->value.data.str, sptr2 );
	 break;
      case ACCUM:
	this->value.data.lng = 0;
	while( *sptr1 ) {
	  if ( *sptr1 == '1' ) this->value.data.lng ++;
	  sptr1 ++;
	}
	break;
	
      }
      this->operation = CONST_OP;

   } else {

      Allocate_Ptrs( lParse, this );
     
      if( !lParse->status ) {
	 rows  = lParse->nRows;
	 switch( this->operation ) {

	    /*  BITSTR comparisons  */

	 case NE:
	 case EQ:
	 case GT:
	 case LT:
	 case LTE:
	 case GTE:
	    while( rows-- ) {
	       if( !const1 )
		  sptr1 = that1->value.data.strptr[rows];
	       if( !const2 )
		  sptr2 = that2->value.data.strptr[rows];
	       switch( this->operation ) {
	       case NE:  this->value.data.logptr[rows] = 
                                                      !bitcmp( sptr1, sptr2 );
                         break;
	       case EQ:  this->value.data.logptr[rows] = 
                                                       bitcmp( sptr1, sptr2 );
                         break;
	       case GT:
	       case LT:
	       case LTE:
	       case GTE: this->value.data.logptr[rows] = 
                                     bitlgte( sptr1, this->operation, sptr2 );
	                 break;
	       }
	       this->value.undef[rows] = 0;
	    }
	    break;
	 
	    /*  BITSTR AND/ORs ...  no UNDEFS in or out */
      
	 case '|': 
	 case '&': 
	 case '+':
	    while( rows-- ) {
	       if( !const1 )
		  sptr1 = that1->value.data.strptr[rows];
	       if( !const2 )
		  sptr2 = that2->value.data.strptr[rows];
	       if( this->operation=='|' )
		  bitor(  this->value.data.strptr[rows], sptr1, sptr2 );
	       else if( this->operation=='&' )
		  bitand( this->value.data.strptr[rows], sptr1, sptr2 );
	       else {
		  strcpy( this->value.data.strptr[rows], sptr1 );
		  strcat( this->value.data.strptr[rows], sptr2 );
	       }
	    }
	    break;

	    /* Accumulate 1 bits */
	 case ACCUM:
	   { 
	     long i, previous, curr;

	     previous = that2->value.data.lng;
	     
	      /* Cumulative sum of this chunk */
	     for (i=0; i<rows; i++) {
	       sptr1 = that1->value.data.strptr[i];
	       for (curr = 0; *sptr1; sptr1 ++) {
		 if ( *sptr1 == '1' ) curr ++;
	       }
	       previous += curr;
	       this->value.data.lngptr[i] = previous;
	       this->value.undef[i] = 0;
	     }
	     
	      /* Store final cumulant for next pass */
	     that2->value.data.lng = previous;
	   }
	 }
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.strptr[0] );
      free( that1->value.data.strptr    );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.strptr[0] );
      free( that2->value.data.strptr    );
   }
}

static void Do_BinOp_str( ParseData *lParse, Node *this )
{
   Node *that1, *that2;
   char *sptr1, *sptr2, null1=0, null2=0;
   int const1, const2, val;
   long rows;

   that1 = lParse->Nodes + this->SubNodes[0];
   that2 = lParse->Nodes + this->SubNodes[1];

   const1 = ( that1->operation==CONST_OP );
   const2 = ( that2->operation==CONST_OP );
   sptr1  = ( const1 ? that1->value.data.str : NULL );
   sptr2  = ( const2 ? that2->value.data.str : NULL );

   if( const1 && const2 ) {  /*  Result is a constant  */
      switch( this->operation ) {

	 /*  Compare Strings  */

      case NE:
      case EQ:
	 val = ( FSTRCMP( sptr1, sptr2 ) == 0 );
	 this->value.data.log = ( this->operation==EQ ? val : !val );
	 break;
      case GT:
	 this->value.data.log = ( FSTRCMP( sptr1, sptr2 ) > 0 );
	 break;
      case LT:
	 this->value.data.log = ( FSTRCMP( sptr1, sptr2 ) < 0 );
	 break;
      case GTE:
	 this->value.data.log = ( FSTRCMP( sptr1, sptr2 ) >= 0 );
	 break;
      case LTE:
	 this->value.data.log = ( FSTRCMP( sptr1, sptr2 ) <= 0 );
	 break;

	 /*  Concat Strings  */

      case '+':
	 strcpy( this->value.data.str, sptr1 );
	 strcat( this->value.data.str, sptr2 );
	 break;
      }
      this->operation = CONST_OP;

   } else {  /*  Not a constant  */

     Allocate_Ptrs( lParse, this );

      if( !lParse->status ) {

	 rows = lParse->nRows;
	 switch( this->operation ) {

	    /*  Compare Strings  */

	 case NE:
	 case EQ:
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  val = ( FSTRCMP( sptr1, sptr2 ) == 0 );
		  this->value.data.logptr[rows] =
		     ( this->operation==EQ ? val : !val );
	       }
	    }
	    break;
	    
	 case GT:
	 case LT:
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  val = ( FSTRCMP( sptr1, sptr2 ) );
		  this->value.data.logptr[rows] =
		     ( this->operation==GT ? val>0 : val<0 );
	       }
	    }
	    break;

	 case GTE:
	 case LTE:
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  val = ( FSTRCMP( sptr1, sptr2 ) );
		  this->value.data.logptr[rows] =
		     ( this->operation==GTE ? val>=0 : val<=0 );
	       }
	    }
	    break;

	    /*  Concat Strings  */
	    
	 case '+':
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  strcpy( this->value.data.strptr[rows], sptr1 );
		  strcat( this->value.data.strptr[rows], sptr2 );
	       }
	    }
	    break;
	 }
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.strptr[0] );
      free( that1->value.data.strptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.strptr[0] );
      free( that2->value.data.strptr );
   }
}

static void Do_BinOp_log( ParseData *lParse, Node *this )
{
   Node *that1, *that2;
   int vector1, vector2;
   char val1=0, val2=0, null1=0, null2=0;
   long rows, nelem, elem;

   that1 = lParse->Nodes + this->SubNodes[0];
   that2 = lParse->Nodes + this->SubNodes[1];

   vector1 = ( that1->operation!=CONST_OP );
   if( vector1 )
      vector1 = that1->value.nelem;
   else {
      val1  = that1->value.data.log;
   }

   vector2 = ( that2->operation!=CONST_OP );
   if( vector2 )
      vector2 = that2->value.nelem;
   else {
      val2  = that2->value.data.log;
   }

   if( !vector1 && !vector2 ) {  /*  Result is a constant  */
      switch( this->operation ) {
      case OR:
	 this->value.data.log = (val1 || val2);
	 break;
      case AND:
	 this->value.data.log = (val1 && val2);
	 break;
      case EQ:
	 this->value.data.log = ( (val1 && val2) || (!val1 && !val2) );
	 break;
      case NE:
	 this->value.data.log = ( (val1 && !val2) || (!val1 && val2) );
	 break;
      case ACCUM:
	 this->value.data.lng = val1;
	 break;
      }
      this->operation=CONST_OP;
   } else if (this->operation == ACCUM) {
      long i, previous, curr;
      rows  = lParse->nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;
      
      Allocate_Ptrs( lParse, this );
      
      if( !lParse->status ) {
	previous = that2->value.data.lng;
	
	/* Cumulative sum of this chunk */
	for (i=0; i<elem; i++) {
	  if (!that1->value.undef[i]) {
	    curr = that1->value.data.logptr[i];
	    previous += curr;
	  }
	  this->value.data.lngptr[i] = previous;
	  this->value.undef[i] = 0;
	}
	
	/* Store final cumulant for next pass */
	that2->value.data.lng = previous;
      }
      
   } else {
      rows  = lParse->nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;

      Allocate_Ptrs( lParse, this );

      if( !lParse->status ) {
	
	 if (this->operation == ACCUM) {
	   long i, previous, curr;
	   
	   previous = that2->value.data.lng;
	   
	   /* Cumulative sum of this chunk */
	   for (i=0; i<elem; i++) {
	     if (!that1->value.undef[i]) {
	       curr = that1->value.data.logptr[i];
	       previous += curr;
	     }
	     this->value.data.lngptr[i] = previous;
	     this->value.undef[i] = 0;
	   }
	   
	   /* Store final cumulant for next pass */
	   that2->value.data.lng = previous;
	 }
	
	 while( rows-- ) {
	    while( nelem-- ) {
	       elem--;

	       if( vector1>1 ) {
		  val1  = that1->value.data.logptr[elem];
		  null1 = that1->value.undef[elem];
	       } else if( vector1 ) {
		  val1  = that1->value.data.logptr[rows];
		  null1 = that1->value.undef[rows];
	       }

	       if( vector2>1 ) {
		  val2  = that2->value.data.logptr[elem];
		  null2 = that2->value.undef[elem];
	       } else if( vector2 ) {
		  val2  = that2->value.data.logptr[rows];
		  null2 = that2->value.undef[rows];
	       }

	       this->value.undef[elem] = (null1 || null2);
	       switch( this->operation ) {

	       case OR:
		  /*  This is more complicated than others to suppress UNDEFs */
		  /*  in those cases where the other argument is DEF && TRUE  */

		  if( !null1 && !null2 ) {
		     this->value.data.logptr[elem] = (val1 || val2);
		  } else if( (null1 && !null2 && val2)
			     || ( !null1 && null2 && val1 ) ) {
		     this->value.data.logptr[elem] = 1;
		     this->value.undef[elem] = 0;
		  }
		  break;

	       case AND:
		  /*  This is more complicated than others to suppress UNDEFs */
		  /*  in those cases where the other argument is DEF && FALSE */

		  if( !null1 && !null2 ) {
		     this->value.data.logptr[elem] = (val1 && val2);
		  } else if( (null1 && !null2 && !val2)
			     || ( !null1 && null2 && !val1 ) ) {
		     this->value.data.logptr[elem] = 0;
		     this->value.undef[elem] = 0;
		  }
		  break;

	       case EQ:
		  this->value.data.logptr[elem] = 
		     ( (val1 && val2) || (!val1 && !val2) );
		  break;

	       case NE:
		  this->value.data.logptr[elem] =
		     ( (val1 && !val2) || (!val1 && val2) );
		  break;
	       }
	    }
	    nelem = this->value.nelem;
	 }
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.ptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.ptr );
   }
}

static void Do_BinOp_lng( ParseData *lParse, Node *this )
{
   Node *that1, *that2;
   int  vector1, vector2;
   long val1=0, val2=0;
   char null1=0, null2=0;
   long rows, nelem, elem;

   that1 = lParse->Nodes + this->SubNodes[0];
   that2 = lParse->Nodes + this->SubNodes[1];

   vector1 = ( that1->operation!=CONST_OP );
   if( vector1 )
      vector1 = that1->value.nelem;
   else {
      val1  = that1->value.data.lng;
   }

   vector2 = ( that2->operation!=CONST_OP );
   if( vector2 )
      vector2 = that2->value.nelem;
   else {
      val2  = that2->value.data.lng;
   }

   if( !vector1 && !vector2 ) {  /*  Result is a constant  */

      switch( this->operation ) {
      case '~':   /* Treat as == for LONGS */
      case EQ:    this->value.data.log = (val1 == val2);   break;
      case NE:    this->value.data.log = (val1 != val2);   break;
      case GT:    this->value.data.log = (val1 >  val2);   break;
      case LT:    this->value.data.log = (val1 <  val2);   break;
      case LTE:   this->value.data.log = (val1 <= val2);   break;
      case GTE:   this->value.data.log = (val1 >= val2);   break;

      case '+':   this->value.data.lng = (val1  + val2);   break;
      case '-':   this->value.data.lng = (val1  - val2);   break;
      case '*':   this->value.data.lng = (val1  * val2);   break;

      case '&':   this->value.data.lng = (val1  & val2);   break;
      case '|':   this->value.data.lng = (val1  | val2);   break;
      case '^':   this->value.data.lng = (val1  ^ val2);   break;

      case '%':
	 if( val2 ) this->value.data.lng = (val1 % val2);
	 else       yyerror(0, lParse, "Divide by Zero");
	 break;
      case '/': 
	 if( val2 ) this->value.data.lng = (val1 / val2); 
	 else       yyerror(0, lParse, "Divide by Zero");
	 break;
      case POWER:
	 this->value.data.lng = (long)pow((double)val1,(double)val2);
	 break;
      case ACCUM:
	 this->value.data.lng = val1;
	 break;
      case DIFF:
	 this->value.data.lng = 0;
	 break;
      }
      this->operation=CONST_OP;

   } else if ((this->operation == ACCUM) || (this->operation == DIFF)) {
      long i, previous, curr;
      long undef;
      rows  = lParse->nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;
      
      Allocate_Ptrs( lParse, this );
      
      if( !lParse->status ) {
	previous = that2->value.data.lng;
	undef    = (long) that2->value.undef;
	
	if (this->operation == ACCUM) {
	  /* Cumulative sum of this chunk */
	  for (i=0; i<elem; i++) {
	    if (!that1->value.undef[i]) {
	      curr = that1->value.data.lngptr[i];
	      previous += curr;
	    }
	    this->value.data.lngptr[i] = previous;
	    this->value.undef[i] = 0;
	  }
	} else {
	  /* Sequential difference for this chunk */
	  for (i=0; i<elem; i++) {
	    curr = that1->value.data.lngptr[i];
	    if (that1->value.undef[i] || undef) {
	      /* Either this, or previous, value was undefined */
	      this->value.data.lngptr[i] = 0;
	      this->value.undef[i] = 1;
	    } else {
	      /* Both defined, we are okay! */
	      this->value.data.lngptr[i] = curr - previous;
	      this->value.undef[i] = 0;
	    }

	    previous = curr;
	    undef = that1->value.undef[i];
	  }
	}	  
	
	/* Store final cumulant for next pass */
	that2->value.data.lng = previous;
	that2->value.undef    = (char *) undef; /* XXX evil, but no harm here */
      }
      
   } else {

      rows  = lParse->nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;

      Allocate_Ptrs( lParse, this );

      while( rows-- && !lParse->status ) {
	 while( nelem-- && !lParse->status ) {
	    elem--;

	    if( vector1>1 ) {
	       val1  = that1->value.data.lngptr[elem];
	       null1 = that1->value.undef[elem];
	    } else if( vector1 ) {
	       val1  = that1->value.data.lngptr[rows];
	       null1 = that1->value.undef[rows];
	    }

	    if( vector2>1 ) {
	       val2  = that2->value.data.lngptr[elem];
	       null2 = that2->value.undef[elem];
	    } else if( vector2 ) {
	       val2  = that2->value.data.lngptr[rows];
	       null2 = that2->value.undef[rows];
	    }

	    this->value.undef[elem] = (null1 || null2);
	    switch( this->operation ) {
	    case '~':   /* Treat as == for LONGS */
	    case EQ:   this->value.data.logptr[elem] = (val1 == val2);   break;
	    case NE:   this->value.data.logptr[elem] = (val1 != val2);   break;
	    case GT:   this->value.data.logptr[elem] = (val1 >  val2);   break;
	    case LT:   this->value.data.logptr[elem] = (val1 <  val2);   break;
	    case LTE:  this->value.data.logptr[elem] = (val1 <= val2);   break;
	    case GTE:  this->value.data.logptr[elem] = (val1 >= val2);   break;
	       
	    case '+':  this->value.data.lngptr[elem] = (val1  + val2);   break;
	    case '-':  this->value.data.lngptr[elem] = (val1  - val2);   break;
	    case '*':  this->value.data.lngptr[elem] = (val1  * val2);   break;

	    case '&':  this->value.data.lngptr[elem] = (val1  & val2);   break;
	    case '|':  this->value.data.lngptr[elem] = (val1  | val2);   break;
	    case '^':  this->value.data.lngptr[elem] = (val1  ^ val2);   break;

	    case '%':   
	       if( val2 ) this->value.data.lngptr[elem] = (val1 % val2);
	       else {
		 this->value.data.lngptr[elem] = 0;
		 this->value.undef[elem] = 1;
	       }
	       break;
	    case '/': 
	       if( val2 ) this->value.data.lngptr[elem] = (val1 / val2); 
	       else {
		 this->value.data.lngptr[elem] = 0;
		 this->value.undef[elem] = 1;
	       }
	       break;
	    case POWER:
	       this->value.data.lngptr[elem] = (long)pow((double)val1,(double)val2);
	       break;
	    }
	 }
	 nelem = this->value.nelem;
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.ptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.ptr );
   }
}

static void Do_BinOp_dbl( ParseData *lParse, Node *this )
{
   Node   *that1, *that2;
   int    vector1, vector2;
   double val1=0.0, val2=0.0;
   char   null1=0, null2=0;
   long   rows, nelem, elem;

   that1 = lParse->Nodes + this->SubNodes[0];
   that2 = lParse->Nodes + this->SubNodes[1];

   vector1 = ( that1->operation!=CONST_OP );
   if( vector1 )
      vector1 = that1->value.nelem;
   else {
      val1  = that1->value.data.dbl;
   }

   vector2 = ( that2->operation!=CONST_OP );
   if( vector2 )
      vector2 = that2->value.nelem;
   else {
      val2  = that2->value.data.dbl;
   } 

   if( !vector1 && !vector2 ) {  /*  Result is a constant  */

      switch( this->operation ) {
      case '~':   this->value.data.log = ( fabs(val1-val2) < APPROX );   break;
      case EQ:    this->value.data.log = (val1 == val2);   break;
      case NE:    this->value.data.log = (val1 != val2);   break;
      case GT:    this->value.data.log = (val1 >  val2);   break;
      case LT:    this->value.data.log = (val1 <  val2);   break;
      case LTE:   this->value.data.log = (val1 <= val2);   break;
      case GTE:   this->value.data.log = (val1 >= val2);   break;

      case '+':   this->value.data.dbl = (val1  + val2);   break;
      case '-':   this->value.data.dbl = (val1  - val2);   break;
      case '*':   this->value.data.dbl = (val1  * val2);   break;

      case '%':
	 if( val2 ) this->value.data.dbl = val1 - val2*((int)(val1/val2));
	 else       yyerror(0, lParse, "Divide by Zero");
	 break;
      case '/': 
	 if( val2 ) this->value.data.dbl = (val1 / val2); 
	 else       yyerror(0, lParse, "Divide by Zero");
	 break;
      case POWER:
	 this->value.data.dbl = (double)pow(val1,val2);
	 break;
      case ACCUM:
	 this->value.data.dbl = val1;
	 break;
      case DIFF:
	this->value.data.dbl = 0;
	 break;
      }
      this->operation=CONST_OP;

   } else if ((this->operation == ACCUM) || (this->operation == DIFF)) {
      long i;
      long undef;
      double previous, curr;
      rows  = lParse->nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;
      
      Allocate_Ptrs( lParse, this );
      
      if( !lParse->status ) {
	previous = that2->value.data.dbl;
	undef    = (long) that2->value.undef;
	
	if (this->operation == ACCUM) {
	  /* Cumulative sum of this chunk */
	  for (i=0; i<elem; i++) {
	    if (!that1->value.undef[i]) {
	      curr = that1->value.data.dblptr[i];
	      previous += curr;
	    }
	    this->value.data.dblptr[i] = previous;
	    this->value.undef[i] = 0;
	  }
	} else {
	  /* Sequential difference for this chunk */
	  for (i=0; i<elem; i++) {
	    curr = that1->value.data.dblptr[i];
	    if (that1->value.undef[i] || undef) {
	      /* Either this, or previous, value was undefined */
	      this->value.data.dblptr[i] = 0;
	      this->value.undef[i] = 1;
	    } else {
	      /* Both defined, we are okay! */
	      this->value.data.dblptr[i] = curr - previous;
	      this->value.undef[i] = 0;
	    }

	    previous = curr;
	    undef = that1->value.undef[i];
	  }
	}	  
	
	/* Store final cumulant for next pass */
	that2->value.data.dbl = previous;
	that2->value.undef    = (char *) undef; /* XXX evil, but no harm here */
      }
      
   } else {

      rows  = lParse->nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;

      Allocate_Ptrs( lParse, this );

      while( rows-- && !lParse->status ) {
	 while( nelem-- && !lParse->status ) {
	    elem--;

	    if( vector1>1 ) {
	       val1  = that1->value.data.dblptr[elem];
	       null1 = that1->value.undef[elem];
	    } else if( vector1 ) {
	       val1  = that1->value.data.dblptr[rows];
	       null1 = that1->value.undef[rows];
	    }

	    if( vector2>1 ) {
	       val2  = that2->value.data.dblptr[elem];
	       null2 = that2->value.undef[elem];
	    } else if( vector2 ) {
	       val2  = that2->value.data.dblptr[rows];
	       null2 = that2->value.undef[rows];
	    }

	    this->value.undef[elem] = (null1 || null2);
	    switch( this->operation ) {
	    case '~':   this->value.data.logptr[elem] =
                                          ( fabs(val1-val2) < APPROX );   break;
	    case EQ:    this->value.data.logptr[elem] = (val1 == val2);   break;
	    case NE:    this->value.data.logptr[elem] = (val1 != val2);   break;
	    case GT:    this->value.data.logptr[elem] = (val1 >  val2);   break;
	    case LT:    this->value.data.logptr[elem] = (val1 <  val2);   break;
	    case LTE:   this->value.data.logptr[elem] = (val1 <= val2);   break;
	    case GTE:   this->value.data.logptr[elem] = (val1 >= val2);   break;
	       
	    case '+':   this->value.data.dblptr[elem] = (val1  + val2);   break;
	    case '-':   this->value.data.dblptr[elem] = (val1  - val2);   break;
	    case '*':   this->value.data.dblptr[elem] = (val1  * val2);   break;

	    case '%':
	       if( val2 ) this->value.data.dblptr[elem] =
                                val1 - val2*((int)(val1/val2));
	       else {
		 this->value.data.dblptr[elem] = 0.0;
		 this->value.undef[elem] = 1;
	       }
	       break;
	    case '/': 
	       if( val2 ) this->value.data.dblptr[elem] = (val1 / val2); 
	       else {
		 this->value.data.dblptr[elem] = 0.0;
		 this->value.undef[elem] = 1;
	       }
	       break;
	    case POWER:
	       this->value.data.dblptr[elem] = (double)pow(val1,val2);
	       break;
	    }
	 }
	 nelem = this->value.nelem;
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.ptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.ptr );
   }
}

/*
 *  This Quickselect routine is based on the algorithm described in
 *  "Numerical recipes in C", Second Edition,
 *  Cambridge University Press, 1992, Section 8.5, ISBN 0-521-43108-5
 *  This code by Nicolas Devillard - 1998. Public domain.
 * http://ndevilla.free.fr/median/median/src/quickselect.c
 */

#define ELEM_SWAP(a,b) { register long t=(a);(a)=(b);(b)=t; }

/* 
 * qselect_median_lng - select the median value of a long array
 *
 * This routine selects the median value of the long integer array
 * arr[].  If there are an even number of elements, the "lower median"
 * is selected.
 *
 * The array arr[] is scrambled, so users must operate on a scratch
 * array if they wish the values to be preserved.
 *
 * long arr[] - array of values
 * int n - number of elements in arr
 *
 * RETURNS: the lower median value of arr[]
 *
 */
long qselect_median_lng(long arr[], int n)
{
    int low, high ;
    int median;
    int middle, ll, hh;

    low = 0 ; high = n-1 ; median = (low + high) / 2;
    for (;;) {

        if (high <= low) { /* One element only */
	  return arr[median];	  
	}

        if (high == low + 1) {  /* Two elements only */
            if (arr[low] > arr[high])
                ELEM_SWAP(arr[low], arr[high]) ;
	    return arr[median];
        }

    /* Find median of low, middle and high items; swap into position low */
    middle = (low + high) / 2;
    if (arr[middle] > arr[high])    ELEM_SWAP(arr[middle], arr[high]) ;
    if (arr[low] > arr[high])       ELEM_SWAP(arr[low], arr[high]) ;
    if (arr[middle] > arr[low])     ELEM_SWAP(arr[middle], arr[low]) ;

    /* Swap low item (now in position middle) into position (low+1) */
    ELEM_SWAP(arr[middle], arr[low+1]) ;

    /* Nibble from each end towards middle, swapping items when stuck */
    ll = low + 1;
    hh = high;
    for (;;) {
        do ll++; while (arr[low] > arr[ll]) ;
        do hh--; while (arr[hh]  > arr[low]) ;

        if (hh < ll)
        break;

        ELEM_SWAP(arr[ll], arr[hh]) ;
    }

    /* Swap middle item (in position low) back into correct position */
    ELEM_SWAP(arr[low], arr[hh]) ;

    /* Re-set active partition */
    if (hh <= median)
        low = ll;
        if (hh >= median)
        high = hh - 1;
    }
}

#undef ELEM_SWAP

#define ELEM_SWAP(a,b) { register double t=(a);(a)=(b);(b)=t; }

/* 
 * qselect_median_dbl - select the median value of a double array
 *
 * This routine selects the median value of the double array
 * arr[].  If there are an even number of elements, the "lower median"
 * is selected.
 *
 * The array arr[] is scrambled, so users must operate on a scratch
 * array if they wish the values to be preserved.
 *
 * double arr[] - array of values
 * int n - number of elements in arr
 *
 * RETURNS: the lower median value of arr[]
 *
 */
double qselect_median_dbl(double arr[], int n)
{
    int low, high ;
    int median;
    int middle, ll, hh;

    low = 0 ; high = n-1 ; median = (low + high) / 2;
    for (;;) {
        if (high <= low) { /* One element only */
            return arr[median] ;
	}

        if (high == low + 1) {  /* Two elements only */
            if (arr[low] > arr[high])
                ELEM_SWAP(arr[low], arr[high]) ;
            return arr[median] ;
        }

    /* Find median of low, middle and high items; swap into position low */
    middle = (low + high) / 2;
    if (arr[middle] > arr[high])    ELEM_SWAP(arr[middle], arr[high]) ;
    if (arr[low] > arr[high])       ELEM_SWAP(arr[low], arr[high]) ;
    if (arr[middle] > arr[low])     ELEM_SWAP(arr[middle], arr[low]) ;

    /* Swap low item (now in position middle) into position (low+1) */
    ELEM_SWAP(arr[middle], arr[low+1]) ;

    /* Nibble from each end towards middle, swapping items when stuck */
    ll = low + 1;
    hh = high;
    for (;;) {
        do ll++; while (arr[low] > arr[ll]) ;
        do hh--; while (arr[hh]  > arr[low]) ;

        if (hh < ll)
        break;

        ELEM_SWAP(arr[ll], arr[hh]) ;
    }

    /* Swap middle item (in position low) back into correct position */
    ELEM_SWAP(arr[low], arr[hh]) ;

    /* Re-set active partition */
    if (hh <= median)
        low = ll;
        if (hh >= median)
        high = hh - 1;
    }
}

#undef ELEM_SWAP

/*
 * angsep_calc - compute angular separation between celestial coordinates
 *   
 * This routine computes the angular separation between to coordinates
 * on the celestial sphere (i.e. RA and Dec).  Note that all units are
 * in DEGREES, unlike the other trig functions in the calculator.
 *
 * double ra1, dec1 - RA and Dec of the first position in degrees
 * double ra2, dec2 - RA and Dec of the second position in degrees
 * 
 * RETURNS: (double) angular separation in degrees
 *
 */
double angsep_calc(double ra1, double dec1, double ra2, double dec2)
{
/*  double cd;  */
  static double deg = 0;
  double a, sdec, sra;
  
  if (deg == 0) deg = ((double)4)*atan((double)1)/((double)180);
  /* deg = 1.0; **** UNCOMMENT IF YOU WANT RADIANS */

  /* The algorithm is the law of Haversines.  This algorithm is
     stable even when the points are close together.  The normal
     Law of Cosines fails for angles around 0.1 arcsec. */

  sra  = sin( (ra2 - ra1)*deg / 2 );
  sdec = sin( (dec2 - dec1)*deg / 2);
  a = sdec*sdec + cos(dec1*deg)*cos(dec2*deg)*sra*sra;

  /* Sanity checking to avoid a range error in the sqrt()'s below */
  if (a < 0) { a = 0; }
  if (a > 1) { a = 1; }

  return 2.0*atan2(sqrt(a), sqrt(1.0 - a)) / deg;
}

static void Do_Func( ParseData *lParse, Node *this )
{
   Node *theParams[MAXSUBS];
   int  vector[MAXSUBS], allConst;
   lval pVals[MAXSUBS];
   char pNull[MAXSUBS];
   long   ival;
   double dval;
   int  i, valInit;
   long row, elem, nelem;

   i = this->nSubNodes;
   allConst = 1;
   while( i-- ) {
      theParams[i] = lParse->Nodes + this->SubNodes[i];
      vector[i]   = ( theParams[i]->operation!=CONST_OP );
      if( vector[i] ) {
	 allConst = 0;
	 vector[i] = theParams[i]->value.nelem;
      } else {
	 if( theParams[i]->type==DOUBLE ) {
	    pVals[i].data.dbl = theParams[i]->value.data.dbl;
	 } else if( theParams[i]->type==LONG ) {
	    pVals[i].data.lng = theParams[i]->value.data.lng;
	 } else if( theParams[i]->type==BOOLEAN ) {
	    pVals[i].data.log = theParams[i]->value.data.log;
	 } else
	    strcpy(pVals[i].data.str, theParams[i]->value.data.str);
	 pNull[i] = 0;
      }
   }

   if( this->nSubNodes==0 ) allConst = 0; /* These do produce scalars */
   /* Random numbers are *never* constant !! */
   if( this->operation == poirnd_fct ) allConst = 0;
   if( this->operation == gasrnd_fct ) allConst = 0;
   if( this->operation == rnd_fct ) allConst = 0;

   if( allConst ) {

      switch( this->operation ) {

	    /* Non-Trig single-argument functions */

	 case sum_fct:
	    if( theParams[0]->type==BOOLEAN )
	       this->value.data.lng = ( pVals[0].data.log ? 1 : 0 );
	    else if( theParams[0]->type==LONG )
	       this->value.data.lng = pVals[0].data.lng;
	    else if( theParams[0]->type==DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    else if( theParams[0]->type==BITSTR )
	      strcpy(this->value.data.str, pVals[0].data.str);
	    break;
         case average_fct:
	    if( theParams[0]->type==LONG )
	       this->value.data.dbl = pVals[0].data.lng;
	    else if( theParams[0]->type==DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    break;
         case stddev_fct:
	    this->value.data.dbl = 0;  /* Standard deviation of a constant = 0 */
	    break;
	 case median_fct:
	    if( theParams[0]->type==BOOLEAN )
	       this->value.data.lng = ( pVals[0].data.log ? 1 : 0 );
	    else if( theParams[0]->type==LONG )
	       this->value.data.lng = pVals[0].data.lng;
	    else
	       this->value.data.dbl = pVals[0].data.dbl;
	    break;

	 case poirnd_fct:
	    if( theParams[0]->type==DOUBLE )
	      this->value.data.lng = simplerng_getpoisson(pVals[0].data.dbl);
	    else
	      this->value.data.lng = simplerng_getpoisson(pVals[0].data.lng);
	    break;

	 case abs_fct:
	    if( theParams[0]->type==DOUBLE ) {
	       dval = pVals[0].data.dbl;
	       this->value.data.dbl = (dval>0.0 ? dval : -dval);
	    } else {
	       ival = pVals[0].data.lng;
	       this->value.data.lng = (ival> 0  ? ival : -ival);
	    }
	    break;

            /* Special Null-Handling Functions */

         case nonnull_fct:
	    this->value.data.lng = 1; /* Constants are always 1-element and defined */
	    break;
         case isnull_fct:  /* Constants are always defined */
	    this->value.data.log = 0;
	    break;
         case defnull_fct:
	    if( this->type==BOOLEAN )
	       this->value.data.log = pVals[0].data.log;
            else if( this->type==LONG )
	       this->value.data.lng = pVals[0].data.lng;
            else if( this->type==DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
            else if( this->type==STRING )
	       strcpy(this->value.data.str,pVals[0].data.str);
	    break;
        case setnull_fct: /* Only defined for numeric expressions */
            if( this->type==LONG )
 	      this->value.data.lng = pVals[0].data.lng;
            else if( this->type==DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    break;

	    /* Math functions with 1 double argument */

	 case sin_fct:
	    this->value.data.dbl = sin( pVals[0].data.dbl );
	    break;
	 case cos_fct:
	    this->value.data.dbl = cos( pVals[0].data.dbl );
	    break;
	 case tan_fct:
	    this->value.data.dbl = tan( pVals[0].data.dbl );
	    break;
	 case asin_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<-1.0 || dval>1.0 )
	       yyerror(0, lParse, "Out of range argument to arcsin");
	    else
	       this->value.data.dbl = asin( dval );
	    break;
	 case acos_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<-1.0 || dval>1.0 )
	       yyerror(0, lParse, "Out of range argument to arccos");
	    else
	       this->value.data.dbl = acos( dval );
	    break;
	 case atan_fct:
	    this->value.data.dbl = atan( pVals[0].data.dbl );
	    break;
	 case sinh_fct:
	    this->value.data.dbl = sinh( pVals[0].data.dbl );
	    break;
	 case cosh_fct:
	    this->value.data.dbl = cosh( pVals[0].data.dbl );
	    break;
	 case tanh_fct:
	    this->value.data.dbl = tanh( pVals[0].data.dbl );
	    break;
	 case exp_fct:
	    this->value.data.dbl = exp( pVals[0].data.dbl );
	    break;
	 case log_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<=0.0 )
	       yyerror(0, lParse, "Out of range argument to log");
	    else
	       this->value.data.dbl = log( dval );
	    break;
	 case log10_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<=0.0 )
	       yyerror(0, lParse, "Out of range argument to log10");
	    else
	       this->value.data.dbl = log10( dval );
	    break;
	 case sqrt_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<0.0 )
	       yyerror(0, lParse, "Out of range argument to sqrt");
	    else
	       this->value.data.dbl = sqrt( dval );
	    break;
	 case ceil_fct:
	    this->value.data.dbl = ceil( pVals[0].data.dbl );
	    break;
	 case floor_fct:
	    this->value.data.dbl = floor( pVals[0].data.dbl );
	    break;
	 case round_fct:
	    this->value.data.dbl = floor( pVals[0].data.dbl + 0.5 );
	    break;

	    /* Two-argument Trig Functions */

	 case atan2_fct:
	    this->value.data.dbl =
	       atan2( pVals[0].data.dbl, pVals[1].data.dbl );
	    break;

	    /* Four-argument ANGSEP function */
         case angsep_fct:
	    this->value.data.dbl = 
	      angsep_calc(pVals[0].data.dbl, pVals[1].data.dbl,
			  pVals[2].data.dbl, pVals[3].data.dbl);

	    /*  Min/Max functions taking 1 or 2 arguments  */

         case min1_fct:
	    /* No constant vectors! */
	    if( this->type == DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    else if( this->type == LONG )
	       this->value.data.lng = pVals[0].data.lng;
	    else if( this->type == BITSTR )
	      strcpy(this->value.data.str, pVals[0].data.str);
	    break;
         case min2_fct:
	    if( this->type == DOUBLE )
	       this->value.data.dbl =
		  minvalue( pVals[0].data.dbl, pVals[1].data.dbl );
	    else if( this->type == LONG )
	       this->value.data.lng =
		  minvalue( pVals[0].data.lng, pVals[1].data.lng );
	    break;
         case max1_fct:
	    /* No constant vectors! */
	    if( this->type == DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    else if( this->type == LONG )
	       this->value.data.lng = pVals[0].data.lng;
	    else if( this->type == BITSTR )
	      strcpy(this->value.data.str, pVals[0].data.str);
	    break;
         case max2_fct:
	    if( this->type == DOUBLE )
	       this->value.data.dbl =
		  maxvalue( pVals[0].data.dbl, pVals[1].data.dbl );
	    else if( this->type == LONG )
	       this->value.data.lng =
		  maxvalue( pVals[0].data.lng, pVals[1].data.lng );
	    break;

	    /* Boolean SAO region Functions... scalar or vector dbls */

	 case near_fct:
	    this->value.data.log = bnear( pVals[0].data.dbl, pVals[1].data.dbl,
					  pVals[2].data.dbl );
	    break;
	 case circle_fct:
	    this->value.data.log = circle( pVals[0].data.dbl, pVals[1].data.dbl,
					   pVals[2].data.dbl, pVals[3].data.dbl,
					   pVals[4].data.dbl );
	    break;
	 case box_fct:
	    this->value.data.log = saobox( pVals[0].data.dbl, pVals[1].data.dbl,
					   pVals[2].data.dbl, pVals[3].data.dbl,
					   pVals[4].data.dbl, pVals[5].data.dbl,
					   pVals[6].data.dbl );
	    break;
	 case elps_fct:
	    this->value.data.log =
                               ellipse( pVals[0].data.dbl, pVals[1].data.dbl,
					pVals[2].data.dbl, pVals[3].data.dbl,
					pVals[4].data.dbl, pVals[5].data.dbl,
					pVals[6].data.dbl );
	    break;

            /* C Conditional expression:  bool ? expr : expr */

         case ifthenelse_fct:
            switch( this->type ) {
            case BOOLEAN:
               this->value.data.log = ( pVals[2].data.log ?
                                        pVals[0].data.log : pVals[1].data.log );
               break;
            case LONG:
               this->value.data.lng = ( pVals[2].data.log ?
                                        pVals[0].data.lng : pVals[1].data.lng );
               break;
            case DOUBLE:
               this->value.data.dbl = ( pVals[2].data.log ?
                                        pVals[0].data.dbl : pVals[1].data.dbl );
               break;
            case STRING:
	       strcpy(this->value.data.str, ( pVals[2].data.log ?
                                              pVals[0].data.str :
                                              pVals[1].data.str ) );
               break;
            }
            break;

	    /* String functions */
         case strmid_fct:
	   cstrmid(lParse, 
		   this->value.data.str, this->value.nelem, 
		   pVals[0].data.str,    pVals[0].nelem,
		   pVals[1].data.lng);
	   break;
         case strpos_fct:
	   {
	     char *res = strstr(pVals[0].data.str, pVals[1].data.str);
	     if (res == NULL) {
	       this->value.data.lng = 0; 
	     } else {
	       this->value.data.lng = (res - pVals[0].data.str) + 1;
	     }
	     break;
	   }

      }
      this->operation = CONST_OP;

   } else {

     Allocate_Ptrs( lParse, this );

      row  = lParse->nRows;
      elem = row * this->value.nelem;

      if( !lParse->status ) {
	 switch( this->operation ) {

	    /* Special functions with no arguments */

	 case row_fct:
	    while( row-- ) {
	       this->value.data.lngptr[row] = lParse->firstRow + row;
	       this->value.undef[row] = 0;
	    }
	    break;
	 case null_fct:
            if( this->type==LONG ) {
               while( row-- ) {
                  this->value.data.lngptr[row] = 0;
                  this->value.undef[row] = 1;
               }
            } else if( this->type==STRING ) {
               while( row-- ) {
                  this->value.data.strptr[row][0] = '\0';
                  this->value.undef[row] = 1;
               }
            }
	    break;
	 case axiselem_fct:
	   {
	     long ielem;
	     long iaxis[MAXDIMS] = {1, 1, 1, 1, 1};
	     long ipos = pVals[1].data.lng - 1; /* This should be a constant long value */
	     int naxis = this->value.naxis;
	     int j;
	     if (ipos < 0 || ipos >= MAXDIMS) {
	         yyerror(0, lParse, "AXISELEM(V,n) n value exceeded maximum dimension");
		 free( this->value.data.ptr );
		 break;
	     }

	     for (ielem = 0; ielem<elem; ielem++) {
	       this->value.data.lngptr[ielem] = iaxis[ipos];
	       this->value.undef[ielem] = 0;
	       iaxis[0]++;
	       for (j = 0; j < naxis; j++) {
		 if (iaxis[j] > this->value.naxes[j]) { 
		   iaxis[j] = 1; 
		   if (j < (naxis-1)) iaxis[j+1]++;
		 } else {
		   break;
		 }
	       }

	     }
	   }
	   break;
	 case elemnum_fct:
	   {
	     long ielem;
	     long elemnum = 1;
	     int j;

	     for (ielem = 0; ielem<elem; ielem++) {
	       this->value.data.lngptr[ielem] = elemnum;
	       this->value.undef[ielem] = 0;
	       elemnum ++;
	       if (elemnum > this->value.nelem) elemnum = 1;
	     }
	   }
	   break;
	 case rnd_fct:
	   while( elem-- ) {
	     this->value.data.dblptr[elem] = simplerng_getuniform();
	     this->value.undef[elem] = 0;
	    }
	    break;

	 case gasrnd_fct:
	    while( elem-- ) {
	       this->value.data.dblptr[elem] = simplerng_getnorm();
	       this->value.undef[elem] = 0;
	    }
	    break;

	 case poirnd_fct:
	   if( theParams[0]->type==DOUBLE ) {
	      if (theParams[0]->operation == CONST_OP) {
		while( elem-- ) {
		  this->value.undef[elem] = (pVals[0].data.dbl < 0);
		  if (! this->value.undef[elem]) {
		    this->value.data.lngptr[elem] = simplerng_getpoisson(pVals[0].data.dbl);
		  }
		} 
	      } else {
		while( elem-- ) {
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
		  if (theParams[0]->value.data.dblptr[elem] < 0) 
		    this->value.undef[elem] = 1;
		  if (! this->value.undef[elem]) {
		    this->value.data.lngptr[elem] = 
		      simplerng_getpoisson(theParams[0]->value.data.dblptr[elem]);
		  }
		} /* while */
	      } /* ! CONST_OP */
	   } else {
	     /* LONG */
	      if (theParams[0]->operation == CONST_OP) {
		while( elem-- ) {
		  this->value.undef[elem] = (pVals[0].data.lng < 0);
		  if (! this->value.undef[elem]) {
		    this->value.data.lngptr[elem] = simplerng_getpoisson(pVals[0].data.lng);
		  }
		} 
	      } else {
		while( elem-- ) {
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
		  if (theParams[0]->value.data.lngptr[elem] < 0) 
		    this->value.undef[elem] = 1;
		  if (! this->value.undef[elem]) {
		    this->value.data.lngptr[elem] = 
		      simplerng_getpoisson(theParams[0]->value.data.lngptr[elem]);
		  }
		} /* while */
	      } /* ! CONST_OP */
	   } /* END LONG */
	   break;


	    /* Non-Trig single-argument functions */
	    
	 case sum_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( theParams[0]->type==BOOLEAN ) {
	       while( row-- ) {
		  this->value.data.lngptr[row] = 0;
		  /* Default is UNDEF until a defined value is found */
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( ! theParams[0]->value.undef[elem] ) {
		       this->value.data.lngptr[row] +=
			 ( theParams[0]->value.data.logptr[elem] ? 1 : 0 );
		       this->value.undef[row] = 0;
		     }
		  }
	       }
	    } else if( theParams[0]->type==LONG ) {
	       while( row-- ) {
		  this->value.data.lngptr[row] = 0;
		  /* Default is UNDEF until a defined value is found */
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( ! theParams[0]->value.undef[elem] ) {
		       this->value.data.lngptr[row] +=
			 theParams[0]->value.data.lngptr[elem];
		       this->value.undef[row] = 0;
		     }
		  }
	       }		  
	    } else if( theParams[0]->type==DOUBLE ){
	       while( row-- ) {
		  this->value.data.dblptr[row] = 0.0;
		  /* Default is UNDEF until a defined value is found */
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( ! theParams[0]->value.undef[elem] ) {
		       this->value.data.dblptr[row] +=
			 theParams[0]->value.data.dblptr[elem];
		       this->value.undef[row] = 0;
		     }
		  }
	       }		  
	    } else { /* BITSTR */
	       nelem = theParams[0]->value.nelem;
	       while( row-- ) {
		  char *sptr1 = theParams[0]->value.data.strptr[row];
		  this->value.data.lngptr[row] = 0;
		  this->value.undef[row] = 0;
		  while (*sptr1) {
		    if (*sptr1 == '1') this->value.data.lngptr[row] ++;
		    sptr1++;
		  }
	       }		  
	    }
	    break;

	 case average_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( theParams[0]->type==LONG ) {
	       while( row-- ) {
		  int count = 0;
		  this->value.data.dblptr[row] = 0;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if (theParams[0]->value.undef[elem] == 0) {
		       this->value.data.dblptr[row] +=
			 theParams[0]->value.data.lngptr[elem];
		       count ++;
		     }
		  }
		  if (count == 0) {
		    this->value.undef[row] = 1;
		  } else {
		    this->value.undef[row] = 0;
		    this->value.data.dblptr[row] /= count;
		  }
	       }		  
	    } else if( theParams[0]->type==DOUBLE ){
	       while( row-- ) {
		  int count = 0;
		  this->value.data.dblptr[row] = 0;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if (theParams[0]->value.undef[elem] == 0) {
		       this->value.data.dblptr[row] +=
			 theParams[0]->value.data.dblptr[elem];
		       count ++;
		     }
		  }
		  if (count == 0) {
		    this->value.undef[row] = 1;
		  } else {
		    this->value.undef[row] = 0;
		    this->value.data.dblptr[row] /= count;
		  }
	       }		  
	    }
	    break;
	 case stddev_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( theParams[0]->type==LONG ) {

	       /* Compute the mean value */
	       while( row-- ) {
		  int count = 0;
		  double sum = 0, sum2 = 0;

		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if (theParams[0]->value.undef[elem] == 0) {
		       sum += theParams[0]->value.data.lngptr[elem];
		       count ++;
		     }
		  }
		  if (count > 1) {
		    sum /= count;

		    /* Compute the sum of squared deviations */
		    nelem = theParams[0]->value.nelem;
		    elem += nelem;  /* Reset elem for second pass */
		    while( nelem-- ) {
		      elem--;
		      if (theParams[0]->value.undef[elem] == 0) {
			double dx = (theParams[0]->value.data.lngptr[elem] - sum);
			sum2 += (dx*dx);
		      }
		    }

		    sum2 /= (double)count-1;

		    this->value.undef[row] = 0;
		    this->value.data.dblptr[row] = sqrt(sum2);
		  } else {
		    this->value.undef[row] = 0;       /* STDDEV => 0 */
		    this->value.data.dblptr[row] = 0;
		  }
	       }
	    } else if( theParams[0]->type==DOUBLE ){

	       /* Compute the mean value */
	       while( row-- ) {
		  int count = 0;
		  double sum = 0, sum2 = 0;

		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if (theParams[0]->value.undef[elem] == 0) {
		       sum += theParams[0]->value.data.dblptr[elem];
		       count ++;
		     }
		  }
		  if (count > 1) {
		    sum /= count;

		    /* Compute the sum of squared deviations */
		    nelem = theParams[0]->value.nelem;
		    elem += nelem;  /* Reset elem for second pass */
		    while( nelem-- ) {
		      elem--;
		      if (theParams[0]->value.undef[elem] == 0) {
			double dx = (theParams[0]->value.data.dblptr[elem] - sum);
			sum2 += (dx*dx);
		      }
		    }

		    sum2 /= (double)count-1;

		    this->value.undef[row] = 0;
		    this->value.data.dblptr[row] = sqrt(sum2);
		  } else {
		    this->value.undef[row] = 0;       /* STDDEV => 0 */
		    this->value.data.dblptr[row] = 0;
		  }
	       }
	    }
	    break;

	 case median_fct:
	   elem = row * theParams[0]->value.nelem;
	   nelem = theParams[0]->value.nelem;
	   if( theParams[0]->type==LONG ) {
	       long *dptr = theParams[0]->value.data.lngptr;
	       char *uptr = theParams[0]->value.undef;
	       long *mptr = (long *) malloc(sizeof(long)*nelem);
	       int irow;

	       /* Allocate temporary storage for this row, since the
                  quickselect function will scramble the contents */
	       if (mptr == 0) {
		 yyerror(0, lParse, "Could not allocate temporary memory in median function");
		 free( this->value.data.ptr );
		 break;
	       }

	       for (irow=0; irow<row; irow++) {
		  long *p = mptr;
		  int nelem1 = nelem;


		  while ( nelem1-- ) { 
		    if (*uptr == 0) {
		      *p++ = *dptr;   /* Only advance the dest pointer if we copied */
		    }
		    dptr ++;  /* Advance the source pointer ... */
		    uptr ++;  /* ... and source "undef" pointer */
		  }
		  
		  nelem1 = (p - mptr);  /* Number of accepted data points */
		  if (nelem1 > 0) {
		    this->value.undef[irow] = 0;
		    this->value.data.lngptr[irow] = qselect_median_lng(mptr, nelem1);
		  } else {
		    this->value.undef[irow] = 1;
		    this->value.data.lngptr[irow] = 0;
		  }
		    
	       }		  

	       free(mptr);
	    } else {
	       double *dptr = theParams[0]->value.data.dblptr;
	       char   *uptr = theParams[0]->value.undef;
	       double *mptr = (double *) malloc(sizeof(double)*nelem);
	       int irow;

	       /* Allocate temporary storage for this row, since the
                  quickselect function will scramble the contents */
	       if (mptr == 0) {
		 yyerror(0, lParse, "Could not allocate temporary memory in median function");
		 free( this->value.data.ptr );
		 break;
	       }

	       for (irow=0; irow<row; irow++) {
		  double *p = mptr;
		  int nelem1 = nelem;

		  while ( nelem1-- ) { 
		    if (*uptr == 0) {
		      *p++ = *dptr;   /* Only advance the dest pointer if we copied */
		    }
		    dptr ++;  /* Advance the source pointer ... */
		    uptr ++;  /* ... and source "undef" pointer */
		  }

		  nelem1 = (p - mptr);  /* Number of accepted data points */
		  if (nelem1 > 0) {
		    this->value.undef[irow] = 0;
		    this->value.data.dblptr[irow] = qselect_median_dbl(mptr, nelem1);
		  } else {
		    this->value.undef[irow] = 1;
		    this->value.data.dblptr[irow] = 0;
		  }

	       }
	       free(mptr);
	    }
	    break;
	 case abs_fct:
	    if( theParams[0]->type==DOUBLE )
	       while( elem-- ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  this->value.data.dblptr[elem] = (dval>0.0 ? dval : -dval);
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
	       }
	    else
	       while( elem-- ) {
		  ival = theParams[0]->value.data.lngptr[elem];
		  this->value.data.lngptr[elem] = (ival> 0  ? ival : -ival);
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
	       }
	    break;

            /* Special Null-Handling Functions */

	 case nonnull_fct:
	   nelem = theParams[0]->value.nelem;
	   if ( theParams[0]->type==STRING ) nelem = 1;
	   elem = row * nelem;
	   while( row-- ) {
	     int nelem1 = nelem;

	     this->value.undef[row] = 0;        /* Initialize to 0 (defined) */
	     this->value.data.lngptr[row] = 0;
	     while( nelem1-- ) {	
	       elem --;
	       if ( theParams[0]->value.undef[elem] == 0 ) this->value.data.lngptr[row] ++;
	     }
	   }
	   break;
	 case isnull_fct:
	    if( theParams[0]->type==STRING ) elem = row;
	    while( elem-- ) {
	       this->value.data.logptr[elem] = theParams[0]->value.undef[elem];
	       this->value.undef[elem] = 0;
	    }
	    break;
         case defnull_fct:
	    switch( this->type ) {
	    case BOOLEAN:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pNull[i] = theParams[i]->value.undef[elem];
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[elem];
			} else if( vector[i] ) {
			   pNull[i] = theParams[i]->value.undef[row];
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[row];
			}
		     if( pNull[0] ) {
			this->value.undef[elem] = pNull[1];
			this->value.data.logptr[elem] = pVals[1].data.log;
		     } else {
			this->value.undef[elem] = 0;
			this->value.data.logptr[elem] = pVals[0].data.log;
		     }
		  }
	       }
	       break;
	    case LONG:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pNull[i] = theParams[i]->value.undef[elem];
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[elem];
			} else if( vector[i] ) {
			   pNull[i] = theParams[i]->value.undef[row];
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[row];
			}
		     if( pNull[0] ) {
			this->value.undef[elem] = pNull[1];
			this->value.data.lngptr[elem] = pVals[1].data.lng;
		     } else {
			this->value.undef[elem] = 0;
			this->value.data.lngptr[elem] = pVals[0].data.lng;
		     }
		  }
	       }
	       break;
	    case DOUBLE:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pNull[i] = theParams[i]->value.undef[elem];
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[elem];
			} else if( vector[i] ) {
			   pNull[i] = theParams[i]->value.undef[row];
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[row];
			}
		     if( pNull[0] ) {
			this->value.undef[elem] = pNull[1];
			this->value.data.dblptr[elem] = pVals[1].data.dbl;
		     } else {
			this->value.undef[elem] = 0;
			this->value.data.dblptr[elem] = pVals[0].data.dbl;
		     }
		  }
	       }
	       break;
	    case STRING:
	       while( row-- ) {
		  i=2; while( i-- )
		     if( vector[i] ) {
			pNull[i] = theParams[i]->value.undef[row];
			strcpy(pVals[i].data.str,
			       theParams[i]->value.data.strptr[row]);
		     }
		  if( pNull[0] ) {
		     this->value.undef[row] = pNull[1];
		     strcpy(this->value.data.strptr[row],pVals[1].data.str);
		  } else {
		     this->value.undef[elem] = 0;
		     strcpy(this->value.data.strptr[row],pVals[0].data.str);
		  }
	       }
	    }
	    break;
         case setnull_fct:
	    switch( this->type ) {
	    case LONG:
	      while( elem-- ) {
		if ( theParams[1]->value.data.lng == 
		     theParams[0]->value.data.lngptr[elem] ) {
		  this->value.data.lngptr[elem] = 0;
		  this->value.undef[elem] = 1;
		} else {
		  this->value.data.lngptr[elem] = theParams[0]->value.data.lngptr[elem];
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
		}
	      }
	      break;
	    case DOUBLE:
	      while( elem-- ) {
		if ( theParams[1]->value.data.dbl == 
		     theParams[0]->value.data.dblptr[elem] ) {
		  this->value.data.dblptr[elem] = 0;
		  this->value.undef[elem] = 1;
		} else {
		  this->value.data.dblptr[elem] = theParams[0]->value.data.dblptr[elem];
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
		}
	      }
	      break;
	    }
	    break;

	    /* Math functions with 1 double argument */

	 case sin_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     sin( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case cos_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     cos( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case tan_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     tan( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case asin_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<-1.0 || dval>1.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = asin( dval );
	       }
	    break;
	 case acos_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<-1.0 || dval>1.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = acos( dval );
	       }
	    break;
	 case atan_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  this->value.data.dblptr[elem] = atan( dval );
	       }
	    break;
	 case sinh_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     sinh( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case cosh_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     cosh( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case tanh_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     tanh( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case exp_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  this->value.data.dblptr[elem] = exp( dval );
	       }
	    break;
	 case log_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<=0.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = log( dval );
	       }
	    break;
	 case log10_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<=0.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = log10( dval );
	       }
	    break;
	 case sqrt_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<0.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = sqrt( dval );
	       }
	    break;
	 case ceil_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     ceil( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case floor_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     floor( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case round_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     floor( theParams[0]->value.data.dblptr[elem] + 0.5);
	       }
	    break;

	    /* Two-argument Trig Functions */
	    
	 case atan2_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=2; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1]) ) )
		     this->value.data.dblptr[elem] =
			atan2( pVals[0].data.dbl, pVals[1].data.dbl );
	       }
	    }
	    break;

	    /* Four-argument ANGSEP Function */
	    
	 case angsep_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=4; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2] || pNull[3]) ) )
		     this->value.data.dblptr[elem] =
		       angsep_calc(pVals[0].data.dbl, pVals[1].data.dbl,
				   pVals[2].data.dbl, pVals[3].data.dbl);
	       }
	    }
	    break;



	    /*  Min/Max functions taking 1 or 2 arguments  */

         case min1_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( this->type==LONG ) {
	       long minVal=0;
	       while( row-- ) {
		  valInit = 1;
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( !theParams[0]->value.undef[elem] ) {
		       if ( valInit ) {
			 valInit = 0;
			 minVal  = theParams[0]->value.data.lngptr[elem];
		       } else {
			 minVal  = minvalue( minVal,
					     theParams[0]->value.data.lngptr[elem] );
		       }
		       this->value.undef[row] = 0;
		     }
		  }  
		  this->value.data.lngptr[row] = minVal;
	       }		  
	    } else if( this->type==DOUBLE ) {
	       double minVal=0.0;
	       while( row-- ) {
		  valInit = 1;
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( !theParams[0]->value.undef[elem] ) {
		       if ( valInit ) {
			 valInit = 0;
			 minVal  = theParams[0]->value.data.dblptr[elem];
		       } else {
			 minVal  = minvalue( minVal,
					     theParams[0]->value.data.dblptr[elem] );
		       }
		       this->value.undef[row] = 0;
		     }
		  }  
		  this->value.data.dblptr[row] = minVal;
	       }		  
	    } else if( this->type==BITSTR ) {
	       char minVal;
	       while( row-- ) {
		  char *sptr1 = theParams[0]->value.data.strptr[row];
		  minVal = '1';
		  while (*sptr1) {
		    if (*sptr1 == '0') minVal = '0';
		    sptr1++;
		  }
		  this->value.data.strptr[row][0] = minVal;
		  this->value.data.strptr[row][1] = 0;     /* Null terminate */
	       }		  
	    }
	    break;
         case min2_fct:
	    if( this->type==LONG ) {
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( pNull[0] && pNull[1] ) {
		       this->value.undef[elem] = 1;
		       this->value.data.lngptr[elem] = 0;
		     } else if (pNull[0]) {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] = pVals[1].data.lng;
		     } else if (pNull[1]) {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] = pVals[0].data.lng;
		     } else {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] =
			 minvalue( pVals[0].data.lng, pVals[1].data.lng );
		     }
		  }
	       }
	    } else if( this->type==DOUBLE ) {
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( pNull[0] && pNull[1] ) {
		       this->value.undef[elem] = 1;
		       this->value.data.dblptr[elem] = 0;
		     } else if (pNull[0]) {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] = pVals[1].data.dbl;
		     } else if (pNull[1]) {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] = pVals[0].data.dbl;
		     } else {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] =
			 minvalue( pVals[0].data.dbl, pVals[1].data.dbl );
		     }
		  }
 	       }
	    }
	    break;

         case max1_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( this->type==LONG ) {
	       long maxVal=0;
	       while( row-- ) {
		  valInit = 1;
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( !theParams[0]->value.undef[elem] ) {
		       if ( valInit ) {
			 valInit = 0;
			 maxVal  = theParams[0]->value.data.lngptr[elem];
		       } else {
			 maxVal  = maxvalue( maxVal,
					     theParams[0]->value.data.lngptr[elem] );
		       }
		       this->value.undef[row] = 0;
		     }
		  }
		  this->value.data.lngptr[row] = maxVal;
	       }		  
	    } else if( this->type==DOUBLE ) {
	       double maxVal=0.0;
	       while( row-- ) {
		  valInit = 1;
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( !theParams[0]->value.undef[elem] ) {
		       if ( valInit ) {
			 valInit = 0;
			 maxVal  = theParams[0]->value.data.dblptr[elem];
		       } else {
			 maxVal  = maxvalue( maxVal,
					     theParams[0]->value.data.dblptr[elem] );
		       }
		       this->value.undef[row] = 0;
		     }
		  }
		  this->value.data.dblptr[row] = maxVal;
	       }		  
	    } else if( this->type==BITSTR ) {
	       char maxVal;
	       while( row-- ) {
		  char *sptr1 = theParams[0]->value.data.strptr[row];
		  maxVal = '0';
		  while (*sptr1) {
		    if (*sptr1 == '1') maxVal = '1';
		    sptr1++;
		  }
		  this->value.data.strptr[row][0] = maxVal;
		  this->value.data.strptr[row][1] = 0;     /* Null terminate */
	       }		  
	    }
	    break;
         case max2_fct:
	    if( this->type==LONG ) {
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( pNull[0] && pNull[1] ) {
		       this->value.undef[elem] = 1;
		       this->value.data.lngptr[elem] = 0;
		     } else if (pNull[0]) {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] = pVals[1].data.lng;
		     } else if (pNull[1]) {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] = pVals[0].data.lng;
		     } else {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] =
			 maxvalue( pVals[0].data.lng, pVals[1].data.lng );
		     }
		  }
	       }
	    } else if( this->type==DOUBLE ) {
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( pNull[0] && pNull[1] ) {
		       this->value.undef[elem] = 1;
		       this->value.data.dblptr[elem] = 0;
		     } else if (pNull[0]) {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] = pVals[1].data.dbl;
		     } else if (pNull[1]) {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] = pVals[0].data.dbl;
		     } else {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] =
			 maxvalue( pVals[0].data.dbl, pVals[1].data.dbl );
		     }
		  }
	       }
	    }
	    break;

	    /* Boolean SAO region Functions... scalar or vector dbls */

	 case near_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=3; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2]) ) )
		    this->value.data.logptr[elem] =
		      bnear( pVals[0].data.dbl, pVals[1].data.dbl,
			     pVals[2].data.dbl );
	       }
	    }
	    break;

	 case circle_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=5; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2] || pNull[3] ||
						   pNull[4]) ) )
		    this->value.data.logptr[elem] =
		     circle( pVals[0].data.dbl, pVals[1].data.dbl,
			     pVals[2].data.dbl, pVals[3].data.dbl,
			     pVals[4].data.dbl );
	       }
	    }
	    break;

	 case box_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=7; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2] || pNull[3] ||
						   pNull[4] || pNull[5] ||
						   pNull[6] ) ) )
		    this->value.data.logptr[elem] =
		     saobox( pVals[0].data.dbl, pVals[1].data.dbl,
			     pVals[2].data.dbl, pVals[3].data.dbl,
			     pVals[4].data.dbl, pVals[5].data.dbl,
			     pVals[6].data.dbl );	
	       }
	    }
	    break;

	 case elps_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=7; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2] || pNull[3] ||
						   pNull[4] || pNull[5] ||
						   pNull[6] ) ) )
		    this->value.data.logptr[elem] =
		     ellipse( pVals[0].data.dbl, pVals[1].data.dbl,
			      pVals[2].data.dbl, pVals[3].data.dbl,
			      pVals[4].data.dbl, pVals[5].data.dbl,
			      pVals[6].data.dbl );
	       }
	    }
	    break;

            /* C Conditional expression:  bool ? expr : expr */

         case ifthenelse_fct:
            switch( this->type ) {
            case BOOLEAN:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
                     if( vector[2]>1 ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[elem];
                        pNull[2] = theParams[2]->value.undef[elem];
                     } else if( vector[2] ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[row];
                        pNull[2] = theParams[2]->value.undef[row];
                     }
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( !(this->value.undef[elem] = pNull[2]) ) {
                        if( pVals[2].data.log ) {
                           this->value.data.logptr[elem] = pVals[0].data.log;
                           this->value.undef[elem]       = pNull[0];
                        } else {
                           this->value.data.logptr[elem] = pVals[1].data.log;
                           this->value.undef[elem]       = pNull[1];
                        }
                     }
		  }
	       }
               break;
            case LONG:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
                     if( vector[2]>1 ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[elem];
                        pNull[2] = theParams[2]->value.undef[elem];
                     } else if( vector[2] ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[row];
                        pNull[2] = theParams[2]->value.undef[row];
                     }
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( !(this->value.undef[elem] = pNull[2]) ) {
                        if( pVals[2].data.log ) {
                           this->value.data.lngptr[elem] = pVals[0].data.lng;
                           this->value.undef[elem]       = pNull[0];
                        } else {
                           this->value.data.lngptr[elem] = pVals[1].data.lng;
                           this->value.undef[elem]       = pNull[1];
                        }
                     }
		  }
	       }
               break;
            case DOUBLE:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
                     if( vector[2]>1 ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[elem];
                        pNull[2] = theParams[2]->value.undef[elem];
                     } else if( vector[2] ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[row];
                        pNull[2] = theParams[2]->value.undef[row];
                     }
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( !(this->value.undef[elem] = pNull[2]) ) {
                        if( pVals[2].data.log ) {
                           this->value.data.dblptr[elem] = pVals[0].data.dbl;
                           this->value.undef[elem]       = pNull[0];
                        } else {
                           this->value.data.dblptr[elem] = pVals[1].data.dbl;
                           this->value.undef[elem]       = pNull[1];
                        }
                     }
		  }
	       }
               break;
            case STRING:
	       while( row-- ) {
                  if( vector[2] ) {
                     pVals[2].data.log = theParams[2]->value.data.logptr[row];
                     pNull[2] = theParams[2]->value.undef[row];
                  }
                  i=2; while( i-- )
                     if( vector[i] ) {
                        strcpy( pVals[i].data.str,
                                theParams[i]->value.data.strptr[row] );
                        pNull[i] = theParams[i]->value.undef[row];
                     }
                  if( !(this->value.undef[row] = pNull[2]) ) {
                     if( pVals[2].data.log ) {
                        strcpy( this->value.data.strptr[row],
                                pVals[0].data.str );
                        this->value.undef[row]       = pNull[0];
                     } else {
                        strcpy( this->value.data.strptr[row],
                                pVals[1].data.str );
                        this->value.undef[row]       = pNull[1];
                     }
                  } else {
                     this->value.data.strptr[row][0] = '\0';
                  }
	       }
               break;

            }
            break;

	    /* String functions */
            case strmid_fct:
	      {
		int strconst = theParams[0]->operation == CONST_OP;
		int posconst = theParams[1]->operation == CONST_OP;
		int lenconst = theParams[2]->operation == CONST_OP;
		int dest_len = this->value.nelem;
		int src_len  = theParams[0]->value.nelem;

		while (row--) {
		  int pos;
		  int len;
		  char *str;
		  int undef = 0;

		  if (posconst) {
		    pos = theParams[1]->value.data.lng;
		  } else {
		    pos = theParams[1]->value.data.lngptr[row];
		    if (theParams[1]->value.undef[row]) undef = 1;
		  }
		  if (strconst) {
		    str = theParams[0]->value.data.str;
		    if (src_len == 0) src_len = strlen(str);
		  } else {
		    str = theParams[0]->value.data.strptr[row];
		    if (theParams[0]->value.undef[row]) undef = 1;
		  }
		  if (lenconst) {
		    len = dest_len;
		  } else {
		    len = theParams[2]->value.data.lngptr[row];
		    if (theParams[2]->value.undef[row]) undef = 1;
		  }
		  this->value.data.strptr[row][0] = '\0';
		  if (pos == 0) undef = 1;
		  if (! undef ) {
		    if (cstrmid(lParse,
				this->value.data.strptr[row], len,
				str, src_len, pos) < 0) break;
		  }
		  this->value.undef[row] = undef;
		}
	      }		      
	      break;

	    /* String functions */
            case strpos_fct:
	      {
		int const1 = theParams[0]->operation == CONST_OP;
		int const2 = theParams[1]->operation == CONST_OP;

		while (row--) {
		  char *str1, *str2;
		  int undef = 0;

		  if (const1) {
		    str1 = theParams[0]->value.data.str;
		  } else {
		    str1 = theParams[0]->value.data.strptr[row];
		    if (theParams[0]->value.undef[row]) undef = 1;
		  }
		  if (const2) {
		    str2 = theParams[1]->value.data.str;
		  } else {
		    str2 = theParams[1]->value.data.strptr[row];
		    if (theParams[1]->value.undef[row]) undef = 1;
		  }
		  this->value.data.lngptr[row] = 0;
		  if (! undef ) {
		    char *res = strstr(str1, str2);
		    if (res == NULL) {
		      undef = 1;
		      this->value.data.lngptr[row] = 0; 
		    } else {
		      this->value.data.lngptr[row] = (res - str1) + 1;
		    }
		  }
		  this->value.undef[row] = undef;
		}
	      }
	      break;

		    
	 } /* End switch(this->operation) */
      } /* End if (!lParse->status) */
   } /* End non-constant operations */

   i = this->nSubNodes;
   while( i-- ) {
      if( theParams[i]->operation>0 ) {
	 /*  Currently only numeric params allowed  */
	 free( theParams[i]->value.data.ptr );
      }
   }
}

static void Do_Deref( ParseData *lParse, Node *this )
{
   Node *theVar, *theDims[MAXDIMS];
   int  isConst[MAXDIMS], allConst;
   long dimVals[MAXDIMS];
   int  i, nDims;
   long row, elem, dsize;

   theVar = lParse->Nodes + this->SubNodes[0];

   i = nDims = this->nSubNodes-1;
   allConst = 1;
   while( i-- ) {
      theDims[i] = lParse->Nodes + this->SubNodes[i+1];
      isConst[i] = ( theDims[i]->operation==CONST_OP );
      if( isConst[i] )
	 dimVals[i] = theDims[i]->value.data.lng;
      else
	 allConst = 0;
   }

   if( this->type==DOUBLE ) {
      dsize = sizeof( double );
   } else if( this->type==LONG ) {
      dsize = sizeof( long );
   } else if( this->type==BOOLEAN ) {
      dsize = sizeof( char );
   } else
      dsize = 0;

   Allocate_Ptrs( lParse, this );

   if( !lParse->status ) {

      if( allConst && theVar->value.naxis==nDims ) {

	 /* Dereference completely using constant indices */

	 elem = 0;
	 i    = nDims;
	 while( i-- ) {
	    if( dimVals[i]<1 || dimVals[i]>theVar->value.naxes[i] ) break;
	    elem = theVar->value.naxes[i]*elem + dimVals[i]-1;
	 }
	 if( i<0 ) {
	    for( row=0; row<lParse->nRows; row++ ) {
	       if( this->type==STRING )
		 this->value.undef[row] = theVar->value.undef[row];
	       else if( this->type==BITSTR ) 
		 this->value.undef;  /* Dummy - BITSTRs do not have undefs */
	       else 
		 this->value.undef[row] = theVar->value.undef[elem];

	       if( this->type==DOUBLE )
		  this->value.data.dblptr[row] = 
		     theVar->value.data.dblptr[elem];
	       else if( this->type==LONG )
		  this->value.data.lngptr[row] = 
		     theVar->value.data.lngptr[elem];
	       else if( this->type==BOOLEAN )
		  this->value.data.logptr[row] = 
		     theVar->value.data.logptr[elem];
	       else {
		 /* XXX Note, the below expression uses knowledge of
                    the layout of the string format, namely (nelem+1)
                    characters per string, followed by (nelem+1)
                    "undef" values. */
		  this->value.data.strptr[row][0] = 
		     theVar->value.data.strptr[0][elem+row];
		  this->value.data.strptr[row][1] = 0;  /* Null terminate */
	       }
	       elem += theVar->value.nelem;
	    }
	 } else {
	    yyerror(0, lParse, "Index out of range");
	    free( this->value.data.ptr );
	 }
	 
      } else if( allConst && nDims==1 ) {
	 
	 /* Reduce dimensions by 1, using a constant index */
	 
	 if( dimVals[0] < 1 ||
	     dimVals[0] > theVar->value.naxes[ theVar->value.naxis-1 ] ) {
	    yyerror(0, lParse, "Index out of range");
	    free( this->value.data.ptr );
	 } else if ( this->type == BITSTR || this->type == STRING ) {
	    elem = this->value.nelem * (dimVals[0]-1);
	    for( row=0; row<lParse->nRows; row++ ) {
	      if (this->value.undef) 
		this->value.undef[row] = theVar->value.undef[row];
	      memcpy( (char*)this->value.data.strptr[0]
		      + row*sizeof(char)*(this->value.nelem+1),
		      (char*)theVar->value.data.strptr[0] + elem*sizeof(char),
		      this->value.nelem * sizeof(char) );
	      /* Null terminate */
	      this->value.data.strptr[row][this->value.nelem] = 0;
	      elem += theVar->value.nelem+1;
	    }	       
	 } else {
	    elem = this->value.nelem * (dimVals[0]-1);
	    for( row=0; row<lParse->nRows; row++ ) {
	       memcpy( this->value.undef + row*this->value.nelem,
		       theVar->value.undef + elem,
		       this->value.nelem * sizeof(char) );
	       memcpy( (char*)this->value.data.ptr
		       + row*dsize*this->value.nelem,
		       (char*)theVar->value.data.ptr + elem*dsize,
		       this->value.nelem * dsize );
	       elem += theVar->value.nelem;
	    }	       
	 }
      
      } else if( theVar->value.naxis==nDims ) {

	 /* Dereference completely using an expression for the indices */

	 for( row=0; row<lParse->nRows; row++ ) {

	    for( i=0; i<nDims; i++ ) {
	       if( !isConst[i] ) {
		  if( theDims[i]->value.undef[row] ) {
		     yyerror(0, lParse, "Null encountered as vector index");
		     free( this->value.data.ptr );
		     break;
		  } else
		     dimVals[i] = theDims[i]->value.data.lngptr[row];
	       }
	    }
	    if( lParse->status ) break;

	    elem = 0;
	    i    = nDims;
	    while( i-- ) {
	       if( dimVals[i]<1 || dimVals[i]>theVar->value.naxes[i] ) break;
	       elem = theVar->value.naxes[i]*elem + dimVals[i]-1;
	    }
	    if( i<0 ) {
	       elem += row*theVar->value.nelem;

	       if( this->type==STRING )
		 this->value.undef[row] = theVar->value.undef[row];
	       else if( this->type==BITSTR ) 
		 this->value.undef;  /* Dummy - BITSTRs do not have undefs */
	       else 
		 this->value.undef[row] = theVar->value.undef[elem];

	       if( this->type==DOUBLE )
		  this->value.data.dblptr[row] = 
		     theVar->value.data.dblptr[elem];
	       else if( this->type==LONG )
		  this->value.data.lngptr[row] = 
		     theVar->value.data.lngptr[elem];
	       else if( this->type==BOOLEAN )
		  this->value.data.logptr[row] = 
		     theVar->value.data.logptr[elem];
	       else {
		 /* XXX Note, the below expression uses knowledge of
                    the layout of the string format, namely (nelem+1)
                    characters per string, followed by (nelem+1)
                    "undef" values. */
		  this->value.data.strptr[row][0] = 
		     theVar->value.data.strptr[0][elem+row];
		  this->value.data.strptr[row][1] = 0;  /* Null terminate */
	       }
	    } else {
	       yyerror(0, lParse, "Index out of range");
	       free( this->value.data.ptr );
	    }
	 }

      } else {

	 /* Reduce dimensions by 1, using a nonconstant expression */

	 for( row=0; row<lParse->nRows; row++ ) {

	    /* Index cannot be a constant */

	    if( theDims[0]->value.undef[row] ) {
	       yyerror(0, lParse, "Null encountered as vector index");
	       free( this->value.data.ptr );
	       break;
	    } else
	       dimVals[0] = theDims[0]->value.data.lngptr[row];

	    if( dimVals[0] < 1 ||
		dimVals[0] > theVar->value.naxes[ theVar->value.naxis-1 ] ) {
	       yyerror(0, lParse, "Index out of range");
	       free( this->value.data.ptr );
	    } else if ( this->type == BITSTR || this->type == STRING ) {
	      elem = this->value.nelem * (dimVals[0]-1);
	      elem += row*(theVar->value.nelem+1);
	      if (this->value.undef) 
		this->value.undef[row] = theVar->value.undef[row];
	      memcpy( (char*)this->value.data.strptr[0]
		      + row*sizeof(char)*(this->value.nelem+1),
		      (char*)theVar->value.data.strptr[0] + elem*sizeof(char),
		      this->value.nelem * sizeof(char) );
	      /* Null terminate */
	      this->value.data.strptr[row][this->value.nelem] = 0;
	    } else {
	       elem  = this->value.nelem * (dimVals[0]-1);
	       elem += row*theVar->value.nelem;
	       memcpy( this->value.undef + row*this->value.nelem,
		       theVar->value.undef + elem,
		       this->value.nelem * sizeof(char) );
	       memcpy( (char*)this->value.data.ptr
		       + row*dsize*this->value.nelem,
		       (char*)theVar->value.data.ptr + elem*dsize,
		       this->value.nelem * dsize );
	    }
	 }
      }
   }

   if( theVar->operation>0 ) {
     if (theVar->type == STRING || theVar->type == BITSTR) 
       free(theVar->value.data.strptr[0] );
     else 
       free( theVar->value.data.ptr );
   }
   for( i=0; i<nDims; i++ )
      if( theDims[i]->operation>0 ) {
	 free( theDims[i]->value.data.ptr );
      }
}

static void Do_GTI( ParseData *lParse, Node *this )
{
   Node *theExpr, *theTimes;
   double *start, *stop, *times;
   long elem, nGTI, gti;
   int ordered;
   int dorow = (this->operation == gtifind_fct);

   theTimes = lParse->Nodes + this->SubNodes[0];
   theExpr  = lParse->Nodes + this->SubNodes[1];

   nGTI    = theTimes->value.nelem;
   start   = theTimes->value.data.dblptr;
   stop    = theTimes->value.data.dblptr + nGTI;
   ordered = theTimes->type;

   if( theExpr->operation==CONST_OP ) {
      gti = Search_GTI( theExpr->value.data.dbl, nGTI, start, stop, ordered, 0 );
      if (dorow) {
	this->value.data.lng = (gti >= 0) ? (gti+1) : -1;
      } else {
	this->value.data.log = (gti>=0);
      }
      this->operation      = CONST_OP;

   } else {

      Allocate_Ptrs( lParse, this );

      times = theExpr->value.data.dblptr;
      if( !lParse->status ) {

	 elem = lParse->nRows * this->value.nelem;
	 if( nGTI ) {
	    gti = -1;
	    while( elem-- ) {
	       if( (this->value.undef[elem] = theExpr->value.undef[elem]) )
		  continue;

            /*  Before searching entire GTI, check the GTI found last time  */
	       if( gti<0 || times[elem]<start[gti] || times[elem]>stop[gti] ) {
		 gti = Search_GTI( times[elem], nGTI, start, stop, ordered, 0 );
	       }
	       if (dorow) {
		 this->value.data.lngptr[elem] = ( gti >= 0 ) ? (gti + 1) : (-1);
		 this->value.undef[elem]  = ( gti >= 0 ) ? 0 : 1;
	       } else {
		 this->value.data.logptr[elem] = ( gti>=0 );
	       }
	    }
	 } else { /* nGTI == 0 */

	   if (dorow) { /* no good times so all values are undef */
	     while( elem-- ) {
	       this->value.undef[elem]       = 1;
	     }
	   } else {    /* no good times so all logicals are 0 */
	     while( elem-- ) {
	       this->value.data.logptr[elem] = 0;
	       this->value.undef[elem]       = 0;
	     }
	   }
	   
	 }
      }
   }

   if( theExpr->operation>0 )
      free( theExpr->value.data.ptr );
}

static void Do_GTI_Over( ParseData *lParse, Node *this )
{
   Node *theTimes, *theStart, *theStop;
   double *gtiStart, *gtiStop;
   double *evtStart, *evtStop;
   long elem, nGTI, gti, nextGTI;
   int ordered;

   theTimes = lParse->Nodes + this->SubNodes[0]; /* GTI times */
   theStop  = lParse->Nodes + this->SubNodes[2]; /* User start time */
   theStart = lParse->Nodes + this->SubNodes[1]; /* User stop time */

   nGTI     = theTimes->value.nelem;
   gtiStart = theTimes->value.data.dblptr;        /* GTI start */
   gtiStop  = theTimes->value.data.dblptr + nGTI; /* GTI stop */

   if( theStart->operation==CONST_OP && theStop->operation==CONST_OP) {

      this->value.data.dbl = 
	(GTI_Over( theStart->value.data.dbl, theStop->value.data.dbl,
		   nGTI, gtiStart, gtiStop, &gti));
      this->operation      = CONST_OP;

   } else {
      char undefStart = 0, undefStop = 0; /* Input values are undef? */
      double uStart, uStop;       /* User start/stop values */
      if (theStart->operation==CONST_OP) uStart = theStart->value.data.dbl;
      if (theStop ->operation==CONST_OP) uStop  = theStop ->value.data.dbl;

      Allocate_Ptrs( lParse, this );

      evtStart = theStart->value.data.dblptr;
      evtStop  = theStop ->value.data.dblptr;
      if( !lParse->status ) {

	 elem = lParse->nRows * this->value.nelem;
	 if( nGTI ) {
	    double toverlap = 0.0;
	    gti = -1;
	    while( elem-- ) {
	      if (theStart->operation!=CONST_OP) {
		undefStart = theStart->value.undef[elem];
		uStart     = evtStart[elem];
	      }
	      if (theStop->operation!=CONST_OP) {
		undefStop  = theStop ->value.undef[elem];
		uStop      = evtStop[elem];
	      }
	      /* This works because at least one of the values is not const */
	      if( (this->value.undef[elem] = (undefStart||undefStop)) )
		  continue;

            /*  Before searching entire GTI, check the GTI found last time  */
	       if( gti<0 || 
		   uStart<gtiStart[gti] || uStart>gtiStop[gti] ||
		   uStop <gtiStart[gti] || uStop >gtiStop[gti]) {
		 /* Nope, need to recalculate */
		 toverlap = GTI_Over(uStart, uStop, 
				     nGTI, gtiStart, gtiStop, 
				     &gti);
	       } else {
		 /* We are in same GTI, the overlap is just stop-start of user range */
		 toverlap = (uStop-uStart);
	       }

	       /* This works because at least one of the values is not const */
	       this->value.data.dblptr[elem] = toverlap;
	    }
	 } else
	    /* nGTI == 0; there is no overlap so set all values to 0.0 */
	    while( elem-- ) {
	       this->value.data.dblptr[elem] = 0.0;
	       this->value.undef[elem]       = 0;
	    }
      }
   }

   if( theStart->operation>0 ) {
     free( theStart->value.data.ptr );
   }
   if( theStop->operation>0 ) {
     free( theStop->value.data.ptr );
   }
}

static double GTI_Over(double evtStart, double evtStop,
		       long nGTI, double *start, double *stop,
		       long *gtiout)
{
  long gti1, gti2, nextGTI1, nextGTI2;
  long gti, nMax;
  double overlap = 0.0;

  *gtiout = -1L;
  /* Zero or negative bin size */
  if (evtStop <= evtStart) return 0.0;

  /* Locate adjacent GTIs for evtStart and evtStop */
  gti1 = Search_GTI(evtStart, nGTI, start, stop, 1, &nextGTI1);
  gti2 = Search_GTI(evtStop,  nGTI, start, stop, 1, &nextGTI2);

  /* evtStart is in gti1, we return that for future processing */
  if (gti1 >= 0) *gtiout = gti1;

  /* Both evtStart/evtStop are beyond the last GTI */
  if (nextGTI1 < 0 && nextGTI2 < 0) return 0.0;

  /* Both evtStart/evtStop are in the same gap between GTIs */
  if (gti1 < 0 && gti2 < 0 && nextGTI1 == nextGTI2) return 0.0;

  /* Both evtStart/evtStop are in the same GTI */
  if (gti1 >= 0 && gti1 == gti2) return (evtStop-evtStart);

  /* Count through the remaining GTIs; there will be at least one */
  /* The largest GTI to consider is either nextGTI2-1, if it exists,
     or nGTI-1 */
  if (nextGTI2 < 0) nMax = nGTI-1;
  else if (gti2 >= 0) nMax = nextGTI2;
  else nMax = nextGTI2-1;
  for (gti = nextGTI1; gti <= nMax; gti++) {
    double starti = start[gti], stopi = stop[gti];
    /* Trim the GTI by actual evtStart/Stop times */
    if (evtStart > starti) starti = evtStart;
    if (evtStop  < stopi ) stopi  = evtStop;
    overlap += (stopi - starti);
  }
    
  return overlap;
}

/*
 * Search_GTI - search GTI for requested evtTime
 * 
 * double evtTime - requested event time
 * long nGTI - number of entries in start[] and stop[]
 * double start[], stop[] - start and stop of each GTI
 * int ordered - set to 1 if time-ordered
 * long *nextGTI0 - upon return, *nextGTI0 is either
 *                   the GTI evtTime is inside
 *                   the next GTI if evtTime is not inside
 *                   -1L if there is no next GTI
 *                   not set if nextGTI0 is a null pointer
 *
 * NOTE: for *nextGTI to be well-defined, the GTI must
 *   be ordered.  This is true when called by Do_GTI.
 *
 * RETURNS: gti index that evtTime is located inside, or -1L
 */
static long Search_GTI( double evtTime, long nGTI, double *start,
			double *stop, int ordered, long *nextGTI0 )
{
   long gti, nextGTI = -1L, step;
                             
   if( ordered && nGTI>15 ) { /*  If time-ordered and lots of GTIs,   */
                              /*  use "FAST" Binary search algorithm  */
      if( evtTime>=start[0] && evtTime<=stop[nGTI-1] ) {
	 gti = step = (nGTI >> 1);
	 while(1) {
	    if( step>1L ) step >>= 1;
	    
	    if( evtTime>stop[gti] ) {
	       if( evtTime>=start[gti+1] )
		  gti += step;
	       else {
		  nextGTI = gti+1;
		  gti = -1L;
		  break;
	       }
	    } else if( evtTime<start[gti] ) {
	       if( evtTime<=stop[gti-1] )
		  gti -= step;
	       else {
		  nextGTI = gti;
		  gti = -1L;
		  break;
	       }
	    } else {
	       nextGTI = gti;
	       break;
	    }
	 }
      } else {
	 if (start[0] > evtTime) nextGTI = 0;
	 gti = -1L;
      }
      
   } else { /*  Use "SLOW" linear search.  Not required to be 
	        ordered, so we have to search the whole table
		no matter what.
	    */
      gti = nGTI;
      while( gti-- ) {
	if( stop[gti] >= evtTime ) nextGTI = gti;
	if( evtTime>=start[gti] && evtTime<=stop[gti] )
	    break;
      }
   }

   if (nextGTI >= nGTI) nextGTI = -1;
   if (nextGTI0) *nextGTI0 = nextGTI;

   return( gti );
}

static void Do_REG( ParseData *lParse, Node *this )
{
   Node *theRegion, *theX, *theY;
   double Xval=0.0, Yval=0.0;
   char   Xnull=0, Ynull=0;
   int    Xvector, Yvector;
   long   nelem, elem, rows;

   theRegion = lParse->Nodes + this->SubNodes[0];
   theX      = lParse->Nodes + this->SubNodes[1];
   theY      = lParse->Nodes + this->SubNodes[2];

   Xvector = ( theX->operation!=CONST_OP );
   if( Xvector )
      Xvector = theX->value.nelem;
   else {
      Xval  = theX->value.data.dbl;
   }

   Yvector = ( theY->operation!=CONST_OP );
   if( Yvector )
      Yvector = theY->value.nelem;
   else {
      Yval  = theY->value.data.dbl;
   } 

   if( !Xvector && !Yvector ) {

      this->value.data.log =
	 ( fits_in_region( Xval, Yval, (SAORegion *)theRegion->value.data.ptr )
	   != 0 );
      this->operation      = CONST_OP;

   } else {

      Allocate_Ptrs( lParse, this );

      if( !lParse->status ) {

	 rows  = lParse->nRows;
	 nelem = this->value.nelem;
	 elem  = rows*nelem;

	 while( rows-- ) {
	    while( nelem-- ) {
	       elem--;

	       if( Xvector>1 ) {
		  Xval  = theX->value.data.dblptr[elem];
		  Xnull = theX->value.undef[elem];
	       } else if( Xvector ) {
		  Xval  = theX->value.data.dblptr[rows];
		  Xnull = theX->value.undef[rows];
	       }

	       if( Yvector>1 ) {
		  Yval  = theY->value.data.dblptr[elem];
		  Ynull = theY->value.undef[elem];
	       } else if( Yvector ) {
		  Yval  = theY->value.data.dblptr[rows];
		  Ynull = theY->value.undef[rows];
	       }

	       this->value.undef[elem] = ( Xnull || Ynull );
	       if( this->value.undef[elem] )
		  continue;

	       this->value.data.logptr[elem] = 
		  ( fits_in_region( Xval, Yval,
				    (SAORegion *)theRegion->value.data.ptr )
		    != 0 );
	    }
	    nelem = this->value.nelem;
	 }
      }
   }

   if( theX->operation>0 )
      free( theX->value.data.ptr );
   if( theY->operation>0 )
      free( theY->value.data.ptr );
}

static void Do_Vector( ParseData *lParse, Node *this )
{
   Node *that;
   long row, elem, idx, jdx, offset=0;
   int node;

   Allocate_Ptrs( lParse, this );

   if( !lParse->status ) {

      for( node=0; node<this->nSubNodes; node++ ) {

	 that = lParse->Nodes + this->SubNodes[node];

	 if( that->operation == CONST_OP ) {

	    idx = lParse->nRows*this->value.nelem + offset;
	    while( (idx-=this->value.nelem)>=0 ) {
	       
	       this->value.undef[idx] = 0;

	       switch( this->type ) {
	       case BOOLEAN:
		  this->value.data.logptr[idx] = that->value.data.log;
		  break;
	       case LONG:
		  this->value.data.lngptr[idx] = that->value.data.lng;
		  break;
	       case DOUBLE:
		  this->value.data.dblptr[idx] = that->value.data.dbl;
		  break;
	       }
	    }
	    
	 } else {
	       
	    row  = lParse->nRows;
	    idx  = row * that->value.nelem;
	    while( row-- ) {
	       elem = that->value.nelem;
	       jdx = row*this->value.nelem + offset;
	       while( elem-- ) {
		  this->value.undef[jdx+elem] =
		     that->value.undef[--idx];

		  switch( this->type ) {
		  case BOOLEAN:
		     this->value.data.logptr[jdx+elem] =
			that->value.data.logptr[idx];
		     break;
		  case LONG:
		     this->value.data.lngptr[jdx+elem] =
			that->value.data.lngptr[idx];
		     break;
		  case DOUBLE:
		     this->value.data.dblptr[jdx+elem] =
			that->value.data.dblptr[idx];
		     break;
		  }
	       }
	    }
	 }
	 offset += that->value.nelem;
      }

   }

   for( node=0; node < this->nSubNodes; node++ )
     if( OPER(this->SubNodes[node])>0 )
       free( lParse->Nodes[this->SubNodes[node]].value.data.ptr );
}

static void Do_Array( ParseData *lParse, Node *this )
{
   Node *that;
   long row, elem, idx, jdx, offset=0;
   int node;

   Allocate_Ptrs( lParse, this );

   if( !lParse->status ) {

     /* This is the item to be replicated */
     that = lParse->Nodes + this->SubNodes[0];

     if( that->operation == CONST_OP ) {

       idx = lParse->nRows*this->value.nelem + offset;
       while( idx-- ) {

	 this->value.undef[idx] = 0;

	 switch( this->type ) {
	 case BOOLEAN:
	   this->value.data.logptr[idx] = that->value.data.log;
	   break;
	 case LONG:
	   this->value.data.lngptr[idx] = that->value.data.lng;
	   break;
	 case DOUBLE:
	   this->value.data.dblptr[idx] = that->value.data.dbl;
	   break;
	 }
       }

     } else if (that->value.nelem > 1) { /* array "REFORM" */
       /* Note that dimensions change but total number of elements is same,
	  so we just do a straight copy */
      
       idx = lParse->nRows*this->value.nelem;
       while( idx-- ) {

	 this->value.undef[idx] = that->value.undef[idx];

	 switch( this->type ) {
	 case BOOLEAN:
	   this->value.data.logptr[idx] = that->value.data.logptr[idx];
	   break;
	 case LONG:
	   this->value.data.lngptr[idx] = that->value.data.lngptr[idx];
	   break;
	 case DOUBLE:
	   this->value.data.dblptr[idx] = that->value.data.dblptr[idx];
	   break;
	 }
       }
       
     } else { /* Any promotion of scalar to vector/array */
       
       row  = lParse->nRows;
       idx  = row * this->value.nelem - 1;
       while( row-- ) {
	 elem = this->value.nelem;
	 while( elem-- ) {
	   this->value.undef[idx] = that->value.undef[row];

	   switch( this->type ) {
	   case BOOLEAN:
	     this->value.data.logptr[idx] = that->value.data.logptr[row];
	     break;
	   case LONG:
	     this->value.data.lngptr[idx] = that->value.data.lngptr[row];
	     break;
	   case DOUBLE:
	     this->value.data.dblptr[idx] = that->value.data.dblptr[row];
	     break;
	   }
	   idx--;
	 }
       }

     } /* not constant */

     if( OPER(this->SubNodes[0])>0 )
       free( lParse->Nodes[this->SubNodes[0]].value.data.ptr );

   }

}

/*****************************************************************************/
/*  Utility routines which perform the calculations on bits and SAO regions  */
/*****************************************************************************/

static char bitlgte(char *bits1, int oper, char *bits2)
{
 int val1, val2, nextbit;
 char result;
 int i, l1, l2, length, ldiff;
 char *stream=0;
 char chr1, chr2;

 l1 = strlen(bits1);
 l2 = strlen(bits2);
 length = (l1 > l2) ? l1 : l2;
 stream = (char *)malloc(sizeof(char)*(length+1));
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bits1++);
    stream[i] = '\0';
    bits1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bits2++);
    stream[i] = '\0';
    bits2 = stream;
   }

 val1 = val2 = 0;
 nextbit = 1;

 while( length-- )
    {
     chr1 = bits1[length];
     chr2 = bits2[length];
     if ((chr1 != 'x')&&(chr1 != 'X')&&(chr2 != 'x')&&(chr2 != 'X'))
       {
        if (chr1 == '1') val1 += nextbit;
        if (chr2 == '1') val2 += nextbit;
        nextbit *= 2;
       }
    }
 result = 0;
 switch (oper)
       {
        case LT:
             if (val1 < val2) result = 1;
             break;
        case LTE:
             if (val1 <= val2) result = 1;
             break;
        case GT:
             if (val1 > val2) result = 1;
             break;
        case GTE:
             if (val1 >= val2) result = 1;
             break;
       }
 free(stream);
 return (result);
}

static void bitand(char *result,char *bitstrm1,char *bitstrm2)
{
 int i, l1, l2, ldiff, largestStream;
 char *stream=0;
 char chr1, chr2;

 l1 = strlen(bitstrm1);
 l2 = strlen(bitstrm2);
 largestStream = (l1 > l2) ? l1 : l2;
 stream = (char *)malloc(sizeof(char)*(largestStream+1));
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bitstrm1++);
    stream[i] = '\0';
    bitstrm1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bitstrm2++);
    stream[i] = '\0';
    bitstrm2 = stream;
   }
 while ( (chr1 = *(bitstrm1++)) ) 
    {
       chr2 = *(bitstrm2++);
       if ((chr1 == 'x') || (chr2 == 'x'))
          *result = 'x';
       else if ((chr1 == '1') && (chr2 == '1'))
          *result = '1';
       else
          *result = '0';
       result++;
    }
 free(stream);
 *result = '\0';
}

static void bitor(char *result,char *bitstrm1,char *bitstrm2)
{
 int i, l1, l2, ldiff, largestStream;
 char *stream=0;
 char chr1, chr2;

 l1 = strlen(bitstrm1);
 l2 = strlen(bitstrm2);
 largestStream = (l1 > l2) ? l1 : l2;
 stream = (char *)malloc(sizeof(char)*(largestStream+1));
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bitstrm1++);
    stream[i] = '\0';
    bitstrm1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bitstrm2++);
    stream[i] = '\0';
    bitstrm2 = stream;
   }
 while ( (chr1 = *(bitstrm1++)) ) 
    {
       chr2 = *(bitstrm2++);
       if ((chr1 == '1') || (chr2 == '1'))
          *result = '1';
       else if ((chr1 == '0') || (chr2 == '0'))
          *result = '0';
       else
          *result = 'x';
       result++;
    }
 free(stream);
 *result = '\0';
}

static void bitnot(char *result,char *bits)
{
   int length;
   char chr;

   length = strlen(bits);
   while( length-- ) {
      chr = *(bits++);
      *(result++) = ( chr=='1' ? '0' : ( chr=='0' ? '1' : chr ) );
   }
   *result = '\0';
}

static char bitcmp(char *bitstrm1, char *bitstrm2)
{
 int i, l1, l2, ldiff, largestStream;
 char *stream=0;
 char chr1, chr2;

 l1 = strlen(bitstrm1);
 l2 = strlen(bitstrm2);
 largestStream = (l1 > l2) ? l1 : l2;
 stream = (char *)malloc(sizeof(char)*(largestStream+1));
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bitstrm1++);
    stream[i] = '\0';
    bitstrm1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bitstrm2++);
    stream[i] = '\0';
    bitstrm2 = stream;
   }
 while( (chr1 = *(bitstrm1++)) )
    {
       chr2 = *(bitstrm2++);
       if ( ((chr1 == '0') && (chr2 == '1'))
	    || ((chr1 == '1') && (chr2 == '0')) )
       {
          free(stream);
	  return( 0 );
       }
    }
 free(stream);
 return( 1 );
}

static char bnear(double x, double y, double tolerance)
{
 if (fabs(x - y) < tolerance)
   return ( 1 );
 else
   return ( 0 );
}

static char saobox(double xcen, double ycen, double xwid, double ywid,
		   double rot,  double xcol, double ycol)
{
 double x,y,xprime,yprime,xmin,xmax,ymin,ymax,theta;

 theta = (rot / 180.0) * myPI;
 xprime = xcol - xcen;
 yprime = ycol - ycen;
 x =  xprime * cos(theta) + yprime * sin(theta);
 y = -xprime * sin(theta) + yprime * cos(theta);
 xmin = - 0.5 * xwid; xmax = 0.5 * xwid;
 ymin = - 0.5 * ywid; ymax = 0.5 * ywid;
 if ((x >= xmin) && (x <= xmax) && (y >= ymin) && (y <= ymax))
   return ( 1 );
 else
   return ( 0 );
}

static char circle(double xcen, double ycen, double rad,
		   double xcol, double ycol)
{
 double r2,dx,dy,dlen;

 dx = xcol - xcen;
 dy = ycol - ycen;
 dx *= dx; dy *= dy;
 dlen = dx + dy;
 r2 = rad * rad;
 if (dlen <= r2)
   return ( 1 );
 else
   return ( 0 );
}

static char ellipse(double xcen, double ycen, double xrad, double yrad,
		    double rot, double xcol, double ycol)
{
 double x,y,xprime,yprime,dx,dy,dlen,theta;

 theta = (rot / 180.0) * myPI;
 xprime = xcol - xcen;
 yprime = ycol - ycen;
 x =  xprime * cos(theta) + yprime * sin(theta);
 y = -xprime * sin(theta) + yprime * cos(theta);
 dx = x / xrad; dy = y / yrad;
 dx *= dx; dy *= dy;
 dlen = dx + dy;
 if (dlen <= 1.0)
   return ( 1 );
 else
   return ( 0 );
}

/*
 * Extract substring
 */
 int cstrmid(ParseData *lParse, char *dest_str, int dest_len,
	    char *src_str,  int src_len,
	    int pos)
{
  /* char fill_char = ' '; */
  char fill_char = '\0';
  if (src_len == 0) { src_len = strlen(src_str); } /* .. if constant */

  /* Fill destination with blanks */
  if (pos < 0) { 
    yyerror(0, lParse, "STRMID(S,P,N) P must be 0 or greater");
    return -1;
  }
  if (pos > src_len || pos == 0) {
    /* pos==0: blank string requested */
    memset(dest_str, fill_char, dest_len);
  } else if (pos+dest_len > src_len) {
    /* Copy a subset */
    int nsub = src_len-pos+1;
    int npad = dest_len - nsub;
    memcpy(dest_str, src_str+pos-1, nsub);
    /* Fill remaining string with blanks */
    memset(dest_str+nsub, fill_char, npad);
  } else {
    /* Full string copy */
    memcpy(dest_str, src_str+pos-1, dest_len);
  }
  dest_str[dest_len] = '\0'; /* Null-terminate */

  return 0;
}


static void yyerror(yyscan_t scanner, ParseData *lParse, char *s)
{
    char msg[80];

    if( !lParse->status ) lParse->status = PARSE_SYNTAX_ERR;

    strncpy(msg, s, 80);
    msg[79] = '\0';
    ffpmsg(msg);
}
