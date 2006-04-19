extern char *malloc(), *realloc();

# line 2 "grammar.y"

#define import_spp
#define import_libc
#define import_stdio
#define import_ctype
#include <iraf.h>

#include "config.h"
#include "mem.h"
#include "operand.h"
#include "param.h"
#include "grammar.h"
#include "opcodes.h"
#include "clmodes.h"
#include "task.h"
#include "construct.h"
#include "errs.h"


/* CL parser, written as a yacc grammar:
 *   build up an (rpn) instruction sequence begining at the base of the
 *   operand stack as the grammar is recognized.
 *
 * The parser may be called during parameter initialization (initiated by
 * the CALL meta-code instruction), and to parse the executable portion
 * (from the EXEC instruction).
 *
 * CONSTANT's are put on the dictionary by addconst() rather than the operand
 *   stack to avoid conflict with the code being created.  They are accessed
 *   by using the yylval of IDENT and CONSTANT as dictionary indices that
 *   point to struct operands.  This is facilitated with the stkop() macro.
 *   Make sure that topd and topcs are restored on return to discard these
 *   temporaries.
 * When building offsets for branches, such as BIFF and GOTO, allow
 *   for the advancement of the pc by the size of the instruction (in ints).
 *   See opcodes.c for the code executed by the branch instructions.
 */

extern	int cldebug;
#define	lint			/* turns off sccsid in Yacc parser	*/

/* shorthand way to get at operands in dictionary. x will be values returned
 * from addconst() by way of $n's from CONSTANT and IDENT tokens; see gram.c
 * and its uses in grammar.l. also see pushop() for a description of the stack.
 */
#define	stkop(x)	(reference (operand, (x)))

int	dobkg 		= 0;	/* set when want to do code in bkground	*/
int	npipes 		= 0;	/* number of pipes in a command		*/
int	pipe_pc		= 0;	/* pc of last ADDPIPE instruction	*/
int	posit 		= 0;	/* positional argument count		*/
int	inarglist 	= 0;	/* set when in argument list		*/
int	parenlevel 	= 0;	/* level of paren nesting in command	*/
int 	in_iferr 	= 0;	/* in an iferr block			*/
int	cl_level	= 0;	/* CL calling level			*/

int	index_cnt;		/* Index counter in array ref's		*/
char	curr_param[SZ_FNAME];	/* Parameter name of ref's		*/
char	curr_task[SZ_FNAME];	/* ltaskname of command 		*/
int	stmt_pc;		/* PC at beginning of current statement */
int	varlist;		/* Declaration is list directed.	*/
int	vartype;		/* Type of declaration.			*/
int	do_params;		/* Are param definitions legal here?	*/
int	errcnt;			/* Syntax error count.			*/
int	inited;			/* Was variable already initialized.	*/
struct	param *pp;		/* Pointer to param being compiled.	*/
int	n_aval;			/* Number of array init values.		*/
int	lastref;		/* Was last ref an array?		*/
int	for_expr;		/* Was there an expression in FOR?	*/
char	*ifseen;		/* Have we just processed an IF?	*/
char	*errmsg;		/* Syntax error message.		*/

/* context-sensitive switches. technique is ok, but beware of nesting!
 */
static	int absmode 	= 0;	/* set by first absolute mode arg in cmd*/
static	int newstdout 	= 0;	/* set if stdout redirected in arg	*/
static	int bracelevel 	= 0;	/* set while in s_list to inhibit &	*/
static	int tbrace 	= 0;	/* fake braces for declarations		*/
static	int dobrace 	= 0;	/* handling braces.			*/
static	int sawnl 	= 0;	/* set when EOST was \n, else 0		*/
static	int printstmt 	= 0;	/* set when parsing FPRINT statement	*/
static	int scanstmt 	= 0;	/* set when parsing SCAN statement	*/
static	int iferr_tok 	= 0;	/* iferr/ifnoerr token type seen	*/

/* printf-format error messages.
 */
char *arrdeferr   = "Error in array initialization for `%s'.";
char *badparm     = "Parameter definition of `%s' is illegal here.";
char *inval_arr   = "Invalid array type for `%s'.";
char *inv_index   = "Invalid index definition for `%s'.";
char *twoinits    = "Two initializations for parameter `%s'.";

char *exlimits    = "Explicit range required for loop in external param.";
char *illegalvar  = "Illegal variable declarations.";
char *locallist   = "Local list variables are not permitted.";
char *nestediferr = "Nested iferr not allowed in test or handler block.";
char *posfirst    = "All positional arguments must be first";


extern	char	cmdblk[SZ_CMDBLK+1];	/* Command buffer in history.c */
extern	char	*ip_cmdblk;		/* Pointer to current char in command.*/
extern	char	*err_cmdblk;		/* ip_cmdblk when error detected. */

char	*index();
struct	param *initparam();
struct	label *getlabel(), *setlabel();

/* arbitrary large number for bracelevel in a procedure script 
 */
#define MAX_ERR    10
#define EYYERROR  { err_cmdblk = ip_cmdblk; YYERROR; }

# define Y_SCAN 257
# define Y_SCANF 258
# define Y_FSCAN 259
# define Y_FSCANF 260
# define Y_OSESC 261
# define Y_APPEND 262
# define Y_ALLAPPEND 263
# define Y_ALLREDIR 264
# define Y_GSREDIR 265
# define Y_ALLPIPE 266
# define D_D 267
# define D_PEEK 268
# define Y_NEWLINE 269
# define Y_CONSTANT 270
# define Y_IDENT 271
# define Y_WHILE 272
# define Y_IF 273
# define Y_ELSE 274
# define Y_FOR 275
# define Y_BREAK 276
# define Y_NEXT 277
# define Y_SWITCH 278
# define Y_CASE 279
# define Y_DEFAULT 280
# define Y_RETURN 281
# define Y_GOTO 282
# define Y_PROCEDURE 283
# define Y_BEGIN 284
# define Y_END 285
# define Y_BOOL 286
# define Y_INT 287
# define Y_REAL 288
# define Y_STRING 289
# define Y_FILE 290
# define Y_STRUCT 291
# define Y_GCUR 292
# define Y_IMCUR 293
# define Y_UKEY 294
# define Y_PSET 295
# define Y_IFERR 296
# define Y_IFNOERR 297
# define Y_THEN 298
# define YOP_AOADD 299
# define YOP_AOSUB 300
# define YOP_AOMUL 301
# define YOP_AODIV 302
# define YOP_AOCAT 303
# define YOP_OR 304
# define YOP_AND 305
# define YOP_EQ 306
# define YOP_NE 307
# define YOP_LE 308
# define YOP_GE 309
# define YOP_CONCAT 310
# define YOP_NOT 311
# define UMINUS 312
# define YOP_POW 313
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 2105 "grammar.y"


#include "lexyy.c"
#include "lexicon.c"
int yyexca[] ={
-1, 0,
	0, 1,
	261, 1,
	267, 1,
	268, 1,
	271, 1,
	272, 1,
	273, 1,
	275, 1,
	276, 1,
	277, 1,
	278, 1,
	279, 1,
	280, 1,
	281, 1,
	282, 1,
	286, 1,
	287, 1,
	288, 1,
	289, 1,
	290, 1,
	291, 1,
	292, 1,
	293, 1,
	294, 1,
	295, 1,
	296, 1,
	297, 1,
	61, 1,
	126, 1,
	123, 1,
	59, 1,
	-2, 0,
-1, 1,
	0, -1,
	-2, 3,
-1, 6,
	284, 25,
	-2, 0,
-1, 15,
	284, 26,
	-2, 0,
-1, 36,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 58,
	91, 52,
	-2, 51,
-1, 61,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 105,
	61, 260,
	299, 260,
	300, 260,
	301, 260,
	302, 260,
	303, 260,
	91, 260,
	-2, 261,
-1, 108,
	91, 250,
	-2, 249,
-1, 145,
	269, 192,
	59, 192,
	-2, 77,
-1, 146,
	269, 193,
	59, 193,
	-2, 78,
-1, 162,
	40, 259,
	-2, 260,
-1, 204,
	269, 152,
	59, 152,
	41, 152,
	-2, 77,
-1, 205,
	269, 153,
	59, 153,
	41, 153,
	-2, 78,
-1, 289,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 290,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 298,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 299,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 300,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	125, 148,
	-2, 0,
-1, 302,
	44, 253,
	-2, 252,
-1, 303,
	93, 255,
	44, 255,
	-2, 79,
-1, 304,
	93, 256,
	44, 256,
	-2, 78,
-1, 306,
	93, 258,
	44, 258,
	-2, 80,
-1, 363,
	266, 178,
	269, 178,
	59, 178,
	124, 178,
	44, 178,
	41, 178,
	-2, 77,
-1, 364,
	266, 179,
	269, 179,
	59, 179,
	124, 179,
	44, 179,
	41, 179,
	-2, 78,
-1, 365,
	91, 250,
	-2, 249,
-1, 390,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 401,
	266, 190,
	269, 190,
	59, 190,
	124, 190,
	44, 190,
	41, 190,
	-2, 77,
-1, 402,
	266, 191,
	269, 191,
	59, 191,
	91, 250,
	124, 191,
	44, 191,
	41, 191,
	-2, 249,
-1, 408,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 409,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 414,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
-1, 421,
	266, 180,
	269, 180,
	59, 180,
	124, 180,
	44, 180,
	41, 180,
	-2, 77,
-1, 422,
	266, 181,
	269, 181,
	59, 181,
	124, 181,
	44, 181,
	41, 181,
	-2, 78,
-1, 433,
	261, 240,
	271, 240,
	272, 240,
	273, 240,
	275, 240,
	276, 240,
	277, 240,
	278, 240,
	279, 240,
	280, 240,
	281, 240,
	282, 240,
	296, 240,
	297, 240,
	61, 240,
	123, 240,
	59, 240,
	-2, 0,
	};
# define YYNPROD 272
# define YYLAST 910
int yyact[]={

    61,    66,   147,   176,   292,   154,    68,   218,   362,   374,
   361,   338,   175,   125,   261,     9,   219,   201,   301,   167,
    91,   216,   214,   241,   215,   166,   217,   340,   196,    47,
   197,   196,   219,   197,   219,    60,   339,   216,   214,   216,
   215,   189,   217,   379,   217,   207,   208,   209,   210,   211,
    48,    92,   193,    90,   287,   173,   258,   255,   219,    53,
    21,    12,    40,   216,   214,   266,   215,   219,   217,    89,
    45,   391,   216,   214,   187,   215,   310,   217,   107,   120,
   119,   221,   400,   222,   357,    51,   190,   196,    36,   197,
   221,    54,   222,   188,   248,   108,   146,   199,   397,   170,
   110,   257,   121,   119,   432,   145,   349,   253,   319,   296,
   174,   177,   172,   118,   126,    23,    31,    25,    24,    26,
    32,    27,    28,    29,    30,   398,   202,   399,   202,   317,
    46,   119,    65,     7,    50,    16,   358,   302,   180,   242,
    59,    18,   142,    49,   299,   298,   205,   390,    41,     2,
    18,    42,   171,   433,   441,   204,   123,   437,   428,   213,
   130,   229,   230,    51,   415,   387,   238,   236,   372,    94,
   239,   237,   373,   245,   309,   183,   112,    95,   246,   395,
   393,   359,   307,   249,   181,   109,   320,   203,   251,   265,
   212,   144,   206,    23,    31,    25,    24,    26,    32,    27,
    28,    29,    30,    38,    39,   184,   143,   267,   356,   179,
   185,    88,   264,    87,    59,   269,   270,   271,   272,   273,
   274,   275,   276,   277,   278,   279,   280,   281,   282,   283,
    86,   268,    85,    84,    83,    82,    81,   289,   290,    80,
    79,   295,    78,   294,   291,   254,   293,    77,   300,    76,
   247,   303,   304,   252,    75,   192,   193,    74,   244,    73,
   191,    72,    71,    70,    47,    69,    67,   321,   322,   308,
    12,   163,   344,   259,   288,   323,   324,   325,   326,   327,
   328,   329,   330,   331,   332,   333,   334,   335,   336,   337,
    12,   341,   218,   161,   345,   384,   311,   352,   342,   263,
   297,   351,    65,   346,   347,   220,   119,   156,   218,   348,
   218,   364,   354,   355,   192,   286,   285,   316,   318,   284,
   363,   353,   366,   195,   367,   227,   228,   225,   226,   223,
   224,   220,   375,   376,   218,   228,   225,   226,   223,   224,
   220,   378,   313,   218,    65,   343,   350,   194,   312,   156,
   388,   260,   389,   122,   205,    58,   380,   198,   311,     5,
   383,    57,   385,   204,   366,   360,   367,   186,    56,    55,
    65,    44,   305,   408,   409,   156,    22,   394,    15,   401,
   401,   401,   401,   401,   401,   129,     8,     9,   414,   128,
   341,    63,   413,   410,   411,   303,   304,   381,   382,   364,
   364,   422,   386,   365,   417,   420,   419,   416,   363,   363,
   421,   418,   412,    65,    34,   124,   392,   430,   156,   396,
    33,   429,   423,   424,    14,     6,   116,    37,   427,   425,
     4,     3,   243,   435,    10,     1,     0,     0,   431,     0,
   440,   294,   438,   442,   293,    90,     0,   436,     0,   434,
   403,   404,   405,   406,   407,     0,   439,     0,     0,     0,
   262,     0,   402,   402,   402,   402,   402,   402,     0,     0,
   426,     0,    21,   106,   219,   110,     0,   396,   202,   216,
   214,     0,   215,     0,   217,     0,     0,     0,   219,     0,
     0,   365,   365,   216,   214,     0,   215,   221,   217,   222,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   221,     0,   222,     0,     0,     0,   314,   315,   157,
   158,   159,   160,     0,   369,   370,   368,   371,     0,     0,
     0,     0,   148,   162,     0,     0,     0,   107,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   164,
   165,     0,     0,     0,   149,   150,   151,   152,   243,     0,
     0,   157,   158,   159,   160,     0,   369,   370,   368,   371,
     0,     0,     0,   155,   148,   162,     0,     0,     0,   262,
   377,     0,     0,     0,     0,     0,     0,   157,   158,   159,
   160,   164,   165,     0,     0,     0,   149,   150,   151,   152,
   306,   162,     0,   219,     0,     0,     0,     0,   216,   214,
     0,   215,     0,   217,     0,   155,   153,   164,   165,     0,
     0,     0,   149,   150,   151,   152,   221,     0,   222,   219,
   157,   158,   159,   160,   216,   214,     0,   215,     0,   217,
     0,   155,     0,   148,   162,     0,     0,     0,     0,     0,
     0,     0,   221,     0,   222,     0,     0,     0,     0,     0,
   164,   165,     0,    64,     0,   149,   150,   151,   152,    19,
     0,     0,     0,     0,     0,    93,     0,     0,     0,     0,
     0,     0,    12,     0,   155,   105,    96,   111,     0,    97,
   102,   101,    98,    99,   100,   104,   103,     0,     0,    23,
    31,    25,    24,    26,    32,    27,    28,    29,    30,     0,
   113,   114,     0,   168,   169,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   182,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   227,   228,   225,   226,   223,   224,   220,     0,     0,
   218,     0,     0,     0,     0,   227,   228,   225,   226,   223,
   224,   220,     0,    62,   218,     0,    11,     0,     0,    13,
    20,     0,     0,    35,   231,   232,   233,   234,   235,    20,
     0,     0,     0,    43,     0,     0,     0,   240,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   250,     0,
     0,    20,     0,     0,     0,     0,     0,     0,     0,   256,
     0,     0,     0,     0,     0,   115,     0,     0,    20,     0,
     0,     0,    17,     0,     0,     0,     0,    20,     0,     0,
     0,     0,    20,    20,    20,    20,    20,    20,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    20,    20,    20,
    20,     0,     0,   141,     0,     0,     0,     0,     0,     0,
    52,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   178,     0,   225,   226,   223,   224,   220,   117,     0,   218,
     0,     0,     0,     0,     0,     0,   127,     0,     0,     0,
   200,   131,   132,   133,   134,   135,   136,     0,     0,     0,
   223,   224,   220,     0,     0,   218,   137,   138,   139,   140 };
int yypact[]={

   103, -1000,  -208, -1000, -1000,  -208,   413, -1000, -1000,  -208,
   -64, -1000, -1000, -1000,  -269,   413, -1000, -1000, -1000,  -208,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  -242, -1000,   -93,     1, -1000,  -211,
 -1000, -1000, -1000, -1000,    -7,  -208,    92, -1000, -1000,   414,
 -1000,  -208, -1000, -1000,     1,    59,   -44,    41, -1000, -1000,
  -242,  -171, -1000,     1,  -242, -1000, -1000, -1000,     1,     1,
     1,     1,     1,     1, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,     1,     1,     1,     1, -1000, -1000,  -208,
    39, -1000,   373, -1000,  -249,  -255,    92,    92,  -208, -1000,
    54, -1000, -1000,  -216,   373,    53,  -208, -1000, -1000, -1000,
 -1000,    92, -1000, -1000, -1000, -1000,   -64, -1000,    -7, -1000,
   -15, -1000,     6, -1000, -1000, -1000,  -208, -1000,    85,    59,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000,   373,  -254, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,   373,   451,   373,   373,    92,    92,    92,
    92,    92, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -208,
    92,   -12,  -208, -1000,   451, -1000, -1000,  -208, -1000, -1000,
     3,    92,   373,   -45, -1000, -1000,    48,    59,    59, -1000,
 -1000, -1000,    92,    40, -1000,  -214, -1000, -1000,    44,   -12,
 -1000, -1000, -1000,  -242, -1000, -1000,   373, -1000, -1000, -1000,
 -1000, -1000,   -59,   437,  -208,  -208,  -208,  -208,  -208,  -208,
  -208,  -208,  -208,  -208,  -208,  -208,  -208,  -208,  -208,  -306,
  -306, -1000, -1000, -1000,  -217, -1000,  -208,  -208,   373,  -242,
  -208,    51,    59, -1000, -1000, -1000, -1000,  -208,   330, -1000,
 -1000,   437, -1000,   -49,   -15,  -219,   -12,   -12, -1000,    59,
    36, -1000,    50, -1000,   451, -1000,  -208,  -208, -1000,   373,
   373,   373,   373,   373,   373,   373,   373,   373,   373,   373,
   373,   373,   373,   373,  -235,   373,  -235,    59,   373,   -93,
   -93,   437,    47, -1000,    39,   373,  -208,   -12,   -93,   -93,
   -93,    -9, -1000, -1000, -1000, -1000, -1000,   262, -1000,  -289,
 -1000, -1000,    59, -1000,    85, -1000,    44, -1000,   -12,   -12,
  -228, -1000, -1000,    -3,    -3,  -306,  -306, -1000,  -306,   -21,
    -5,    -5,    -5,    -5,   592,   592,    30,   566,    85,    59,
    59,   451,    85, -1000,    87,   451, -1000, -1000, -1000,  -208,
   373,    21, -1000, -1000, -1000, -1000,   -54, -1000,    59,    85,
 -1000,    59, -1000, -1000,    37,    82,   373,   373,   373,   373,
   373,   373,  -208,  -208, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000,  -235,  -235, -1000,   373, -1000,   373,  -208, -1000,    85,
   -93, -1000,   330, -1000, -1000,   304,   304,   373, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,   -93,   -93,
 -1000,    85,    59,   451,   -93,   373,  -208, -1000, -1000,    59,
 -1000, -1000, -1000, -1000, -1000, -1000,  -235, -1000,    45,   451,
 -1000,    85,  -208,   -93, -1000, -1000, -1000,  -242,    85,  -208,
 -1000,   414, -1000 };
int yypgo[]={

     0,   435,   763,   434,    88,    13,   431,   430,   427,   822,
   426,   425,   424,   133,   420,    70,     0,   415,   414,    95,
   391,   616,   389,    17,   385,    57,   378,   135,   134,   376,
   371,    91,   369,   368,   367,   361,   357,    74,   355,   353,
   351,    14,   260,    41,   347,   323,    93,    86,     5,    12,
     3,     2,   319,    11,   316,    27,   315,   295,   293,   274,
   272,   271,     1,   266,     6,   265,   263,   262,   261,   259,
   257,   254,   249,   247,   242,   240,   239,   236,   235,   234,
   233,   232,   230,   213,   211,    69,   209,   208,    51,   206,
   192,    20,   191,   190,   189,   186,   185,   184,   183,   182,
   181,   180,   179,    10,     8,    82,   177,   176,   175,   174,
   172,   171,   169,   168,   167,   166,   165,     4,   164,   158,
   157,   154,   153,   152,    23,   147,   145,   144,   143,   139,
   138,    18,   137,   136 };
int yyr1[]={

     0,     1,     1,     3,     1,     1,     1,     1,     4,    10,
     4,     8,     8,     8,     6,    14,     7,    18,    11,    20,
    20,    22,    22,    24,    24,    12,    12,    26,    26,    27,
    27,    27,    30,    28,    29,    29,    29,    29,    29,    29,
    29,    29,    29,    29,    31,    31,    32,    32,    33,    36,
    33,    35,    39,    35,    38,    38,    40,    40,    40,    41,
    41,    37,    37,    43,    43,    42,    42,    44,    45,    45,
    34,    34,    34,    46,    46,    47,    13,    48,    48,    49,
    49,    49,    49,    49,    49,    51,    51,    51,    51,    51,
    51,    51,    51,    51,    51,    51,    51,    51,    51,    51,
    51,    51,    51,    52,    51,    54,    51,    56,    51,    57,
    51,    59,    51,    58,    58,    58,    55,    53,    53,    53,
    60,    60,    60,    62,    62,    62,    62,    62,    62,    62,
    62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
    62,    62,    62,    62,    62,    63,    63,    86,    87,    85,
    15,    15,    64,    64,    89,    64,    88,    90,    90,    90,
    90,    90,    92,    65,    93,    95,    93,    94,    94,    97,
    99,    91,   102,   100,   100,   103,   103,   104,   104,   104,
   104,   104,   104,   104,   104,   104,   104,   104,   104,   104,
   105,   105,    66,    66,    67,    68,    69,    72,   108,   109,
   106,   111,    73,   107,   107,   110,   110,    70,   113,   112,
   114,    71,   115,   116,    74,   118,   120,   121,    75,   117,
   117,   119,   119,   122,    76,   123,   125,    77,   126,    78,
    79,    80,    82,    82,    17,   127,    83,    81,    84,    84,
   128,     5,     5,     5,   124,   124,   129,    16,    16,    50,
   130,    50,   131,   133,   131,   132,   132,   132,   132,    61,
    19,    96,     9,     9,    25,    98,    98,   101,   101,    21,
    23,     2 };
int yyr2[]={

     0,     1,     5,     1,     9,     3,     3,     5,     0,     1,
     8,     3,     5,     3,     7,     1,    10,     1,    10,     1,
     6,     1,     2,     3,     7,     0,     2,     2,     4,     2,
     2,     5,     1,     9,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     2,     6,     3,    11,     3,     1,
     9,     3,     1,    10,     3,     5,     0,     2,     6,     3,
     7,     2,     6,     3,     9,     2,     2,     5,     3,     3,
     7,     3,     2,     2,     6,     7,     4,     2,     3,     2,
     3,     3,     3,     3,     3,     6,     9,     9,     9,     9,
     9,     9,     9,     9,     9,     9,     9,     9,     9,     9,
     9,     5,     5,     1,    11,     1,    15,     1,    11,     1,
    19,     1,    11,     2,     3,     3,     3,     0,     3,     7,
     0,     3,     7,     2,     4,     4,     4,     4,     4,     4,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     4,
     4,     4,     4,     2,     2,     2,     4,     1,     1,    12,
     0,     6,     7,     7,     1,     9,     3,     3,     3,     3,
     3,     3,     1,     7,     0,     1,     9,     5,     5,     1,
     1,    13,     1,     6,     2,     2,     6,     1,     3,     3,
     7,     7,     5,     5,     5,     5,     5,     5,     5,     5,
     3,     3,     5,     5,     5,     3,     3,     3,     1,     1,
    15,     1,    11,     3,     3,     0,     2,     3,     1,    15,
     1,    11,     1,     1,    17,     1,     1,     1,    33,     2,
     0,     3,     1,     1,    21,     1,     1,    15,     1,    11,
     3,     3,     3,     5,     5,     1,    10,     5,     2,     4,
     1,     5,     2,     5,     2,     6,     3,     0,     2,     3,
     1,    11,     3,     1,     8,     3,     3,     3,     3,     3,
     3,     3,     2,     3,     2,     0,     2,     0,     2,     3,
     3,     3 };
int yychk[]={

 -1000,    -1,    46,    -6,    -7,   256,   -11,   -13,   283,   284,
    -3,    -2,   269,    -2,   -12,   -26,   -27,    -9,   -28,   256,
    -2,    59,   -29,   286,   289,   288,   290,   292,   293,   294,
   295,   287,   291,   -14,   -18,    -2,    -4,    -8,   267,   268,
   126,   -13,   -27,    -2,   -30,   -15,   -19,   271,    -5,  -128,
   -28,   256,    -9,   270,   -31,   -32,   -33,   -35,   -38,   -19,
    42,   -16,    -2,   -20,   -21,    40,   -62,   -63,   -64,   -65,
   -66,   -67,   -68,   -69,   -70,   -71,   -72,   -73,   -74,   -75,
   -76,   -77,   -78,   -79,   -80,   -81,   -82,   -83,   -84,   -85,
   -50,   -91,   -88,   261,  -112,  -106,   272,   275,   278,   279,
   280,   277,   276,   282,   281,   271,    59,   123,   -19,   -96,
    61,   273,  -107,   296,   297,    -2,   -10,    -9,   -25,    44,
   123,    61,   -39,   -19,   -17,    -5,   285,    -9,   -22,   -24,
   -19,    -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,
    -9,    -2,   -88,   -89,   -92,   -49,   -50,   -51,   270,   292,
   293,   294,   295,   -21,   -48,   311,    45,   257,   258,   259,
   260,   -58,   271,   -61,   287,   288,   274,   274,   -21,   -21,
   -16,  -123,    58,   271,   -48,   -49,   -50,    58,    -2,   -86,
  -130,   -97,   -21,  -108,    -4,   -31,   -34,   -37,   -46,   -43,
   -47,   -42,   270,   271,   -44,   -45,    43,    45,   -36,    91,
    -2,   -23,    41,   -25,   -49,   -50,   -90,   299,   300,   301,
   302,   303,   -93,   -48,    43,    45,    42,    47,   313,    37,
   310,    60,    62,   308,   309,   306,   307,   304,   305,   -48,
   -48,   -21,   -21,   -21,   -21,   -21,  -114,  -111,  -115,   -16,
   -21,  -124,  -129,   -42,   270,   -16,   -16,   -15,    91,   -98,
   -21,   -48,   -85,    59,   -25,   -25,   -21,    61,   270,   -37,
   -40,   -41,   -42,   -19,   -48,   -94,   124,   266,   -23,   -16,
   -16,   -16,   -16,   -16,   -16,   -16,   -16,   -16,   -16,   -16,
   -16,   -16,   -16,   -16,   -52,   -54,   -56,   271,   -59,   -16,
   -16,   -48,  -117,   -64,   -50,   -16,    58,   -25,  -126,  -127,
   -16,  -131,  -132,   -51,   -50,    42,   270,   -99,   -23,  -109,
   125,   -43,   -46,   -47,   -42,   -42,   -25,    93,   -25,    58,
   -95,   -16,   -16,   -48,   -48,   -48,   -48,   -48,   -48,   -48,
   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -53,   271,
   -55,   -48,   -53,   -25,   -60,   -48,    -5,    -5,   -23,    59,
   -88,   -48,   -16,  -124,    -5,    -5,   -87,    93,  -133,  -100,
   -25,  -103,  -104,   -49,   -50,   -19,    60,    62,   264,   262,
   263,   265,  -113,  -110,   298,   -23,   -41,   -42,   -91,   271,
   -23,   -25,   -25,   -23,   -57,   -23,   -25,  -116,   -16,   -16,
  -125,   125,   -25,  -101,   -23,  -102,   -25,    61,    43,    45,
  -105,   -49,   -19,  -105,  -105,  -105,  -105,  -105,   -16,   -16,
   -53,   -53,   -55,   -48,   -16,  -118,   -23,    -5,  -131,  -103,
  -104,   -49,   -50,    -5,    -5,   -23,   -25,    -5,  -119,   -48,
   -16,   -53,    59,  -122,   -23,   -16,    -5,  -120,  -117,   -23,
   -16,  -121,   -62 };
int yydef[]={

    -2,    -2,     0,     5,     6,     0,    -2,    15,    17,     0,
     8,     2,   271,     7,     0,    -2,    27,    29,    30,     0,
   262,   263,    32,    34,    35,    36,    37,    38,    39,    40,
    41,    42,    43,   150,     0,    76,    -2,     0,    11,     0,
    13,    14,    28,    31,     0,   247,    19,   260,     4,     0,
   242,     0,     9,    12,     0,    44,    46,    48,    -2,    54,
     0,    -2,   248,     0,    21,   269,   241,   123,     0,     0,
     0,     0,     0,     0,   130,   131,   132,   133,   134,   135,
   136,   137,   138,     0,     0,     0,     0,   143,   144,   145,
   154,   162,   196,   195,   207,   197,     0,     0,   247,   225,
     0,   230,   231,     0,   232,    -2,   238,   147,    -2,   169,
   156,     0,   198,   203,   204,   243,     8,    33,     0,   264,
     0,    49,     0,    55,    16,   151,     0,    18,     0,    22,
    23,   124,   125,   126,   127,   128,   129,   139,   140,   141,
   142,   146,   194,     0,   164,    -2,    -2,    79,    80,    81,
    82,    83,    84,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    -2,   113,   114,   115,   210,   201,   212,   247,
     0,     0,   247,   237,   233,    77,    78,   247,   239,   150,
     0,   265,     0,     0,    10,    45,     0,    71,    72,    61,
    73,    63,    65,     0,    66,     0,    68,    69,     0,    56,
   234,    20,   270,     0,    -2,    -2,     0,   157,   158,   159,
   160,   161,   163,     0,   247,   247,   247,   247,   247,   247,
   247,   247,   247,   247,   247,   247,   247,   247,   247,   101,
   102,   103,   105,   107,     0,   111,   247,   247,     0,   220,
   247,     0,   244,   246,    65,   228,   235,   247,     0,   170,
   266,     0,   199,     0,     0,     0,     0,     0,    67,    50,
     0,    57,    59,    24,   155,   165,   247,   247,    85,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   117,     0,   117,     0,   120,    -2,
    -2,     0,     0,   219,   154,     0,   247,     0,    -2,    -2,
    -2,     0,    -2,    -2,    -2,   257,    -2,   177,   208,   205,
    47,    62,    70,    74,     0,    75,     0,    53,     0,     0,
     0,   167,   168,    86,    87,    88,    89,    90,    91,    92,
    93,    94,    95,    96,    97,    98,    99,   100,     0,   118,
     0,   116,     0,   109,     0,   121,   211,   202,   213,   247,
     0,   247,   226,   245,   229,   236,     0,   251,     0,   267,
   172,   174,   175,    -2,    -2,    -2,     0,     0,     0,     0,
     0,     0,   247,   247,   206,    64,    58,    60,   166,   261,
   104,   117,   117,   108,     0,   112,     0,   247,   215,     0,
    -2,   149,     0,   171,   268,   177,   177,     0,   182,   183,
   184,    -2,    -2,   185,   186,   187,   188,   189,    -2,    -2,
   119,     0,     0,   122,    -2,   222,   247,   227,   254,   173,
   176,    -2,    -2,   209,   200,   106,   117,   214,     0,   221,
   223,     0,   247,    -2,   110,   216,   224,   220,     0,   247,
   217,     0,   218 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"Y_SCAN",	257,
	"Y_SCANF",	258,
	"Y_FSCAN",	259,
	"Y_FSCANF",	260,
	"Y_OSESC",	261,
	"Y_APPEND",	262,
	"Y_ALLAPPEND",	263,
	"Y_ALLREDIR",	264,
	"Y_GSREDIR",	265,
	"Y_ALLPIPE",	266,
	"D_D",	267,
	"D_PEEK",	268,
	"Y_NEWLINE",	269,
	"Y_CONSTANT",	270,
	"Y_IDENT",	271,
	"Y_WHILE",	272,
	"Y_IF",	273,
	"Y_ELSE",	274,
	"Y_FOR",	275,
	"Y_BREAK",	276,
	"Y_NEXT",	277,
	"Y_SWITCH",	278,
	"Y_CASE",	279,
	"Y_DEFAULT",	280,
	"Y_RETURN",	281,
	"Y_GOTO",	282,
	"Y_PROCEDURE",	283,
	"Y_BEGIN",	284,
	"Y_END",	285,
	"Y_BOOL",	286,
	"Y_INT",	287,
	"Y_REAL",	288,
	"Y_STRING",	289,
	"Y_FILE",	290,
	"Y_STRUCT",	291,
	"Y_GCUR",	292,
	"Y_IMCUR",	293,
	"Y_UKEY",	294,
	"Y_PSET",	295,
	"Y_IFERR",	296,
	"Y_IFNOERR",	297,
	"Y_THEN",	298,
	"=",	61,
	"YOP_AOADD",	299,
	"YOP_AOSUB",	300,
	"YOP_AOMUL",	301,
	"YOP_AODIV",	302,
	"YOP_AOCAT",	303,
	"YOP_OR",	304,
	"YOP_AND",	305,
	"YOP_EQ",	306,
	"YOP_NE",	307,
	"<",	60,
	">",	62,
	"YOP_LE",	308,
	"YOP_GE",	309,
	"YOP_CONCAT",	310,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"%",	37,
	"YOP_NOT",	311,
	"UMINUS",	312,
	"YOP_POW",	313,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"block : /* empty */",
	"block : '.' NL",
	"block : block",
	"block : block debug xstmt",
	"block : script_params",
	"block : script_body",
	"block : error NL",
	"debug : /* empty */",
	"debug : D_XXX EOST",
	"debug : D_XXX EOST debug",
	"D_XXX : D_D",
	"D_XXX : D_PEEK Y_CONSTANT",
	"D_XXX : '~'",
	"script_params : proc_stmt var_decls begin_stmt",
	"script_body : begin_stmt",
	"script_body : begin_stmt s_list opnl end_stmt",
	"proc_stmt : Y_PROCEDURE",
	"proc_stmt : Y_PROCEDURE param bparam_list EOST",
	"bparam_list : /* empty */",
	"bparam_list : LP param_list RP",
	"param_list : /* empty */",
	"param_list : xparam_list",
	"xparam_list : param",
	"xparam_list : xparam_list DELIM param",
	"var_decls : /* empty */",
	"var_decls : var_decl_block",
	"var_decl_block : var_decl_line",
	"var_decl_block : var_decl_block var_decl_line",
	"var_decl_line : EOST",
	"var_decl_line : var_decl_stmt",
	"var_decl_line : error NL",
	"var_decl_stmt : typedefs",
	"var_decl_stmt : typedefs var_decl_list EOST",
	"typedefs : Y_BOOL",
	"typedefs : Y_STRING",
	"typedefs : Y_REAL",
	"typedefs : Y_FILE",
	"typedefs : Y_GCUR",
	"typedefs : Y_IMCUR",
	"typedefs : Y_UKEY",
	"typedefs : Y_PSET",
	"typedefs : Y_INT",
	"typedefs : Y_STRUCT",
	"var_decl_list : var_decl_plus",
	"var_decl_list : var_decl_plus DELIM var_decl_list",
	"var_decl_plus : var_decl",
	"var_decl_plus : var_decl '{' options_list ';' '}'",
	"var_decl : var_def",
	"var_decl : var_def '='",
	"var_decl : var_def '=' init_list",
	"var_def : var_name",
	"var_def : var_name",
	"var_def : var_name '[' init_index_list ']'",
	"var_name : param",
	"var_name : '*' param",
	"init_index_list : /* empty */",
	"init_index_list : init_index_range",
	"init_index_list : init_index_list DELIM init_index_range",
	"init_index_range : const",
	"init_index_range : const ':' const",
	"init_list : init_elem",
	"init_list : init_list DELIM init_elem",
	"init_elem : const",
	"init_elem : Y_CONSTANT LP const RP",
	"const : Y_CONSTANT",
	"const : number",
	"number : sign Y_CONSTANT",
	"sign : '+'",
	"sign : '-'",
	"options_list : init_list DELIM options",
	"options_list : init_list",
	"options_list : options",
	"options : option",
	"options : options DELIM option",
	"option : Y_IDENT '=' const",
	"begin_stmt : Y_BEGIN NL",
	"expr : expr0",
	"expr : ref",
	"expr0 : expr1",
	"expr0 : Y_CONSTANT",
	"expr0 : Y_GCUR",
	"expr0 : Y_IMCUR",
	"expr0 : Y_UKEY",
	"expr0 : Y_PSET",
	"expr1 : LP expr RP",
	"expr1 : expr '+' opnl expr",
	"expr1 : expr '-' opnl expr",
	"expr1 : expr '*' opnl expr",
	"expr1 : expr '/' opnl expr",
	"expr1 : expr YOP_POW opnl expr",
	"expr1 : expr '%' opnl expr",
	"expr1 : expr YOP_CONCAT opnl expr",
	"expr1 : expr '<' opnl expr",
	"expr1 : expr '>' opnl expr",
	"expr1 : expr YOP_LE opnl expr",
	"expr1 : expr YOP_GE opnl expr",
	"expr1 : expr YOP_EQ opnl expr",
	"expr1 : expr YOP_NE opnl expr",
	"expr1 : expr YOP_OR opnl expr",
	"expr1 : expr YOP_AND opnl expr",
	"expr1 : YOP_NOT expr",
	"expr1 : '-' expr",
	"expr1 : Y_SCAN LP",
	"expr1 : Y_SCAN LP scanarg RP",
	"expr1 : Y_SCANF LP",
	"expr1 : Y_SCANF LP scanfmt DELIM scanarg RP",
	"expr1 : Y_FSCAN LP",
	"expr1 : Y_FSCAN LP scanarg RP",
	"expr1 : Y_FSCANF LP Y_IDENT DELIM",
	"expr1 : Y_FSCANF LP Y_IDENT DELIM scanfmt DELIM scanarg RP",
	"expr1 : intrinsx LP",
	"expr1 : intrinsx LP intrarg RP",
	"intrinsx : intrins",
	"intrinsx : Y_INT",
	"intrinsx : Y_REAL",
	"scanfmt : expr",
	"scanarg : /* empty */",
	"scanarg : Y_IDENT",
	"scanarg : Y_IDENT DELIM scanarg",
	"intrarg : /* empty */",
	"intrarg : expr",
	"intrarg : intrarg DELIM expr",
	"stmt : c_stmt",
	"stmt : assign EOST",
	"stmt : cmdlist EOST",
	"stmt : immed EOST",
	"stmt : inspect EOST",
	"stmt : osesc EOST",
	"stmt : popstk EOST",
	"stmt : if",
	"stmt : ifelse",
	"stmt : iferr",
	"stmt : iferr_else",
	"stmt : while",
	"stmt : for",
	"stmt : switch",
	"stmt : case",
	"stmt : default",
	"stmt : next EOST",
	"stmt : break EOST",
	"stmt : goto EOST",
	"stmt : return EOST",
	"stmt : label_stmt",
	"stmt : nullstmt",
	"c_stmt : c_blk",
	"c_stmt : c_blk NL",
	"c_blk : '{'",
	"c_blk : '{' s_list opnl",
	"c_blk : '{' s_list opnl '}'",
	"s_list : /* empty */",
	"s_list : s_list opnl xstmt",
	"assign : ref equals expr0",
	"assign : ref equals ref",
	"assign : ref",
	"assign : ref assign_oper expr",
	"equals : '='",
	"assign_oper : YOP_AOADD",
	"assign_oper : YOP_AOSUB",
	"assign_oper : YOP_AOMUL",
	"assign_oper : YOP_AODIV",
	"assign_oper : YOP_AOCAT",
	"cmdlist : command",
	"cmdlist : command cmdpipe",
	"cmdpipe : /* empty */",
	"cmdpipe : cmdpipe pipe",
	"cmdpipe : cmdpipe pipe command",
	"pipe : '|' opnl",
	"pipe : Y_ALLPIPE opnl",
	"command : tasknam",
	"command : tasknam BARG",
	"command : tasknam BARG args EARG",
	"args : DELIM",
	"args : DELIM arglist",
	"args : arglist",
	"arglist : arg",
	"arglist : arglist DELIM arg",
	"arg : /* empty */",
	"arg : expr0",
	"arg : ref",
	"arg : ref '=' expr0",
	"arg : ref '=' ref",
	"arg : param '+'",
	"arg : param '-'",
	"arg : '<' file",
	"arg : '>' file",
	"arg : Y_ALLREDIR file",
	"arg : Y_APPEND file",
	"arg : Y_ALLAPPEND file",
	"arg : Y_GSREDIR file",
	"file : expr0",
	"file : param",
	"immed : equals expr0",
	"immed : equals ref",
	"inspect : ref equals",
	"osesc : Y_OSESC",
	"popstk : equals",
	"iferr : iferr_stat",
	"iferr_stat : iferr_tok",
	"iferr_stat : iferr_tok c_blk",
	"iferr_stat : iferr_tok c_blk op_then opnl xstmt",
	"iferr_else : iferr_stat Y_ELSE",
	"iferr_else : iferr_stat Y_ELSE opnl xstmt",
	"iferr_tok : Y_IFERR",
	"iferr_tok : Y_IFNOERR",
	"op_then : /* empty */",
	"op_then : Y_THEN",
	"if : if_stat",
	"if_stat : Y_IF LP expr RP",
	"if_stat : Y_IF LP expr RP opnl xstmt",
	"ifelse : if_stat Y_ELSE",
	"ifelse : if_stat Y_ELSE opnl xstmt",
	"while : Y_WHILE LP",
	"while : Y_WHILE LP expr RP",
	"while : Y_WHILE LP expr RP opnl xstmt",
	"for : Y_FOR LP opnl xassign ';' opnl",
	"for : Y_FOR LP opnl xassign ';' opnl xexpr ';' opnl",
	"for : Y_FOR LP opnl xassign ';' opnl xexpr ';' opnl xassign RP opnl",
	"for : Y_FOR LP opnl xassign ';' opnl xexpr ';' opnl xassign RP opnl stmt",
	"xassign : assign",
	"xassign : /* empty */",
	"xexpr : expr",
	"xexpr : /* empty */",
	"switch : Y_SWITCH opnl LP opnl expr opnl RP opnl",
	"switch : Y_SWITCH opnl LP opnl expr opnl RP opnl xstmt",
	"case : Y_CASE",
	"case : Y_CASE const_expr_list ':' opnl",
	"case : Y_CASE const_expr_list ':' opnl xstmt",
	"default : Y_DEFAULT ':' opnl",
	"default : Y_DEFAULT ':' opnl xstmt",
	"next : Y_NEXT",
	"break : Y_BREAK",
	"return : Y_RETURN",
	"return : Y_RETURN expr",
	"end_stmt : Y_END NL",
	"label_stmt : Y_IDENT ':' opnl",
	"label_stmt : Y_IDENT ':' opnl xstmt",
	"goto : Y_GOTO Y_IDENT",
	"nullstmt : ';'",
	"nullstmt : ';' NL",
	"xstmt : /* empty */",
	"xstmt : stmt",
	"xstmt : var_decl_stmt",
	"xstmt : error NL",
	"const_expr_list : const_expr",
	"const_expr_list : const_expr DELIM const_expr_list",
	"const_expr : const",
	"opnl : /* empty */",
	"opnl : NL",
	"ref : param",
	"ref : param",
	"ref : param '[' index_list ']'",
	"index_list : index",
	"index_list : index",
	"index_list : index DELIM index_list",
	"index : expr1",
	"index : ref",
	"index : '*'",
	"index : Y_CONSTANT",
	"intrins : Y_IDENT",
	"param : Y_IDENT",
	"tasknam : Y_IDENT",
	"EOST : NL",
	"EOST : ';'",
	"DELIM : ','",
	"BARG : /* empty */",
	"BARG : LP",
	"EARG : /* empty */",
	"EARG : RP",
	"LP : '('",
	"RP : ')'",
	"NL : Y_NEWLINE",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
# line 144 "grammar.y"
{
		    /* Done once on entry but after at least one call to
		     * yylex().  Good for initing parser flags.
		     * Note: this does not get called in procedure scripts.
		     */
		    if (cldebug)
			eprintf ("parse init (block)...\n");

		    errcnt     = 0;
		    err_cmdblk = 0;
		    dobkg      = 0;
		    inarglist  = 0;
		    parenlevel = 0;
		    bracelevel = 0;
		    tbrace     = 0;
		    dobrace    = 0;
		    in_iferr   = 0;
		    do_params  = YES;
		    last_parm  = NULL;
		    ifseen     = NULL;
		    label1     = NULL;
		    errmsg     = NULL;
		    parse_pfile= currentask->t_pfp;
		} break;
case 2:
# line 169 "grammar.y"
{
		    /* Prepare to rerun whatever was compiled last.
		     * Does not work for the debug commands builtin here.
		     */
		    if (parse_state != PARSE_FREE) {
			errmsg = "Illegal parser state.";
			EYYERROR;
		    }
		    rerun();
		    YYACCEPT;
		} break;
case 3:
# line 181 "grammar.y"
{
		    if (parse_state == PARSE_PARAMS) {
			errmsg = "Illegal parser state.";
			EYYERROR;
		    }
		} break;
case 4:
# line 187 "grammar.y"
{
		    if (sawnl && bracelevel == 0) {
			if (!errcnt)
			    compile (END);
			if (ifseen) {
			    /* Simulate an unput of what has been read
			     * from the current line.
			     */
			    ip_cmdblk = ifseen;
			}
			YYACCEPT;
		    }
		} break;
case 5:
# line 201 "grammar.y"
{
		    /* Parse the parameters in a script file.  This will
		     * normally be done on a call by pfileread().
		     */
		    if (parse_state != PARSE_PARAMS) {
			eprintf ("Illegal parser state.\n");
			errcnt++;
		    }
		    YYACCEPT;
		} break;
case 6:
# line 212 "grammar.y"
{
		    /* Parse the executable statements in a script.
		     */
		    if (parse_state != PARSE_BODY) {
			eprintf ("Illegal parser state.\n");
			errcnt++;
		    }
		    if (!errcnt)
			compile (END);
		    YYACCEPT;
		} break;
case 7:
# line 224 "grammar.y"
{
		    /* This catches errors that the two other error lines
		     * can't get, e.g. a missing `}' at the end of a script, 
		     * or errors occuring in interactive input.
		     */
		    yyerrok;

		    /* Discard everything and compile a null statement.
		     */
		    if (!errcnt) {
			do_params = YES;
			pc = currentask->t_bascode;
			if (parse_state != PARSE_PARAMS)
			    compile (END);

			topd = currentask->t_topd;
			topcs = currentask->t_topcs;

			/* Unlink any added parms.  Resetting of topd will
			 * already have reclaimed space.
			 */
			if (last_parm) {
			    last_parm->p_np = NULL;
			    currentask->t_pfp->pf_lastpp = last_parm;
			    last_parm = NULL;
			}
	 	    }

		    /* Print cmdblk and show position of error.
		     */
		    p_position();
		    if (currentask->t_flags & T_SCRIPT) 
			cl_error (E_UERR, "syntax error, line %d",
			    currentask->t_scriptln);
		    else
			cl_error (E_UERR, "syntax error");

		    YYACCEPT;
		} break;
case 9:
# line 266 "grammar.y"
{
		    /* debug are those debugging functions that
		     * should be run directly and not through a
		     * builtin task due to stack or other changes,
		     * ie, don't change what we are trying to show.
		     */
		    printf ("\n");
		} break;
case 11:
# line 276 "grammar.y"
{
		    d_d(); /* show dictionary/stack pointers */
		} break;
case 12:
# line 279 "grammar.y"
{ /* show a dictionary location	*/
		    if (stkop(yypvt[-0])->o_type & OT_INT) {
			int	idx;
			idx = stkop(yypvt[-0])->o_val.v_i;
			eprintf ("%d:\t%d (0%o)\n", idx, stack[idx],
				stack[idx]);
		    } else
			eprintf ("usage: D_PEEK <d. index>\n");
		} break;
case 13:
# line 288 "grammar.y"
{
		    d_stack (pc, 0, 0);		/* show compiled code	*/
		} break;
case 14:
# line 295 "grammar.y"
{ 
			/* Check for required params.
			 */
			if (!errcnt)
			    proc_params(n_procpar);
		} break;
case 15:
# line 303 "grammar.y"
{
			/* Initialize parser for procedure body.
			 */
			if (cldebug)
			    eprintf ("parse init (script_body)...\n");

			errcnt     = 0;
			err_cmdblk = 0;
			dobkg      = 0;
			inarglist  = 0;
			parenlevel = 0;
		        in_iferr   = 0;
			dobrace    = 0;
			bracelevel = PBRACE; /* disable lexmodes; force "end" */
			tbrace     = 0;
 			do_params  = NO;
			last_parm  = NULL;
			ifseen     = NULL;
			label1     = NULL;
			parse_pfile= currentask->t_pfp;
		} break;
case 17:
# line 329 "grammar.y"
{  
			/* Initialize parser for procedure parameters.
			 */
			if (cldebug)
			    eprintf ("parse init (proc_stmt)...\n");

			errcnt     = 0;
			err_cmdblk = 0;
			dobkg      = 0;
			inarglist  = 0;
			parenlevel = 0;
			bracelevel = PBRACE;
			tbrace     = 0;
		        dobrace    = 0;
		        in_iferr   = 0;
			do_params  = YES;
		        last_parm  = NULL;
			label1     = NULL;
		} break;
case 19:
# line 352 "grammar.y"
{
		    n_procpar = 0;
		} break;
case 21:
# line 361 "grammar.y"
{
		    n_procpar = 0;
		} break;
case 23:
# line 367 "grammar.y"
{ 
		    n_procpar = 1;
		    if (!errcnt)
			push (stkop(yypvt[-0]));
		} break;
case 24:
# line 372 "grammar.y"
{
		    n_procpar++;
		    if (!errcnt)
			push (stkop(yypvt[-0]));
		} break;
case 31:
# line 389 "grammar.y"
{
		    /* This catches errors in the parameter declarations
		     * of a procedure script.
		     */
		    yyerrok;

		    /* Discard everything and compile a null statement.
		     */
		    if (!errcnt) {
			do_params = YES;
			pc = currentask->t_bascode;
			if (parse_state != PARSE_PARAMS)
			    compile (END);

			topd = currentask->t_topd;
			topcs = currentask->t_topcs;

			/* Unlink any added parms.  Resetting of topd will
			 * already have reclaimed space.
			 */
			if (last_parm) {
			    last_parm->p_np = NULL;
			    currentask->t_pfp->pf_lastpp = last_parm;
			    last_parm = NULL;
			}
	 	    }

		    /* Print cmdblk and show position of error.  We know
		     * we're parsing a procedure script, so print the line
		     * number too.
		     */
		    p_position();
		    cl_error (E_UERR, "syntax error, line %d", 
			currentask->t_scriptln);
		} break;
case 32:
# line 426 "grammar.y"
{
	 	        /* For in-line definitions we don't want
			 * to freeze stuff on the dictionary, so
			 * only allow additions if the dictionary
			 * is the same as at the beginning of the task.
			 */
			if (!errcnt) {
			    if (parse_state != PARSE_PARAMS) {
				if (currentask->t_topd != topd)
				    cl_error (E_UERR, illegalvar);
				last_parm = currentask->t_pfp->pf_lastpp;
			    }
			}

		        /* Increment bracelevel temporarily to defeat command
			 * mode, in case this is an in-line declaration and
			 * lexmodes=yes.
			 */
			bracelevel += PBRACE;
			tbrace++;

		} break;
case 33:
# line 447 "grammar.y"
{
		      	/* Update dictionary to include these definitions.
			 */
			if (!errcnt) {
			    if (parse_state != PARSE_PARAMS) {
				currentask->t_topd = topd;
				last_parm = 0;
			    }
			}

			/* Restore command mode */
			bracelevel -= PBRACE;
			tbrace--;
		} break;
case 34:
# line 463 "grammar.y"
{ vartype = V_BOOL; } break;
case 35:
# line 464 "grammar.y"
{ vartype = V_STRING; } break;
case 36:
# line 465 "grammar.y"
{ vartype = V_REAL; } break;
case 37:
# line 466 "grammar.y"
{ vartype = V_FILE; } break;
case 38:
# line 467 "grammar.y"
{ vartype = V_GCUR; } break;
case 39:
# line 468 "grammar.y"
{ vartype = V_IMCUR; } break;
case 40:
# line 469 "grammar.y"
{ vartype = V_UKEY; } break;
case 41:
# line 470 "grammar.y"
{ vartype = V_PSET; } break;
case 42:
# line 471 "grammar.y"
{ vartype = V_INT; } break;
case 43:
# line 472 "grammar.y"
{ vartype = V_STRUCT; } break;
case 46:
# line 479 "grammar.y"
{
			if (!errcnt) {
			    if (pp != NULL) {
				if (n_aval > 1)
				    pp->p_type |= PT_ARRAY;

				if (pp->p_type & PT_ARRAY)
				    do_arrayinit (pp, n_aval, index_cnt);
		      		else
		 		    do_scalarinit (pp, inited);
			    }
			}
		} break;
case 47:
# line 497 "grammar.y"
{
			if (!errcnt) {
			    if (pp != NULL) {
				if (!do_params)
				    cl_error (E_UERR, badparm, pp->p_name);

				if (n_aval > 1)
				    pp->p_type |= PT_ARRAY;

				if (pp->p_type & PT_ARRAY)
				    do_arrayinit (pp, n_aval, index_cnt);
		      		else
		 		    do_scalarinit (pp, n_aval);
			    }
			}
		} break;
case 48:
# line 515 "grammar.y"
{
			inited = NO;
			n_aval = 0;
		} break;
case 49:
# line 519 "grammar.y"
{
			n_aval = 0;
		} break;
case 50:
# line 522 "grammar.y"
{
			inited = YES;
		} break;
case 51:
# line 527 "grammar.y"
{
		    index_cnt = 0;
		    if (!errcnt)
			pp = initparam (stkop(yypvt[-0]), do_params, vartype, varlist);
		} break;
case 52:
# line 532 "grammar.y"
{
		    int  itemp;

		    if (!errcnt) {
			pp = initparam (stkop(yypvt[-0]), do_params, vartype, varlist);

			if (pp != NULL) {
			    itemp = (pp->p_type & OT_BASIC) == pp->p_type;
			    itemp = itemp && !varlist;
			    if (itemp)
				pp->p_type |= PT_ARRAY;
			    else
				cl_error (E_UERR, inval_arr, pp->p_name);
			}
		    }
		} break;
case 54:
# line 551 "grammar.y"
{
			varlist = NO;
			index_cnt = 0;
		} break;
case 55:
# line 555 "grammar.y"
{
			if (!do_params) {
			    errmsg = locallist;
			    EYYERROR;
			}
			varlist = YES;
			index_cnt = 0;
			yyval = yypvt[-0];
		} break;
case 59:
# line 575 "grammar.y"
{
		    if (!errcnt) {
			if (pp != NULL) {
			    if (stkop(yypvt[-0])->o_type == OT_INT) {
				push (stkop(yypvt[-0])->o_val.v_i);
				push (1);
			    } else if (maybeindex) {
				/* Confusion between sexagesimal and index
				 * range.  Maybeindex is set only when operand
				 * is real.
			 	 */
				int  i1,i2;
				sexa_to_index (stkop(yypvt[-0])->o_val.v_r, &i1, &i2);
				push (i2-i1+1);
				push (i1);
			    } else {
				eprintf (inv_index, pp->p_name);
				EYYERROR;
			    }
			    index_cnt++;
			}
		    }
		} break;
case 60:
# line 598 "grammar.y"
{
			if (!errcnt) {
			    if (pp != NULL) {
				if (stkop(yypvt[-2])->o_type != OT_INT  ||
				    stkop(yypvt[-0])->o_type != OT_INT)
				    cl_error (E_UERR, inv_index, pp->p_name);
				else {
				    push (stkop(yypvt[-0])->o_val.v_i -
				          stkop(yypvt[-2])->o_val.v_i + 1);
				    push (stkop(yypvt[-2])->o_val.v_i);
			        }
			        index_cnt++;
			    }
			}
		} break;
case 63:
# line 619 "grammar.y"
{
			if (!errcnt) {
			    if (pp != NULL) {
				push (stkop(yypvt[-0]) );
				n_aval++;
			    }
			}
		} break;
case 64:
# line 628 "grammar.y"
{
			int   cnt;
			
			if (!errcnt)
			    if (pp != NULL) {
			    	if (stkop(yypvt[-3])->o_type != OT_INT)
				    cl_error (E_UERR, arrdeferr, pp->p_name);

			        cnt = stkop(yypvt[-3])->o_val.v_i;
			        if (cnt <= 0)
				    cl_error (E_UERR, arrdeferr, pp->p_name);

			        while (cnt-- > 0) {
				    push (stkop(yypvt[-1]));
				    n_aval++;
			        }
			    }
		} break;
case 67:
# line 655 "grammar.y"
{
		      	if (stkop(yypvt[-0])->o_type == OT_INT) {
			    stkop(yypvt[-0])->o_val.v_i *= yypvt[-1];
			    yyval = yypvt[-0];
			} else if (stkop(yypvt[-0])->o_type == OT_REAL) {
			    stkop(yypvt[-0])->o_val.v_r *= yypvt[-1];
			    yyval = yypvt[-0];
			} else {
			    errmsg = "Invalid constant in declaration.";
			    EYYERROR;
			}
		} break;
case 68:
# line 669 "grammar.y"
{ yyval =  1; } break;
case 69:
# line 670 "grammar.y"
{ yyval = -1; } break;
case 70:
# line 672 "grammar.y"
{
			/* Check if we already had an initialization. 
			 */
		      	if (!errcnt) {
			    if (inited && pp != NULL) {
				eprintf (twoinits, pp->p_name);
				EYYERROR;
			    }
			}
		} break;
case 71:
# line 682 "grammar.y"
{
		      	if (!errcnt) {
			    if (inited && pp != NULL) {
			        eprintf (twoinits, pp->p_name);
				EYYERROR;
			    }
			}
		} break;
case 75:
# line 697 "grammar.y"
{
			if (!errcnt)
			    if (pp != NULL)
			    	do_option (pp, stkop(yypvt[-2]), stkop(yypvt[-0]));
		} break;
case 78:
# line 713 "grammar.y"
{
		    if (!errcnt)
		        compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
		} break;
case 80:
# line 727 "grammar.y"
{
		    if  (!errcnt)
		        compile (PUSHCONST, stkop(yypvt[-0]));
		} break;
case 81:
# line 731 "grammar.y"
{
		    /* "gcur" is both a keyword and a CL global parameter,
		     * and must be built into the grammar here to permit
		     * reference of the parameter in expressions.
		     */
		    if (!errcnt)
			compile (PUSHPARAM, "gcur");
		} break;
case 82:
# line 739 "grammar.y"
{
		    if (!errcnt)
			compile (PUSHPARAM, "imcur");
		} break;
case 83:
# line 743 "grammar.y"
{
		    if (!errcnt)
			compile (PUSHPARAM, "ukey");
		} break;
case 84:
# line 747 "grammar.y"
{
		    if (!errcnt)
			compile (PUSHPARAM, "pset");
		} break;
case 86:
# line 755 "grammar.y"
{
		    if (!errcnt)
			compile (ADD);
		} break;
case 87:
# line 759 "grammar.y"
{
		    if (!errcnt)
			compile (SUB);
		} break;
case 88:
# line 763 "grammar.y"
{
		    if (!errcnt)
			compile (MUL);
		} break;
case 89:
# line 767 "grammar.y"
{
		    if (!errcnt)
			compile (DIV);
		} break;
case 90:
# line 771 "grammar.y"
{
		    if (!errcnt)
			compile (POW);
		} break;
case 91:
# line 775 "grammar.y"
{
		    struct	operand o;
		    if (!errcnt) {
			o.o_type = OT_INT;
			o.o_val.v_i = 2;
			compile (PUSHCONST, &o);
			compile (INTRINSIC, "mod");
		    }
		} break;
case 92:
# line 784 "grammar.y"
{
		    if (!errcnt)
			compile (CONCAT);
		} break;
case 93:
# line 788 "grammar.y"
{
		    if (!errcnt)
			compile (LT);
		} break;
case 94:
# line 792 "grammar.y"
{
		    if (!errcnt)
			compile (GT);
		} break;
case 95:
# line 796 "grammar.y"
{
		    if (!errcnt)
			compile (LE);
		} break;
case 96:
# line 800 "grammar.y"
{
		    if (!errcnt)
			compile (GE);
		} break;
case 97:
# line 804 "grammar.y"
{
		    if (!errcnt)
			compile (EQ);
		} break;
case 98:
# line 808 "grammar.y"
{
		    if (!errcnt)
			compile (NE);
		} break;
case 99:
# line 812 "grammar.y"
{
		    if (!errcnt)
			compile (OR);
		} break;
case 100:
# line 816 "grammar.y"
{
		    if (!errcnt)
			compile (AND);
		} break;
case 101:
# line 820 "grammar.y"
{
		    if (!errcnt)
			compile (NOT);
		} break;
case 102:
# line 824 "grammar.y"
{
		    if (!errcnt)
			compile (CHSIGN);
		} break;
case 103:
# line 829 "grammar.y"
{
		    /* Free format scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 104:
# line 833 "grammar.y"
{
		    if (!errcnt) {
			struct	operand o;
			o.o_type = OT_INT;
		        o.o_val.v_i = pop();	/* get total number of args*/
			compile (PUSHCONST, &o);
			compile (SCAN);
		    }
		} break;
case 105:
# line 842 "grammar.y"
{
		    /* Formatted scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 106:
# line 846 "grammar.y"
{
		    if (!errcnt) {
			struct	operand o;

			/* Compile number of arguments. */
			o.o_type = OT_INT;
		        o.o_val.v_i = pop();
			compile (PUSHCONST, &o);

			compile (SCANF);
		    }
		} break;
case 107:
# line 859 "grammar.y"
{
		    /* Free format scan from a parameter.  */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 108:
# line 863 "grammar.y"
{
		    if (!errcnt) {
			struct	operand o;
			o.o_type = OT_INT;
			o.o_val.v_i = pop();	/* get total number of args*/
			compile (PUSHCONST, &o);
			compile (FSCAN);
		    }
		} break;
case 109:
# line 873 "grammar.y"
{
		    /* Formatted scan from a parameter.
		     * fscanf (param, format, arg1, ...)
		     */
		    if (!errcnt) {
			compile (PUSHCONST, stkop (yypvt[-1]));
		        push (1);	/* use control stack to count args */
		    }
		} break;
case 110:
# line 881 "grammar.y"
{
		    if (!errcnt) {
			struct	operand o;

			/* Compile number of arguments. */
			o.o_type = OT_INT;
			o.o_val.v_i = pop();
			compile (PUSHCONST, &o);

			compile (FSCANF);
		    }
		} break;
case 111:
# line 894 "grammar.y"
{
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 112:
# line 897 "grammar.y"
{
		    if (!errcnt) {
			struct	operand o;
			o.o_type = OT_INT;
			o.o_val.v_i = pop();
			compile (PUSHCONST, &o);
			compile (INTRINSIC, stkop(yypvt[-4])->o_val.v_s);
		    }
		} break;
case 114:
# line 912 "grammar.y"
{
			/* The YACC value of this must match normal intrinsics
			 * so we must generate an operand with the proper
			 * string. 
			 */
			if (!errcnt)
			    yyval = addconst ("int", OT_STRING);
		} break;
case 115:
# line 920 "grammar.y"
{
			if (!errcnt)
			    yyval = addconst ("real", OT_STRING);
		} break;
case 116:
# line 926 "grammar.y"
{
		    if (!errcnt) {
		        push (pop() + 1);		/* inc num args	*/
		    }
		} break;
case 118:
# line 939 "grammar.y"
{
                    if (!errcnt) {
                        compile (PUSHCONST, stkop (yypvt[-0]));
                        push (pop() + 1);               /* inc num args */
                    }
		} break;
case 119:
# line 945 "grammar.y"
{
                    if (!errcnt) {
                        compile (PUSHCONST, stkop (yypvt[-2]));
                        push (pop() + 1);               /* inc num args */
                    }
		} break;
case 121:
# line 956 "grammar.y"
{
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		} break;
case 122:
# line 960 "grammar.y"
{
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		} break;
case 147:
# line 1000 "grammar.y"
{
		    bracelevel++;
		} break;
case 148:
# line 1002 "grammar.y"
{
		    --bracelevel;
		} break;
case 152:
# line 1014 "grammar.y"
{
			--parenlevel;
 			if (!errcnt)
		      	    compile (ASSIGN, stkop(yypvt[-2])->o_val.v_s);
		} break;
case 153:
# line 1019 "grammar.y"
{
			/* Old code pushed a constant rather than a param
			 * when not within braces.  This doesn't seem
			 * to be what most people want.
			 */
			--parenlevel;
			if (!errcnt) {
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
		            compile (ASSIGN, stkop(yypvt[-2])->o_val.v_s);
		    	}
		} break;
case 154:
# line 1030 "grammar.y"
{
			parenlevel++;
		} break;
case 155:
# line 1033 "grammar.y"
{
		      	--parenlevel;
			if (!errcnt)
		  	    compile (yypvt[-1], stkop(yypvt[-3])->o_val.v_s);
		} break;
case 156:
# line 1042 "grammar.y"
{
			parenlevel++;
		} break;
case 157:
# line 1047 "grammar.y"
{ yyval = ADDASSIGN; } break;
case 158:
# line 1048 "grammar.y"
{ yyval = SUBASSIGN; } break;
case 159:
# line 1049 "grammar.y"
{ yyval = MULASSIGN; } break;
case 160:
# line 1050 "grammar.y"
{ yyval = DIVASSIGN; } break;
case 161:
# line 1051 "grammar.y"
{ yyval = CATASSIGN; } break;
case 162:
# line 1054 "grammar.y"
{
		    npipes = 0;
		} break;
case 163:
# line 1056 "grammar.y"
{
		    if (!errcnt) {
			compile (EXEC);
			if (npipes > 0)
			    compile (RMPIPES, npipes);
		    }
		} break;
case 165:
# line 1066 "grammar.y"
{
		    /* Pipefiles must be allocated at run time using a stack
		     * to permit pipe commands within loops, and to permit
		     * scripts called in a pipe to themselves contain pipe
		     * commands.  ADDPIPE allocates a new pipefile on the
		     * pipe stack and pushes its name on the operand stack.
		     * GETPIPE pushes the pipefile at the top of the pipe
		     * stack onto the operand stack.  RMPIPES removes N pipes
		     * from the pipe stack, and deletes the physical pipefiles.
		     */

		    if (!newstdout) {
			/* When the runtime code creates the pipe it needs to
			 * know the identity of the two tasks sharing the pipe
			 * to determine what type of pipe to create (text or
			 * binary).  Save the pc of the ADDPIPE instruction
			 * so that we can backpatch it below with a pointer to
			 * the name of the second task in the pipe (ADDPIPE
			 * will be called during startup of the first task
			 * hence will know its name).
			 */
			pipe_pc = compile (ADDPIPE, NULL);

			if (yypvt[-0] == 1)
			    compile (REDIR);
			else
			    compile (ALLREDIR);
			compile (EXEC);

		    } else {
			eprintf ("multiple redirection\n");
			YYERROR;
		    }

		} break;
case 166:
# line 1100 "grammar.y"
{
		    /* Compile the GETPIPE instruction with the name of the
		     * second task in the current pipe, and backpatch the
		     * matching ADDPIPE instruction with the PC of the GETPIPE.
		     */
		    (coderef(pipe_pc))->c_args = compile (GETPIPE, curr_task);
		    compile (REDIRIN);
		    npipes++;		/* Overflow checking is in ADDPIPE */
		} break;
case 167:
# line 1111 "grammar.y"
{
		    yyval = 1;
		} break;
case 168:
# line 1114 "grammar.y"
{
		    yyval = 2;
		} break;
case 169:
# line 1119 "grammar.y"
{
		    char    *ltname;

		    ltname = stkop(yypvt[-0])->o_val.v_s;
		    compile (CALL, ltname);
		    strcpy (curr_task, ltname);

		    /* The FPRINT task is special; the first arg
		     * is the destination and must be compiled as
		     * a string constant no matter what.  Set flag
		     * so that 'arg' compiles PUSHCONST.
		     */
		    printstmt = (strcmp (ltname, "fprint") == 0);

		    /* Ditto with SCAN; all the arguments are call by
		     * reference and must be compiled as string constants.
		     */
		    scanstmt = (strcmp (ltname, "scan") == 0 ||
				strcmp (ltname, "scanf") == 0);

		    absmode = 0;
		    posit = 0;
		    newstdout = 0;
		    parenlevel = 0;
		} break;
case 170:
# line 1143 "grammar.y"
{
		    inarglist = 1;
		} break;
case 171:
# line 1145 "grammar.y"
{
		    extern char *onerr_handler;

		    inarglist = 0;
		    parenlevel = 0;
		    scanstmt = 0;
		} break;
case 172:
# line 1154 "grammar.y"
{
		    /* (,x) equates to nargs == 2.  Call posargset with
		     * negative dummy argument to bump nargs.
		     */
		    if (!errcnt) {
			compile (POSARGSET, -1);
			posit++;
			printstmt = 0;
			scanstmt = 0;
		    }
		} break;
case 177:
# line 1173 "grammar.y"
{
		    if (!errcnt) {
			if (posit > 0) {		/* not first time */
			    compile (POSARGSET, -posit);
			    printstmt = 0;
			    scanstmt = 0;
			}
			posit++;
		    }
		} break;
case 178:
# line 1183 "grammar.y"
{
		    if (absmode) {
			errmsg = posfirst;
			EYYERROR;
		    } else
			if (!errcnt)
			    compile (POSARGSET, posit++);
		} break;
case 179:
# line 1191 "grammar.y"
{
		    if (absmode) {
			errmsg = posfirst;
			EYYERROR;
		    } else if (!errcnt) {
			if (scanstmt) {
			    char    pname[SZ_FNAME];
			    char    *pk, *t, *p, *f;
			    struct  pfile *pfp;
			    struct  operand o;

			    /* If no task name specified check the pfile for
			     * the task containing the scan statement for the
			     * named parameter.
			     */
			    breakout (stkop(yypvt[-0])->o_val.v_s, &pk, &t, &p, &f);
			    pfp = currentask->t_pfp;
			    if (*pk == NULL && *t == NULL &&
				pfp && paramfind(pfp,p,0,1)) {

				sprintf (pname, "%s.%s",
				    currentask->t_ltp->lt_lname, p);
				if (*f) {
				    strcat (pname, ".");
				    strcat (pname, f);
				}
			    } else
				strcpy (pname, stkop(yypvt[-0])->o_val.v_s);

			    o = *(stkop(yypvt[-0]));
			    o.o_val.v_s = pname;
			    compile (PUSHCONST, &o);
			    compile (INDIRPOSSET, posit++);

			} else if (parenlevel == 0 || printstmt) {
			    compile (PUSHCONST, stkop(yypvt[-0]));
			    compile (INDIRPOSSET, posit++);
			    /* only first arg of fprint stmt is special. */
			    printstmt = 0;

			} else {
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
			    compile (POSARGSET, posit++);
			}
		    }
		} break;
case 180:
# line 1237 "grammar.y"
{
		    absmode++;
		    if (!errcnt)
			compile (ABSARGSET, stkop(yypvt[-2])->o_val.v_s); 
		} break;
case 181:
# line 1242 "grammar.y"
{
		    absmode++;
		    if (!errcnt) {
			if (parenlevel == 0) {
			    compile (PUSHCONST, stkop(yypvt[-0]));
			    compile (INDIRABSSET, stkop(yypvt[-2])->o_val.v_s); 
			} else {
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
			    compile (ABSARGSET, stkop(yypvt[-2])->o_val.v_s);
			}
		    }
		} break;
case 182:
# line 1254 "grammar.y"
{
		    absmode++;
		    if (!errcnt)
			compile (SWON, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 183:
# line 1259 "grammar.y"
{
		    absmode++;
		    if (!errcnt)
			compile (SWOFF, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 184:
# line 1264 "grammar.y"
{
		    if (!errcnt)
			compile (REDIRIN);
		} break;
case 185:
# line 1268 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (REDIR);
		} break;
case 186:
# line 1273 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (ALLREDIR);
		} break;
case 187:
# line 1278 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (APPENDOUT);
		} break;
case 188:
# line 1283 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (ALLAPPEND);
		} break;
case 189:
# line 1288 "grammar.y"
{
		    if (!errcnt)
			compile (GSREDIR, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 190:
# line 1294 "grammar.y"
{
		    absmode++;
		    /* constant already pushed by expr0.
		     */
		} break;
case 191:
# line 1299 "grammar.y"
{
		    absmode++;
		    if (!errcnt) {
			if (parenlevel == 0)
			    compile (PUSHCONST, stkop(yypvt[-0]));
			else
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
			}
		} break;
case 192:
# line 1310 "grammar.y"
{
			--parenlevel;
			if (!errcnt)
			    compile (IMMED);
		} break;
case 193:
# line 1315 "grammar.y"
{
		      	--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop(yypvt[-0])->o_val.v_s);
		} break;
case 194:
# line 1322 "grammar.y"
{
			--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 195:
# line 1329 "grammar.y"
{
		    if (!errcnt)
			compile (OSESC, stkop(yypvt[-0])->o_val.v_s);
		} break;
case 196:
# line 1335 "grammar.y"
{
		    --parenlevel;
		    if (!errcnt)
			compile (IMMED);
		} break;
case 197:
# line 1345 "grammar.y"
{
		    /* pop BIFF addr and set branch to just after statement */
		    if (!errcnt) {
		        int   biffaddr = pop();
		    	coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
		    }
		    in_iferr = 0;
		} break;
case 198:
# line 1355 "grammar.y"
{
		    if (++in_iferr > 1) { 
			errmsg = nestediferr; 
			EYYERROR; 
		    } 
		    compile (CALL, "_errpsh");
		    compile (EXEC);

		} break;
case 199:
# line 1363 "grammar.y"
{
		    if (!errcnt) {
			struct	operand o;

			o.o_type = OT_INT;
			o.o_val.v_i = 0;
			compile (PUSHCONST, &o);       /* if (_errpop() != 0) */
			compile (INTRINSIC, "_errpop");
			compile (PUSHCONST, &o);
			compile (((iferr_tok == 0) ? NE : EQ));
			push (compile (BIFF, 0));
		    }
		} break;
case 200:
# line 1375 "grammar.y"
{
		    in_iferr--;
		} break;
case 201:
# line 1380 "grammar.y"
{
		    if (!errcnt) {
			/* Pop and save BIFF address, compile and push addr 
			 * of GOTO, and set BIFF branch to just after GOTO.
			 */
			int  biffaddr = pop();
			push (compile (GOTO, 0));
			coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
		    }

		} break;
case 202:
# line 1390 "grammar.y"
{
		    if (!errcnt) {
			/* Pop GOTO addr and set branch to just after statement
			 */
		    	int  gotoaddr = pop();
			coderef (gotoaddr)->c_args = pc - gotoaddr - SZ_CE;
		    }
		} break;
case 203:
# line 1400 "grammar.y"
{ iferr_tok = 0; } break;
case 204:
# line 1401 "grammar.y"
{ iferr_tok = 1; } break;
case 207:
# line 1412 "grammar.y"
{
		    /* pop BIFF addr and set branch to just after statement
		     */
		    int   biffaddr;
		    if (!errcnt) {
			biffaddr = pop();
		    	coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
		    }
		} break;
case 208:
# line 1423 "grammar.y"
{
			/* save BIFF addr so branch can be filled in 
			 */
			if (!errcnt)
			    push (compile (BIFF, 0));
		} break;
case 209:
# line 1428 "grammar.y"
{
			/* The shift/reduce conflict in the IF-IF/ELSE
			 * construct can cause errors in compilation
			 * because the IF statement can also be a
			 * terminal symbol, i.e. it may be all that
			 * is parsed in one call to the parser.
			 * The parser must look ahead one token
			 * to find if there is an else statement
			 * following.  If there is no following
			 * token an EOF may be detected prematurely.
			 * When the IF statement is being parsed not
			 * inside any braces, then when the next token
			 * is not an ELSE care must be taken that this
			 * token is seen on a subsequent invocation
			 * of the parser.  The `ifseen' flag is
			 * used within the support for the lexical
			 * analyzer located in `history.c'.
			 */
			if (cldebug) 
			    eprintf ("ytab: setting ifseen=yes\n");

			if (currentask->t_flags & T_INTERACTIVE)
			    ifseen = ip_cmdblk;
			else
			    ifseen = cmdblk;
		} break;
case 210:
# line 1456 "grammar.y"
{
		    int  biffaddr;

		    ifseen = NULL;
		    if (!errcnt) {
			/* Pop and save BIFF address, compile and push addr 
			 * of GOTO, and set BIFF branch to just after GOTO.
			 */
			biffaddr = pop();
			push (compile (GOTO, 0));
			coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
		    }
		} break;
case 211:
# line 1468 "grammar.y"
{
		    int  gotoaddr;
		    if (!errcnt) {
			/* Pop GOTO addr and set branch to just after statement
			 */
			gotoaddr = pop();
			coderef (gotoaddr)->c_args = pc - gotoaddr - SZ_CE;
		    }
		} break;
case 212:
# line 1479 "grammar.y"
{
		    /* Save starting addr of while expression.
		     */
		    if (!errcnt) {
			push (pc);
			loopincr();
		    }
		} break;
case 213:
# line 1486 "grammar.y"
{
		    /* Save BIFF addr so branch can be filled in.
		     */
		    if (!errcnt)
			push (compile (BIFF, 0));
		} break;
case 214:
# line 1491 "grammar.y"
{
		    int  biffaddr;

		    if (!errcnt) {
			/* Pop and save addr of BIFF instruction.	   */
			biffaddr = pop();
			/* Pop addr of expression and build a goto there.  */
			compile (GOTO, pop() - pc - SZ_CE);
			/* Now can set BIFF branch to just after statement.*/
			coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
			loopdecr();
		    }
		} break;
case 215:
# line 1524 "grammar.y"
{
			if (!errcnt)
			    push(pc);				/* Loop1: */
		} break;
case 216:
# line 1528 "grammar.y"
{
			if (!errcnt) {
			    if (for_expr)
				ppush (compile(BIFF, 0));	/* if (!e2) */

			    /* Add SZ_CE to skip following GOTO. 
			     */
			    ppush (pc+SZ_CE);			/* Loop2: */
			    ppush (compile(GOTO,0));		/* goto Loop3 */

			    /* Save current location as the destination
			     * for NEXT statements.
			     */
			    loopincr();
			}
		} break;
case 217:
# line 1544 "grammar.y"
{
			int  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-SZ_CE); 	/* Goto loop1 */
			    stmtaddr = pop();
			    coderef(stmtaddr)->c_args = pc - stmtaddr - SZ_CE;
			}
		} break;
case 218:
# line 1554 "grammar.y"
{
			int  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-SZ_CE); /* goto loop2 */

			    if (for_expr) {
				stmtaddr = pop();
				coderef(stmtaddr)->c_args = pc-stmtaddr-SZ_CE;
			    }
			    loopdecr();
			}
		} break;
case 221:
# line 1577 "grammar.y"
{
			for_expr = YES;
		} break;
case 222:
# line 1580 "grammar.y"
{
			for_expr = NO;
		} break;
case 223:
# line 1606 "grammar.y"
{
			if (!errcnt) {
			    push (compile(SWITCH));

			    /* Compile GOTO which will branch past end of
			     * switch.  This is needed if there is no DEFAULT.
			     */
			    compile (GOTO, 0);
			}
		} break;
case 224:
# line 1615 "grammar.y"
{
			/* Set up jumptable and pop space on stack.
			 */
			if (!errcnt)
			    setswitch();
		} break;
case 225:
# line 1623 "grammar.y"
{
			if (!errcnt) {
			    ncaseval = 0;
			    if (!in_switch()) {
				errmsg = "Improper CASE statement.";
				EYYERROR;
			    }
			}
		} break;
case 226:
# line 1631 "grammar.y"
{
			int  pcase;

			if (!errcnt) {
			    pcase = compile (CASE, ncaseval);

			    /* Fill in argument list. 
			     */
			    caseset (&(coderef(pcase)->c_args), ncaseval);
			    push (pcase);
			}
		} break;
case 227:
# line 1642 "grammar.y"
{
			/* Branch to end of switch block 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		} break;
case 228:
# line 1650 "grammar.y"
{
		      	/* Compile an operand to store the current PC.
			 */
			if (!errcnt) {
			    if (!in_switch()) {
				errmsg = "Improper DEFAULT statement.";
				EYYERROR;
			    }
			    push (compile(DEFAULT));
			}
		} break;
case 229:
# line 1660 "grammar.y"
{
		      	/* Branch past jump table. 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		} break;
case 230:
# line 1668 "grammar.y"
{
			/* All NEXT statements are backward references,
			 * so we simply store the addresses in an array.
			 */
			if (!errcnt) {
			    if (nestlevel)
				compile (GOTO, nextdest[nestlevel-1]-pc-SZ_CE);
			    else {
				errmsg = "NEXT outside of loop.";
				EYYERROR;
			    }
			}
		} break;
case 231:
# line 1683 "grammar.y"
{
			/* Each BREAK is a forward reference.  For the
			 * first BREAK in each loop we compile a
			 * GOTO statement which will be the object of
			 * all BREAK statements within the loop.  When
			 * the loop is terminated the target of this
			 * GOTO will be set.
			 */
			int  dest;

			if (!errcnt) {
			    if (!nestlevel) {
				errmsg = "Break outside of loop.";
				EYYERROR;
			    } else if ((dest = brkdest[nestlevel-1]) != 0)
		    		compile (GOTO, dest-pc-SZ_CE);
			    else {
				brkdest[nestlevel-1] = pc;
				compile (GOTO, 0);
			    }
			}
		} break;
case 232:
# line 1707 "grammar.y"
{
			if (!errcnt)
			    compile (END);
		} break;
case 233:
# line 1711 "grammar.y"
{
			/* Return values currently not implemented.
			 */
			eprintf ("Warning: return value ignored.\n");
			if (!errcnt)
			    compile (END);
		} break;
case 234:
# line 1723 "grammar.y"
{
			bracelevel -= PBRACE;
			if (bracelevel < 0) {
			    errmsg = "Too few left braces.";
			    EYYERROR;
			} else if (bracelevel > 0) {
			    errmsg = "Too few right braces.";
			    EYYERROR;
			}
		} break;
case 235:
# line 1735 "grammar.y"
{
			/* Put symbol in table in dictionary and
			 * process indirect references if present.
			 */
			struct label *l;

			if (!errcnt) {
			    l = getlabel (stkop(yypvt[-2]));

			    if (l == NULL) {
				l = setlabel (stkop(yypvt[-2]));
				l->l_loc = pc;
			    } else if (l->l_defined) {
			        errmsg = "Identical labels.";
				EYYERROR;
			    } else {
				/* Get this GOTO out of the
				 * indirect list so we can use
				 * the argument as the destination
				 */
				int  gotopc;
				gotopc = l->l_loc;
				unsetigoto (gotopc);

				/* Fix the indirect reference. 
				 */
				coderef(gotopc)->c_args = pc - gotopc - SZ_CE;
			    }
			    (l->l_defined)++;
			}
		} break;
case 237:
# line 1769 "grammar.y"
{
			/* Get the address corresponding to the label.
			 */
			struct label *l;

			if (!errcnt) {
			    l = getlabel (stkop(yypvt[-0]));

			    if (l != NULL)
				compile (GOTO, l->l_loc - pc - SZ_CE);
			    else {
				/* Ready for indirect GOTO 
				 */
				l = setlabel (stkop(yypvt[-0]));
				l->l_loc = pc;
				setigoto (compile(GOTO, 0));
				l->l_defined = 0;
			    }
			}
		} break;
case 240:
# line 1799 "grammar.y"
{ 
			/* Save pc before compiling statement for loop back
			 */
			stmt_pc = pc;
			n_oarr = 0;
			i_oarr = 0;
			ifseen = NULL;
		} break;
case 241:
# line 1807 "grammar.y"
{
		      	/* If there was an open reference compile the
			 * loop increment and goback.
			 */
			int push_pc;

			if (!errcnt) {
			    if (n_oarr) {
				compile (INDXINCR, stmt_pc-pc-4, 2*n_oarr+1);

				/* We are going to store initialization
				 * info for the implicit loop here.
				 * It is loopincr's responsibility to 
				 * branch around it.  This data is what
				 * should be pointed to by the special
				 * PUSHINDEX compiled at the first open
				 * array reference.
				 */
				push_pc = pop();  /* Location of PUSHINDEX */
				coderef(push_pc)->c_args = pc - push_pc - SZ_CE;

				stack[pc++] = n_oarr;
				for (i_oarr=0; i_oarr<n_oarr; i_oarr++) {
				    stack[pc++] = oarr_beg[i_oarr];
				    stack[pc++] = oarr_end[i_oarr];
				}

				/* Clear n_oarr.  This must be done here
				 * because we may have the end of a compound
				 * statement following on the heels of the
				 * end of the simple statement with the
				 * implicit loop.
				 */
				n_oarr = 0;
				i_oarr = 0;
			    }
			}
		} break;
case 243:
# line 1846 "grammar.y"
{
		    /* This should get most errors in executable statements
		     * or in the local variable declarations in a script.
		     */
		    yyerrok;

		    /* Get rid of any fake braces.
		     */
		    bracelevel -= tbrace;

		    /* Discard everything and compile a null statement.
		     */
		    if (!errcnt) {
			do_params = YES;
			pc = currentask->t_bascode;
			if (parse_state != PARSE_PARAMS)
			    compile (END);

			topd = currentask->t_topd;
			topcs = currentask->t_topcs;

			/* Unlink any added parms.  Resetting of topd will
			 * already have reclaimed space.
			 */
			if (last_parm) {
			    last_parm->p_np = NULL;
			    currentask->t_pfp->pf_lastpp = last_parm;
			    last_parm = NULL;
			}
 		    }

		    /* Tell user about the syntax error, printing the 
		     * offending line and position if possible.
		     */
		    if (currentask->t_flags & T_SCRIPT) {
		        if (errmsg != NULL) {
			    eprintf ("** Syntax error, line %d: %s\n",
			        currentask->t_scriptln, errmsg);
			} else {
			    eprintf ("** Syntax error, line %d\n",
			        currentask->t_scriptln);
			}
		    } else
			eprintf ("** Syntax error\n");
		    p_position();

		    if (!(currentask->t_flags & T_SCRIPT)) {
			/* If interactive, we're finished if not within braces.
			 */
			if (!bracelevel)
			    YYACCEPT;
		    }

		    /* Note that we do not call cl_error() here to abort, but
		     * continue on parsing the script for more syntax errors.
		     */
		    if (++errcnt > MAX_ERR)
			cl_error (E_UERR, "Too many syntax errors.");
		} break;
case 246:
# line 1911 "grammar.y"
{
				if (!errcnt) {
				    push(stkop(yypvt[-0])) ; 
				    ncaseval++;
				}
			} break;
case 249:
# line 1931 "grammar.y"
{
			int  dim, d, i1, i2, mode;

			/*  In command arguments, when not in parentheses
			 *  we just pass the param as a string constant.
			 */
			if (!errcnt) {
			    lastref = NO;
			    if (!inarglist || parenlevel) {
				i_oarr = 0;
				index_cnt = 0;

				strncpy (curr_param, stkop(yypvt[-0])->o_val.v_s, 
					SZ_FNAME);

				/* If a '.' is found in the name we have a 
				 * reference to an external task, or to a 
				 * specific field.  In these cases we don't 
				 * want implicit looping.
				 */
				if (index (curr_param, '.') == NULL) {
				    if ((dim = get_dim (curr_param)) > 0) {
					lastref = YES;
			        	for (d = 0; d < dim; d++) {
					    getlimits (curr_param, d, &i1, &i2);
					    mode = make_imloop (i1, i2);
					    if (mode)
						compile (PUSHINDEX, -1);
					    else
						push (compile(PUSHINDEX, 0));	
					}
			    		n_oarr = dim;
				    }
				}
			    }
			}
		} break;
case 250:
# line 1968 "grammar.y"
{
		    if (!errcnt) {
			strncpy (curr_param, stkop(yypvt[-0])->o_val.v_s, SZ_FNAME);
			index_cnt = 0;
		    }
		} break;
case 251:
# line 1975 "grammar.y"
{
		    if (i_oarr > 0  &&  n_oarr == 0)
			n_oarr = i_oarr;
		    i_oarr = 0;
		    lastref = YES;
		} break;
case 252:
# line 1983 "grammar.y"
{
			index_cnt = 1;
		} break;
case 253:
# line 1986 "grammar.y"
{
			index_cnt++;
		} break;
case 255:
# line 1992 "grammar.y"
{
			if (!errcnt)
			    compile (PUSHINDEX, 0);
		} break;
case 256:
# line 1997 "grammar.y"
{ 
			if (!errcnt) {
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
			    compile (PUSHINDEX, 0);
			}
		} break;
case 257:
# line 2003 "grammar.y"
{
			int  i1, i2, mode;

			if (!errcnt) {
			    if (index(curr_param, '.') != NULL) {
				errmsg = exlimits;
				EYYERROR;
			    }
			    if (getlimits (curr_param, index_cnt, &i1, &i2) 
				== ERR) {
				eprintf ("Implicit index error for %s.\n",
					curr_param);
				EYYERROR;
			    }
			    mode = make_imloop (i1, i2);
			    if (mode)
				compile (PUSHINDEX, mode);
			    else
				push (compile (PUSHINDEX, mode));
			}
		} break;
case 258:
# line 2024 "grammar.y"
{
			/*  There is an ambiguity in the grammar between
			 *  sexagesimal constants, and array range references.
			 *  Since the sexagesimal constants are recognized
			 *  in the lexical analyzer we can't just change the
			 *  grammar.  The kludge around this is to have
			 *  makeop set a flag telling us that the last
			 *  constant it compiled COULD have been an index
			 *  range.  We check the flag here and if it is
			 *  set we convert back and compile an implicit loop
			 *  otherwise we just push the constant.
			 */
			int  i1, i2, mode;

			if (!errcnt) {
			    if (maybeindex) {
				sexa_to_index (stkop(yypvt[-0])->o_val.v_r, &i1, &i2);
				mode = make_imloop (i1, i2);
				if (mode)
				    compile (PUSHINDEX, mode);
				else
				    push (compile (PUSHINDEX, mode));
			    } else {
				compile (PUSHCONST, stkop(yypvt[-0]));
				compile (PUSHINDEX, 0);
			    }
			}
		} break;
case 259:
# line 2058 "grammar.y"
{
		    yyval = yypvt[-0];
		} break;
case 260:
# line 2063 "grammar.y"
{
		    yyval = yypvt[-0];
		} break;
case 261:
# line 2068 "grammar.y"
{
		    yyval = yypvt[-0];
		} break;
case 263:
# line 2074 "grammar.y"
{
		    /* If statements are delimited by ';'s, do not execute
		     * until next newline EOST is received.
		     */
		    sawnl = 0;
		} break;
case 269:
# line 2096 "grammar.y"
{ parenlevel++; } break;
case 270:
# line 2099 "grammar.y"
{ --parenlevel; } break;
case 271:
# line 2102 "grammar.y"
{ sawnl = 1; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
