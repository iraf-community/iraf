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

int	dobkg = 0;		/* set when want to do code in bkground	*/
int	npipes = 0;		/* number of pipes in a command		*/
int	pipe_pc;		/* pc of last ADDPIPE instruction	*/
int	posit = 0;		/* positional argument count		*/
int	inarglist = 0;		/* set when in argument list		*/
int	parenlevel = 0;		/* level of paren nesting in command	*/
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

/* context-sensitive switches. technique is ok, but beware of nesting!
 */
static	int absmode = 0;	/* set by first absolute mode arg in cmd*/
static	int newstdout = 0;	/* set if stdout redirected in arg	*/
static	int bracelevel = 0;	/* set while in s_list to inhibit &	*/
static	int tbrace = 0;		/* fake braces for declarations		*/
static	int dobrace = 0;	/* handling braces.			*/
static	int sawnl = 0;		/* set when EOST was \n, else 0		*/
static	int printstmt = 0;	/* set when parsing FPRINT statement	*/
static	int scanstmt = 0;	/* set when parsing SCAN statement	*/

/* printf-format error messages.
 */
char	*posfirst = "All positional arguments must be first\n";
/* char	*look_parm= "Error searching for parameter `%s'."; */
char	*inval_arr= "Invalid array type for `%s'.";
char	*inv_index= "Invalid index definition for `%s'.";
char	*arrdeferr= "Error in array initialization for `%s'.";
/* char	*arrinbrack="Array initialization must be in brackets for `%s'."; */
char	*badparm =  "Parameter definition of `%s' is illegal here.";
char	*illegalvar="Illegal variable declarations.";
char	*locallist= "Local list variables are not permitted.";
char	*twoinits = "Two initializations for parameter `%s'.";
char	*exlimits = "Explicit range required for loop in external param.\n";

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
# define YOP_AOADD 296
# define YOP_AOSUB 297
# define YOP_AOMUL 298
# define YOP_AODIV 299
# define YOP_AOCAT 300
# define YOP_OR 301
# define YOP_AND 302
# define YOP_EQ 303
# define YOP_NE 304
# define YOP_LE 305
# define YOP_GE 306
# define YOP_CONCAT 307
# define YOP_NOT 308
# define UMINUS 309
# define YOP_POW 310
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

# line 2015 "grammar.y"


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
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	-2, 0,
-1, 58,
	91, 52,
	-2, 51,
-1, 61,
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	-2, 0,
-1, 102,
	61, 248,
	296, 248,
	297, 248,
	298, 248,
	299, 248,
	300, 248,
	91, 248,
	-2, 249,
-1, 105,
	91, 238,
	-2, 237,
-1, 139,
	269, 190,
	59, 190,
	-2, 77,
-1, 140,
	269, 191,
	59, 191,
	-2, 78,
-1, 156,
	40, 247,
	-2, 248,
-1, 196,
	269, 150,
	59, 150,
	41, 150,
	-2, 77,
-1, 197,
	269, 151,
	59, 151,
	41, 151,
	-2, 78,
-1, 279,
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	-2, 0,
-1, 287,
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	-2, 0,
-1, 288,
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	-2, 0,
-1, 289,
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	125, 146,
	-2, 0,
-1, 291,
	44, 241,
	-2, 240,
-1, 292,
	93, 243,
	44, 243,
	-2, 79,
-1, 293,
	93, 244,
	44, 244,
	-2, 78,
-1, 295,
	93, 246,
	44, 246,
	-2, 80,
-1, 350,
	266, 176,
	269, 176,
	59, 176,
	124, 176,
	44, 176,
	41, 176,
	-2, 77,
-1, 351,
	266, 177,
	269, 177,
	59, 177,
	124, 177,
	44, 177,
	41, 177,
	-2, 78,
-1, 352,
	91, 238,
	-2, 237,
-1, 375,
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	-2, 0,
-1, 386,
	266, 188,
	269, 188,
	59, 188,
	124, 188,
	44, 188,
	41, 188,
	-2, 77,
-1, 387,
	266, 189,
	269, 189,
	59, 189,
	91, 238,
	124, 189,
	44, 189,
	41, 189,
	-2, 237,
-1, 393,
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	-2, 0,
-1, 398,
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	-2, 0,
-1, 405,
	266, 178,
	269, 178,
	59, 178,
	124, 178,
	44, 178,
	41, 178,
	-2, 77,
-1, 406,
	266, 179,
	269, 179,
	59, 179,
	124, 179,
	44, 179,
	41, 179,
	-2, 78,
-1, 416,
	261, 228,
	271, 228,
	272, 228,
	273, 228,
	275, 228,
	276, 228,
	277, 228,
	278, 228,
	279, 228,
	280, 228,
	281, 228,
	282, 228,
	61, 228,
	123, 228,
	59, 228,
	-2, 0,
	};
# define YYNPROD 260
# define YYLAST 924
int yyact[]={

    61,    66,   348,   169,   281,   148,    68,   210,    89,   349,
   141,   326,   328,   119,     2,     9,   160,   193,   250,   290,
   188,   211,   189,   211,   232,    60,   208,   206,   208,   207,
    47,   209,   327,   209,    90,   211,   188,    51,   189,   364,
   208,   206,   185,   207,   277,   209,   211,   166,   234,   247,
    48,   208,   206,    88,   207,   188,   209,   189,   213,    40,
   214,    53,   181,    12,   179,   244,   120,    23,    31,    25,
    24,    26,    32,    27,    28,    29,    30,    45,   376,   298,
    51,   114,    54,    36,   256,   385,   199,   200,   201,   202,
   203,   238,   113,   105,   140,   182,   163,   344,   180,   168,
    21,   191,   415,   382,    65,   107,   246,   167,   113,   150,
    23,    31,    25,    24,    26,    32,    27,    28,    29,    30,
   115,   112,   336,   136,   353,   242,   354,   307,    46,   285,
   170,   165,   383,   194,   384,   113,   113,   194,    59,    65,
   197,   305,     7,    50,    16,   345,   291,   173,   233,    49,
    18,   288,   287,   205,   117,   221,   222,    41,   124,    18,
    42,   375,   164,   230,   416,   424,   235,   420,   411,   399,
   372,   236,   229,   228,   359,    92,   380,   378,   346,   296,
   239,   241,   174,   106,   308,   255,   204,   138,   198,   195,
   139,   137,   343,   172,   176,   177,    87,    86,    85,    84,
    38,    39,    83,    82,   254,    81,    59,   259,   260,   261,
   262,   263,   264,   265,   266,   267,   268,   269,   270,   271,
   272,   273,    80,   258,     5,   183,   257,    79,    78,   279,
    77,    76,   284,    75,   283,   280,   196,   282,   289,    74,
    73,    72,   293,    71,    70,   243,    69,   184,   185,   292,
   237,     8,     9,    67,    47,   248,   157,   309,   310,   297,
   332,   278,   155,   252,   369,   311,   312,   313,   314,   315,
   316,   317,   318,   319,   320,   321,   322,   323,   324,   325,
   276,   329,   184,   275,   333,   274,   339,    65,   330,   253,
   338,   212,   150,   334,   210,   187,   210,   186,   335,   286,
   351,   341,   342,   215,   216,   212,   299,   353,   210,   354,
    12,   340,   249,   116,   304,   306,    58,   363,   337,   210,
   360,   151,   152,   153,   154,   361,   356,   357,   355,   358,
   190,    65,    57,   294,   142,   156,   150,   373,   178,   374,
   301,   197,   300,   331,   365,    56,    55,    44,   368,    22,
   370,   158,   159,    15,   123,   122,   143,   144,   145,   146,
   393,    63,   347,    34,   379,   118,    33,   299,    14,     6,
   110,    37,   149,   398,     4,   329,     3,   397,   394,   395,
    10,   293,   396,   403,   351,   351,   406,     1,   292,   401,
   352,   404,   400,   366,   367,    65,   350,   402,   371,     0,
   150,   413,     0,     0,     0,   412,     0,   407,     0,     0,
     0,   377,   410,   408,   381,     0,   418,   251,     0,     0,
     0,   414,     0,   423,   283,   421,   425,   282,    88,     0,
   419,     0,   417,     0,     0,     0,     0,   196,     0,   422,
   388,   389,   390,   391,   392,     0,     0,   387,   387,   387,
   387,   387,   387,   386,   386,   386,   386,   386,   386,     0,
   211,     0,   409,     0,     0,   208,   206,    21,   207,   381,
   209,   302,   303,     0,   352,   352,     0,     0,     0,     0,
   350,   350,   405,   213,     0,   214,     0,     0,     0,     0,
   211,     0,     0,     0,   194,   208,   206,     0,   207,     0,
   209,     0,     0,     0,   151,   152,   153,   154,     0,   356,
   357,   355,   358,   213,     0,   214,     0,   142,   156,     0,
   211,     0,     0,     0,     0,   208,   206,     0,   207,   103,
   209,   107,   251,   362,   158,   159,     0,     0,     0,   143,
   144,   145,   146,   213,     0,   214,     0,     0,   151,   152,
   153,   154,     0,     0,     0,   149,     0,     0,     0,   147,
     0,   295,   156,     0,   211,     0,     0,     0,     0,   208,
   206,     0,   207,     0,   209,     0,     0,     0,   158,   159,
     0,     0,     0,   143,   144,   145,   146,   213,     0,   214,
     0,     0,     0,   104,     0,   211,     0,     0,     0,   149,
   208,   206,    17,   207,     0,   209,    64,     0,     0,     0,
     0,     0,   151,   152,   153,   154,     0,     0,   213,     0,
   214,     0,     0,     0,     0,   142,   156,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    52,     0,   158,   159,     0,     0,     0,   143,   144,   145,
   146,     0,     0,   161,   162,     0,     0,   111,     0,     0,
     0,     0,     0,   149,    19,     0,   121,     0,   175,     0,
     0,   125,   126,   127,   128,   129,   130,    12,     0,     0,
     0,     0,     0,     0,   131,   132,   133,   134,     0,     0,
     0,     0,    12,     0,    23,    31,    25,    24,    26,    32,
    27,    28,    29,    30,     0,     0,     0,     0,     0,     0,
     0,   223,   224,   225,   226,   227,     0,     0,     0,     0,
     0,     0,     0,   231,   219,   220,   217,   218,   215,   216,
   212,    91,     0,   210,   240,     0,     0,     0,     0,     0,
     0,   102,    93,   108,   245,    94,    99,    98,    95,    96,
    97,   101,   100,     0,   219,   220,   217,   218,   215,   216,
   212,     0,     0,   210,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   219,   220,   217,   218,   215,   216,
   212,     0,     0,   210,     0,     0,     0,     0,     0,     0,
     0,     0,    62,     0,     0,    11,     0,     0,    13,    20,
     0,     0,    35,     0,     0,     0,     0,     0,    20,     0,
     0,     0,    43,     0,     0,     0,     0,     0,     0,   220,
   217,   218,   215,   216,   212,     0,     0,   210,     0,     0,
    20,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   109,     0,     0,    20,     0,     0,
     0,   217,   218,   215,   216,   212,    20,     0,   210,     0,
     0,    20,    20,    20,    20,    20,    20,     0,     0,     0,
     0,     0,     0,     0,    20,    20,    20,    20,     0,     0,
   135,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   171,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   192 };
int yypact[]={

   -32, -1000,  -206, -1000, -1000,  -206,   408, -1000, -1000,  -206,
   -67, -1000, -1000, -1000,  -269,   408, -1000, -1000, -1000,  -206,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  -241, -1000,  -176,    41, -1000,  -209,
 -1000, -1000, -1000, -1000,   -17,  -206,    99, -1000, -1000,   470,
 -1000,  -206, -1000, -1000,    41,    91,   -42,    59, -1000, -1000,
  -241,  -219, -1000,    41,  -241, -1000, -1000, -1000,    41,    41,
    41,    41,    41,    41, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000,    41,    41,    41,    41, -1000, -1000,  -206,    44, -1000,
   355, -1000,  -258,    99,    99,  -206, -1000,    73, -1000, -1000,
  -224,   355,    72,  -206, -1000, -1000, -1000, -1000,    99, -1000,
   -67, -1000,   -17, -1000,   -23, -1000,    10, -1000, -1000, -1000,
  -206, -1000,    96,    91, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000,   355,  -210, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000,   355,   483,   355,
   355,    99,    99,    99,    99,    99, -1000, -1000, -1000, -1000,
 -1000, -1000,  -206,    99,  -222,  -206, -1000,   483, -1000, -1000,
  -206, -1000, -1000,     0,    99,   355, -1000, -1000,    66,    91,
    91, -1000, -1000, -1000,    99,    45, -1000,  -221, -1000, -1000,
    12,    -7, -1000, -1000, -1000,  -241, -1000, -1000,   355, -1000,
 -1000, -1000, -1000, -1000,   -40,   453,  -206,  -206,  -206,  -206,
  -206,  -206,  -206,  -206,  -206,  -206,  -206,  -206,  -206,  -206,
  -206,  -303,  -303, -1000, -1000, -1000,  -227, -1000,  -206,   355,
  -241,  -206,    71,    91, -1000, -1000, -1000,  -206,   291, -1000,
 -1000,   453,   -46,   -23,  -229,    -7,    -7, -1000,    91,    48,
 -1000,    69, -1000, -1000,   483, -1000,  -206,  -206, -1000,   355,
   355,   355,   355,   355,   355,   355,   355,   355,   355,   355,
   355,   355,   355,   355,  -239,   355,  -239,    91,   355,  -176,
   453,    63, -1000,    44,   355,  -206,  -222,  -176,  -176,  -176,
     4, -1000, -1000, -1000, -1000, -1000,    64, -1000, -1000, -1000,
    91, -1000,    96, -1000,    12, -1000,    -7,    -7,  -232, -1000,
 -1000,   -14,   -14,  -303,  -303, -1000,  -303,     9,   -16,   -16,
   -16,   -16,    -2,    -2,   527,   558,    96,    91,    91,   483,
    96, -1000,    92,   483, -1000, -1000,  -206,   355,   423, -1000,
 -1000, -1000, -1000,   -47, -1000,    91,    96, -1000,    91, -1000,
 -1000,    42,    89,   355,   355,   355,   355,   355,   355,  -206,
 -1000, -1000, -1000, -1000, -1000, -1000,  -239,  -239, -1000,   355,
 -1000,   355,  -206, -1000,    96,  -176, -1000,   291, -1000, -1000,
   247,   247,   355, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,  -176, -1000,    96,    91,   483,  -176,   355,
  -206, -1000, -1000,    91, -1000, -1000, -1000, -1000, -1000,  -239,
 -1000,    43,   483, -1000,    96,  -206,  -176, -1000, -1000, -1000,
  -241,    96,  -206, -1000,   470, -1000 };
int yypgo[]={

     0,   387,   802,   380,    83,    13,   376,   374,   371,   602,
   370,   369,   368,   142,   366,    77,     0,   365,   363,    93,
   361,   559,   355,    17,   354,    65,   353,   144,   143,   349,
   347,    82,   346,   345,   338,   332,   330,    64,   316,   313,
   312,    18,   225,    62,   297,   295,    98,    95,     5,    99,
     3,    10,   285,    11,   283,    12,   280,   264,   262,   261,
   260,   256,     1,   253,     6,   246,   244,   243,   241,   240,
   239,   233,   231,   230,   228,   227,   222,   205,   203,   202,
   199,   198,   197,   196,   193,   192,    34,   191,   188,     8,
   187,   186,   185,   184,   183,   182,   180,   179,   178,   177,
   176,     2,     9,    85,   175,   174,   173,   172,   170,     4,
   169,   168,   167,   165,   164,   162,    24,   161,   152,   151,
   149,   148,   147,    19,   146,   145 };
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
    62,    62,    62,    63,    63,    84,    85,    83,    15,    15,
    64,    64,    87,    64,    86,    88,    88,    88,    88,    88,
    90,    65,    91,    93,    91,    92,    92,    95,    97,    89,
   100,    98,    98,   101,   101,   102,   102,   102,   102,   102,
   102,   102,   102,   102,   102,   102,   102,   102,   103,   103,
    66,    66,    67,    68,    69,    70,   105,   104,   106,    71,
   107,   108,    72,   110,   112,   113,    73,   109,   109,   111,
   111,   114,    74,   115,   117,    75,   118,    76,    77,    78,
    80,    80,    17,   119,    81,    79,    82,    82,   120,     5,
     5,     5,   116,   116,   121,    16,    16,    50,   122,    50,
   123,   125,   123,   124,   124,   124,   124,    61,    19,    94,
     9,     9,    25,    96,    96,    99,    99,    21,    23,     2 };
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
     2,     2,     2,     2,     2,     2,     2,     4,     4,     4,
     4,     2,     2,     2,     4,     1,     1,    12,     0,     6,
     7,     7,     1,     9,     3,     3,     3,     3,     3,     3,
     1,     7,     0,     1,     9,     5,     5,     1,     1,    13,
     1,     6,     2,     2,     6,     1,     3,     3,     7,     7,
     5,     5,     5,     5,     5,     5,     5,     5,     3,     3,
     5,     5,     5,     3,     3,     3,     1,    15,     1,    11,
     1,     1,    17,     1,     1,     1,    33,     2,     0,     3,
     1,     1,    21,     1,     1,    15,     1,    11,     3,     3,
     3,     5,     5,     1,    10,     5,     2,     4,     1,     5,
     2,     5,     2,     6,     3,     0,     2,     3,     1,    11,
     3,     1,     8,     3,     3,     3,     3,     3,     3,     3,
     2,     3,     2,     0,     2,     0,     2,     3,     3,     3 };
int yychk[]={

 -1000,    -1,    46,    -6,    -7,   256,   -11,   -13,   283,   284,
    -3,    -2,   269,    -2,   -12,   -26,   -27,    -9,   -28,   256,
    -2,    59,   -29,   286,   289,   288,   290,   292,   293,   294,
   295,   287,   291,   -14,   -18,    -2,    -4,    -8,   267,   268,
   126,   -13,   -27,    -2,   -30,   -15,   -19,   271,    -5,  -120,
   -28,   256,    -9,   270,   -31,   -32,   -33,   -35,   -38,   -19,
    42,   -16,    -2,   -20,   -21,    40,   -62,   -63,   -64,   -65,
   -66,   -67,   -68,   -69,   -70,   -71,   -72,   -73,   -74,   -75,
   -76,   -77,   -78,   -79,   -80,   -81,   -82,   -83,   -50,   -89,
   -86,   261,  -104,   272,   275,   278,   279,   280,   277,   276,
   282,   281,   271,    59,   123,   -19,   -94,    61,   273,    -2,
   -10,    -9,   -25,    44,   123,    61,   -39,   -19,   -17,    -5,
   285,    -9,   -22,   -24,   -19,    -9,    -9,    -9,    -9,    -9,
    -9,    -9,    -9,    -9,    -9,    -2,   -86,   -87,   -90,   -49,
   -50,   -51,   270,   292,   293,   294,   295,   -21,   -48,   308,
    45,   257,   258,   259,   260,   -58,   271,   -61,   287,   288,
   274,   -21,   -21,   -16,  -115,    58,   271,   -48,   -49,   -50,
    58,    -2,   -84,  -122,   -95,   -21,    -4,   -31,   -34,   -37,
   -46,   -43,   -47,   -42,   270,   271,   -44,   -45,    43,    45,
   -36,    91,    -2,   -23,    41,   -25,   -49,   -50,   -88,   296,
   297,   298,   299,   300,   -91,   -48,    43,    45,    42,    47,
   310,    37,   307,    60,    62,   305,   306,   303,   304,   301,
   302,   -48,   -48,   -21,   -21,   -21,   -21,   -21,  -106,  -107,
   -16,   -21,  -116,  -121,   270,   -16,   -16,   -15,    91,   -96,
   -21,   -48,    59,   -25,   -25,   -21,    61,   270,   -37,   -40,
   -41,   -42,   270,   -19,   -48,   -92,   124,   266,   -23,   -16,
   -16,   -16,   -16,   -16,   -16,   -16,   -16,   -16,   -16,   -16,
   -16,   -16,   -16,   -16,   -52,   -54,   -56,   271,   -59,   -16,
   -48,  -109,   -64,   -50,   -16,    58,   -25,  -118,  -119,   -16,
  -123,  -124,   -51,   -50,    42,   270,   -97,   -23,   125,   -43,
   -46,   -47,   -42,   -42,   -25,    93,   -25,    58,   -93,   -16,
   -16,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,
   -48,   -48,   -48,   -48,   -48,   -48,   -53,   271,   -55,   -48,
   -53,   -25,   -60,   -48,    -5,   -23,    59,   -86,   -48,   -16,
  -116,    -5,    -5,   -85,    93,  -125,   -98,   -25,  -101,  -102,
   -49,   -50,   -19,    60,    62,   264,   262,   263,   265,  -105,
   -23,   -41,   -42,   -89,   271,   -23,   -25,   -25,   -23,   -57,
   -23,   -25,  -108,   -16,   -16,  -117,   125,   -25,   -99,   -23,
  -100,   -25,    61,    43,    45,  -103,   -49,   -19,  -103,  -103,
  -103,  -103,  -103,   -16,   -53,   -53,   -55,   -48,   -16,  -110,
   -23,    -5,  -123,  -101,  -102,   -49,   -50,    -5,   -23,   -25,
    -5,  -111,   -48,   -16,   -53,    59,  -114,   -23,   -16,    -5,
  -112,  -109,   -23,   -16,  -113,   -62 };
int yydef[]={

    -2,    -2,     0,     5,     6,     0,    -2,    15,    17,     0,
     8,     2,   259,     7,     0,    -2,    27,    29,    30,     0,
   250,   251,    32,    34,    35,    36,    37,    38,    39,    40,
    41,    42,    43,   148,     0,    76,    -2,     0,    11,     0,
    13,    14,    28,    31,     0,   235,    19,   248,     4,     0,
   230,     0,     9,    12,     0,    44,    46,    48,    -2,    54,
     0,    -2,   236,     0,    21,   257,   229,   123,     0,     0,
     0,     0,     0,     0,   130,   131,   132,   133,   134,   135,
   136,     0,     0,     0,     0,   141,   142,   143,   152,   160,
   194,   193,   195,     0,     0,   235,   213,     0,   218,   219,
     0,   220,    -2,   226,   145,    -2,   167,   154,     0,   231,
     8,    33,     0,   252,     0,    49,     0,    55,    16,   149,
     0,    18,     0,    22,    23,   124,   125,   126,   127,   128,
   129,   137,   138,   139,   140,   144,   192,     0,   162,    -2,
    -2,    79,    80,    81,    82,    83,    84,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    -2,   113,   114,   115,
   198,   200,   235,     0,     0,   235,   225,   221,    77,    78,
   235,   227,   148,     0,   253,     0,    10,    45,     0,    71,
    72,    61,    73,    63,    65,     0,    66,     0,    68,    69,
     0,    56,   222,    20,   258,     0,    -2,    -2,     0,   155,
   156,   157,   158,   159,   161,     0,   235,   235,   235,   235,
   235,   235,   235,   235,   235,   235,   235,   235,   235,   235,
   235,   101,   102,   103,   105,   107,     0,   111,   235,     0,
   208,   235,     0,   232,   234,   216,   223,   235,     0,   168,
   254,     0,     0,     0,     0,     0,     0,    67,    50,     0,
    57,    59,    65,    24,   153,   163,   235,   235,    85,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   117,     0,   117,     0,   120,    -2,
     0,     0,   207,   152,     0,   235,     0,    -2,    -2,    -2,
     0,    -2,    -2,    -2,   245,    -2,   175,   196,    47,    62,
    70,    74,     0,    75,     0,    53,     0,     0,     0,   165,
   166,    86,    87,    88,    89,    90,    91,    92,    93,    94,
    95,    96,    97,    98,    99,   100,     0,   118,     0,   116,
     0,   109,     0,   121,   199,   201,   235,     0,   235,   214,
   233,   217,   224,     0,   239,     0,   255,   170,   172,   173,
    -2,    -2,    -2,     0,     0,     0,     0,     0,     0,   235,
    64,    58,    60,   164,   249,   104,   117,   117,   108,     0,
   112,     0,   235,   203,     0,    -2,   147,     0,   169,   256,
   175,   175,     0,   180,   181,   182,    -2,    -2,   183,   184,
   185,   186,   187,    -2,   119,     0,     0,   122,    -2,   210,
   235,   215,   242,   171,   174,    -2,    -2,   197,   106,   117,
   202,     0,   209,   211,     0,   235,    -2,   110,   204,   212,
   208,     0,   235,   205,     0,   206 };
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
	"=",	61,
	"YOP_AOADD",	296,
	"YOP_AOSUB",	297,
	"YOP_AOMUL",	298,
	"YOP_AODIV",	299,
	"YOP_AOCAT",	300,
	"YOP_OR",	301,
	"YOP_AND",	302,
	"YOP_EQ",	303,
	"YOP_NE",	304,
	"<",	60,
	">",	62,
	"YOP_LE",	305,
	"YOP_GE",	306,
	"YOP_CONCAT",	307,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"%",	37,
	"YOP_NOT",	308,
	"UMINUS",	309,
	"YOP_POW",	310,
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
	"const_expr : Y_CONSTANT",
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
# line 137 "grammar.y"
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
		    do_params  = YES;
		    last_parm  = NULL;
		    ifseen     = NULL;
		    label1     = NULL;
		    parse_pfile= currentask->t_pfp;
		} break;
case 2:
# line 160 "grammar.y"
{
		    /* Prepare to rerun whatever was compiled last.
		     * Does not work for the debug commands builtin here.
		     */
		    if (parse_state != PARSE_FREE) {
			eprintf ("Illegal parser state.\n");
			EYYERROR;
		    }
		    rerun();
		    YYACCEPT;
		} break;
case 3:
# line 172 "grammar.y"
{
		    if (parse_state == PARSE_PARAMS) {
			eprintf ("Illegal parser state.\n");
			EYYERROR;
		    }
		} break;
case 4:
# line 178 "grammar.y"
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
# line 192 "grammar.y"
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
# line 203 "grammar.y"
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
# line 215 "grammar.y"
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
# line 257 "grammar.y"
{
		    /* debug are those debugging functions that
		     * should be run directly and not through a
		     * builtin task due to stack or other changes,
		     * ie, don't change what we are trying to show.
		     */
		    printf ("\n");
		} break;
case 11:
# line 267 "grammar.y"
{
		    d_d(); /* show dictionary/stack pointers */
		} break;
case 12:
# line 270 "grammar.y"
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
# line 279 "grammar.y"
{
		    d_stack (pc, 0);		/* show compiled code	*/
		} break;
case 14:
# line 286 "grammar.y"
{ 
			/* Check for required params.
			 */
			if (!errcnt)
			    proc_params(n_procpar);
		} break;
case 15:
# line 294 "grammar.y"
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
# line 319 "grammar.y"
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
			do_params  = YES;
		        last_parm  = NULL;
			label1     = NULL;
		} break;
case 19:
# line 341 "grammar.y"
{
		    n_procpar = 0;
		} break;
case 21:
# line 350 "grammar.y"
{
		    n_procpar = 0;
		} break;
case 23:
# line 356 "grammar.y"
{ 
		    n_procpar = 1;
		    if (!errcnt)
			push (stkop(yypvt[-0]));
		} break;
case 24:
# line 361 "grammar.y"
{
		    n_procpar++;
		    if (!errcnt)
			push (stkop(yypvt[-0]));
		} break;
case 31:
# line 378 "grammar.y"
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
# line 415 "grammar.y"
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
# line 436 "grammar.y"
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
# line 452 "grammar.y"
{ vartype = V_BOOL; } break;
case 35:
# line 453 "grammar.y"
{ vartype = V_STRING; } break;
case 36:
# line 454 "grammar.y"
{ vartype = V_REAL; } break;
case 37:
# line 455 "grammar.y"
{ vartype = V_FILE; } break;
case 38:
# line 456 "grammar.y"
{ vartype = V_GCUR; } break;
case 39:
# line 457 "grammar.y"
{ vartype = V_IMCUR; } break;
case 40:
# line 458 "grammar.y"
{ vartype = V_UKEY; } break;
case 41:
# line 459 "grammar.y"
{ vartype = V_PSET; } break;
case 42:
# line 460 "grammar.y"
{ vartype = V_INT; } break;
case 43:
# line 461 "grammar.y"
{ vartype = V_STRUCT; } break;
case 46:
# line 468 "grammar.y"
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
# line 486 "grammar.y"
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
# line 504 "grammar.y"
{
			inited = NO;
			n_aval = 0;
		} break;
case 49:
# line 508 "grammar.y"
{
			n_aval = 0;
		} break;
case 50:
# line 511 "grammar.y"
{
			inited = YES;
		} break;
case 51:
# line 516 "grammar.y"
{
		    index_cnt = 0;
		    if (!errcnt)
			pp = initparam (stkop(yypvt[-0]), do_params, vartype, varlist);
		} break;
case 52:
# line 521 "grammar.y"
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
# line 540 "grammar.y"
{
			varlist = NO;
			index_cnt = 0;
		} break;
case 55:
# line 544 "grammar.y"
{
			if (!do_params) {
			    eprintf (locallist);
			    EYYERROR;
			}
			varlist = YES;
			index_cnt = 0;
			yyval = yypvt[-0];
		} break;
case 59:
# line 564 "grammar.y"
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
# line 587 "grammar.y"
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
# line 608 "grammar.y"
{
			if (!errcnt) {
			    if (pp != NULL) {
				push (stkop(yypvt[-0]) );
				n_aval++;
			    }
			}
		} break;
case 64:
# line 617 "grammar.y"
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
# line 644 "grammar.y"
{
		      	if (stkop(yypvt[-0])->o_type == OT_INT) {
			    stkop(yypvt[-0])->o_val.v_i *= yypvt[-1];
			    yyval = yypvt[-0];
			} else if (stkop(yypvt[-0])->o_type == OT_REAL) {
			    stkop(yypvt[-0])->o_val.v_r *= yypvt[-1];
			    yyval = yypvt[-0];
			} else {
			    eprintf ("Invalid constant in declaration.\n");
			    EYYERROR;
			}
		} break;
case 68:
# line 658 "grammar.y"
{ yyval =  1; } break;
case 69:
# line 659 "grammar.y"
{ yyval = -1; } break;
case 70:
# line 661 "grammar.y"
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
# line 671 "grammar.y"
{
		      	if (!errcnt) {
			    if (inited && pp != NULL) {
			        eprintf (twoinits, pp->p_name);
				EYYERROR;
			    }
			}
		} break;
case 75:
# line 686 "grammar.y"
{
			if (!errcnt)
			    if (pp != NULL)
			    	do_option (pp, stkop(yypvt[-2]), stkop(yypvt[-0]));
		} break;
case 78:
# line 702 "grammar.y"
{
		    if (!errcnt)
		        compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
		} break;
case 80:
# line 716 "grammar.y"
{
		    if  (!errcnt)
		        compile (PUSHCONST, stkop(yypvt[-0]));
		} break;
case 81:
# line 720 "grammar.y"
{
		    /* "gcur" is both a keyword and a CL global parameter,
		     * and must be built into the grammar here to permit
		     * reference of the parameter in expressions.
		     */
		    if (!errcnt)
			compile (PUSHPARAM, "gcur");
		} break;
case 82:
# line 728 "grammar.y"
{
		    if (!errcnt)
			compile (PUSHPARAM, "imcur");
		} break;
case 83:
# line 732 "grammar.y"
{
		    if (!errcnt)
			compile (PUSHPARAM, "ukey");
		} break;
case 84:
# line 736 "grammar.y"
{
		    if (!errcnt)
			compile (PUSHPARAM, "pset");
		} break;
case 86:
# line 744 "grammar.y"
{
		    if (!errcnt)
			compile (ADD);
		} break;
case 87:
# line 748 "grammar.y"
{
		    if (!errcnt)
			compile (SUB);
		} break;
case 88:
# line 752 "grammar.y"
{
		    if (!errcnt)
			compile (MUL);
		} break;
case 89:
# line 756 "grammar.y"
{
		    if (!errcnt)
			compile (DIV);
		} break;
case 90:
# line 760 "grammar.y"
{
		    if (!errcnt)
			compile (POW);
		} break;
case 91:
# line 764 "grammar.y"
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
# line 773 "grammar.y"
{
		    if (!errcnt)
			compile (CONCAT);
		} break;
case 93:
# line 777 "grammar.y"
{
		    if (!errcnt)
			compile (LT);
		} break;
case 94:
# line 781 "grammar.y"
{
		    if (!errcnt)
			compile (GT);
		} break;
case 95:
# line 785 "grammar.y"
{
		    if (!errcnt)
			compile (LE);
		} break;
case 96:
# line 789 "grammar.y"
{
		    if (!errcnt)
			compile (GE);
		} break;
case 97:
# line 793 "grammar.y"
{
		    if (!errcnt)
			compile (EQ);
		} break;
case 98:
# line 797 "grammar.y"
{
		    if (!errcnt)
			compile (NE);
		} break;
case 99:
# line 801 "grammar.y"
{
		    if (!errcnt)
			compile (OR);
		} break;
case 100:
# line 805 "grammar.y"
{
		    if (!errcnt)
			compile (AND);
		} break;
case 101:
# line 809 "grammar.y"
{
		    if (!errcnt)
			compile (NOT);
		} break;
case 102:
# line 813 "grammar.y"
{
		    if (!errcnt)
			compile (CHSIGN);
		} break;
case 103:
# line 818 "grammar.y"
{
		    /* Free format scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 104:
# line 822 "grammar.y"
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
# line 831 "grammar.y"
{
		    /* Formatted scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 106:
# line 835 "grammar.y"
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
# line 848 "grammar.y"
{
		    /* Free format scan from a parameter.  */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 108:
# line 852 "grammar.y"
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
# line 862 "grammar.y"
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
# line 870 "grammar.y"
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
# line 883 "grammar.y"
{
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 112:
# line 886 "grammar.y"
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
# line 901 "grammar.y"
{
			/* The YACC value of this must match normal intrinsics
			 * so we must generate an operand with the proper
			 * string. 
			 */
			if (!errcnt)
			    yyval = addconst ("int", OT_STRING);
		} break;
case 115:
# line 909 "grammar.y"
{
			if (!errcnt)
			    yyval = addconst ("real", OT_STRING);
		} break;
case 116:
# line 915 "grammar.y"
{
		    if (!errcnt) {
		        push (pop() + 1);		/* inc num args	*/
		    }
		} break;
case 118:
# line 928 "grammar.y"
{
                    if (!errcnt) {
                        compile (PUSHCONST, stkop (yypvt[-0]));
                        push (pop() + 1);               /* inc num args */
                    }
		} break;
case 119:
# line 934 "grammar.y"
{
                    if (!errcnt) {
                        compile (PUSHCONST, stkop (yypvt[-2]));
                        push (pop() + 1);               /* inc num args */
                    }
		} break;
case 121:
# line 945 "grammar.y"
{
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		} break;
case 122:
# line 949 "grammar.y"
{
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		} break;
case 145:
# line 987 "grammar.y"
{
		    bracelevel++;
		} break;
case 146:
# line 989 "grammar.y"
{
		    --bracelevel;
		} break;
case 150:
# line 1001 "grammar.y"
{
			--parenlevel;
 			if (!errcnt)
		      	    compile (ASSIGN, stkop(yypvt[-2])->o_val.v_s);
		} break;
case 151:
# line 1006 "grammar.y"
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
case 152:
# line 1017 "grammar.y"
{
			parenlevel++;
		} break;
case 153:
# line 1020 "grammar.y"
{
		      	--parenlevel;
			if (!errcnt)
		  	    compile (yypvt[-1], stkop(yypvt[-3])->o_val.v_s);
		} break;
case 154:
# line 1029 "grammar.y"
{
			parenlevel++;
		} break;
case 155:
# line 1034 "grammar.y"
{ yyval = ADDASSIGN; } break;
case 156:
# line 1035 "grammar.y"
{ yyval = SUBASSIGN; } break;
case 157:
# line 1036 "grammar.y"
{ yyval = MULASSIGN; } break;
case 158:
# line 1037 "grammar.y"
{ yyval = DIVASSIGN; } break;
case 159:
# line 1038 "grammar.y"
{ yyval = CATASSIGN; } break;
case 160:
# line 1041 "grammar.y"
{
		    npipes = 0;
		} break;
case 161:
# line 1043 "grammar.y"
{
		    if (!errcnt) {
			compile (EXEC);
			if (npipes > 0)
			    compile (RMPIPES, npipes);
		    }
		} break;
case 163:
# line 1053 "grammar.y"
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
case 164:
# line 1087 "grammar.y"
{
		    /* Compile the GETPIPE instruction with the name of the
		     * second task in the current pipe, and backpatch the
		     * matching ADDPIPE instruction with the PC of the GETPIPE.
		     */
		    (coderef(pipe_pc))->c_args = compile (GETPIPE, curr_task);
		    compile (REDIRIN);
		    npipes++;		/* Overflow checking is in ADDPIPE */
		} break;
case 165:
# line 1098 "grammar.y"
{
		    yyval = 1;
		} break;
case 166:
# line 1101 "grammar.y"
{
		    yyval = 2;
		} break;
case 167:
# line 1106 "grammar.y"
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
case 168:
# line 1130 "grammar.y"
{
		    inarglist = 1;
		} break;
case 169:
# line 1132 "grammar.y"
{
		    inarglist = 0;
		    parenlevel = 0;
		    scanstmt = 0;
		} break;
case 170:
# line 1139 "grammar.y"
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
case 175:
# line 1158 "grammar.y"
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
case 176:
# line 1168 "grammar.y"
{
		    if (absmode) {
			eprintf (posfirst);
			EYYERROR;
		    } else
			if (!errcnt)
			    compile (POSARGSET, posit++);
		} break;
case 177:
# line 1176 "grammar.y"
{
		    if (absmode) {
			eprintf (posfirst);
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
case 178:
# line 1222 "grammar.y"
{
		    absmode++;
		    if (!errcnt)
			compile (ABSARGSET, stkop(yypvt[-2])->o_val.v_s); 
		} break;
case 179:
# line 1227 "grammar.y"
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
case 180:
# line 1239 "grammar.y"
{
		    absmode++;
		    if (!errcnt)
			compile (SWON, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 181:
# line 1244 "grammar.y"
{
		    absmode++;
		    if (!errcnt)
			compile (SWOFF, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 182:
# line 1249 "grammar.y"
{
		    if (!errcnt)
			compile (REDIRIN);
		} break;
case 183:
# line 1253 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (REDIR);
		} break;
case 184:
# line 1258 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (ALLREDIR);
		} break;
case 185:
# line 1263 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (APPENDOUT);
		} break;
case 186:
# line 1268 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (ALLAPPEND);
		} break;
case 187:
# line 1273 "grammar.y"
{
		    if (!errcnt)
			compile (GSREDIR, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 188:
# line 1279 "grammar.y"
{
		    absmode++;
		    /* constant already pushed by expr0.
		     */
		} break;
case 189:
# line 1284 "grammar.y"
{
		    absmode++;
		    if (!errcnt) {
			if (parenlevel == 0)
			    compile (PUSHCONST, stkop(yypvt[-0]));
			else
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
			}
		} break;
case 190:
# line 1295 "grammar.y"
{
			--parenlevel;
			if (!errcnt)
			    compile (IMMED);
		} break;
case 191:
# line 1300 "grammar.y"
{
		      	--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop(yypvt[-0])->o_val.v_s);
		} break;
case 192:
# line 1307 "grammar.y"
{
			--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 193:
# line 1314 "grammar.y"
{
		    if (!errcnt)
			compile (OSESC, stkop(yypvt[-0])->o_val.v_s);
		} break;
case 194:
# line 1320 "grammar.y"
{
		    --parenlevel;
		    if (!errcnt)
			compile (IMMED);
		} break;
case 195:
# line 1327 "grammar.y"
{
		    /* pop BIFF addr and set branch to just after statement
		     */
		    int   biffaddr;
		    if (!errcnt) {
			biffaddr = pop();
		    	coderef (biffaddr)->c_args = pc - biffaddr - 3;
		    }
		} break;
case 196:
# line 1338 "grammar.y"
{
			/* save BIFF addr so branch can be filled in 
			 */
			if (!errcnt)
			    push (compile (BIFF, 0));
		} break;
case 197:
# line 1343 "grammar.y"
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
case 198:
# line 1371 "grammar.y"
{
		    int  biffaddr;

		    ifseen = NULL;
		    if (!errcnt) {
			/* Pop and save BIFF address, compile and push addr 
			 * of GOTO, and set BIFF branch to just after GOTO.
			 */
			biffaddr = pop();
			push (compile (GOTO, 0));
			coderef (biffaddr)->c_args = pc - biffaddr - 3;
		    }
		} break;
case 199:
# line 1383 "grammar.y"
{
		    int  gotoaddr;
		    if (!errcnt) {
			/* Pop GOTO addr and set branch to just after statement
			 */
			gotoaddr = pop();
			coderef (gotoaddr)->c_args = pc - gotoaddr - 3;
		    }
		} break;
case 200:
# line 1394 "grammar.y"
{
		    /* Save starting addr of while expression.
		     */
		    if (!errcnt) {
			push (pc);
			loopincr();
		    }
		} break;
case 201:
# line 1401 "grammar.y"
{
		    /* Save BIFF addr so branch can be filled in.
		     */
		    if (!errcnt)
			push (compile (BIFF, 0));
		} break;
case 202:
# line 1406 "grammar.y"
{
		    int  biffaddr;

		    if (!errcnt) {
			/* Pop and save addr of BIFF instruction.	   */
			biffaddr = pop();
			/* Pop addr of expression and build a goto there.  */
			compile (GOTO, pop() - pc - 3);
			/* Now can set BIFF branch to just after statement.*/
			coderef (biffaddr)->c_args = pc - biffaddr - 3;
			loopdecr();
		    }
		} break;
case 203:
# line 1439 "grammar.y"
{
			if (!errcnt)
			    push(pc);				/* Loop1: */
		} break;
case 204:
# line 1443 "grammar.y"
{
			if (!errcnt) {
			    if (for_expr)
				ppush (compile(BIFF, 0));	/* if (!e2) */

			    /* Add 3 to skip following GOTO. 
			     */
			    ppush (pc+3);			/* Loop2: */
			    ppush (compile(GOTO,0));		/* goto Loop3 */

			    /* Save current location as the destination
			     * for NEXT statements.
			     */
			    loopincr();
			}
		} break;
case 205:
# line 1459 "grammar.y"
{
			int  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-3); 	/* Goto loop1 */
			    stmtaddr = pop();
			    coderef(stmtaddr)->c_args = pc - stmtaddr - 3;
			}
		} break;
case 206:
# line 1469 "grammar.y"
{
			int  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-3); /* goto loop2 */

			    if (for_expr) {
				stmtaddr = pop();
				coderef(stmtaddr)->c_args = pc - stmtaddr - 3;
			    }
			    loopdecr();
			}
		} break;
case 209:
# line 1492 "grammar.y"
{
			for_expr = YES;
		} break;
case 210:
# line 1495 "grammar.y"
{
			for_expr = NO;
		} break;
case 211:
# line 1521 "grammar.y"
{
			if (!errcnt) {
			    push (compile(SWITCH));

			    /* Compile GOTO which will branch past end of
			     * switch.  This is needed if there is no DEFAULT.
			     */
			    compile (GOTO, 0);
			}
		} break;
case 212:
# line 1530 "grammar.y"
{
			/* Set up jumptable and pop space on stack.
			 */
			if (!errcnt)
			    setswitch();
		} break;
case 213:
# line 1538 "grammar.y"
{
			if (!errcnt) {
			    ncaseval = 0;
			    if (!in_switch()) {
				eprintf ("Improper CASE statement.\n");
				EYYERROR;
			    }
			}
		} break;
case 214:
# line 1546 "grammar.y"
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
case 215:
# line 1557 "grammar.y"
{
			/* Branch to end of switch block 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		} break;
case 216:
# line 1565 "grammar.y"
{
		      	/* Compile an operand to store the current PC.
			 */
			if (!errcnt) {
			    if (!in_switch()) {
				eprintf ("Improper DEFAULT statement.\n");
				EYYERROR;
			    }
			    push (compile(DEFAULT));
			}
		} break;
case 217:
# line 1575 "grammar.y"
{
		      	/* Branch past jump table. 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		} break;
case 218:
# line 1583 "grammar.y"
{
			/* All NEXT statements are backward references,
			 * so we simply store the addresses in an array.
			 */
			if (!errcnt) {
			    if (nestlevel)
				compile (GOTO, nextdest[nestlevel-1]-pc-3);
			    else {
				eprintf ( "NEXT outside of loop.\n");
				EYYERROR;
			    }
			}
		} break;
case 219:
# line 1598 "grammar.y"
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
				eprintf ("Break outside of loop.\n");
				EYYERROR;
			    } else if ((dest = brkdest[nestlevel-1]) != 0)
		    		compile (GOTO, dest-pc-3);
			    else {
				brkdest[nestlevel-1] = pc;
				compile (GOTO, 0);
			    }
			}
		} break;
case 220:
# line 1622 "grammar.y"
{
			if (!errcnt)
			    compile (END);
		} break;
case 221:
# line 1626 "grammar.y"
{
			/* Return values currently not implemented.
			 */
			eprintf ("Warning: return value ignored.\n");
			if (!errcnt)
			    compile (END);
		} break;
case 222:
# line 1638 "grammar.y"
{
			bracelevel -= PBRACE;
			if (bracelevel < 0) {
			    eprintf ("Too few left braces.\n");
			    EYYERROR;
			} else if (bracelevel > 0) {
			    eprintf ("Too few right braces.\n");
			    EYYERROR;
			}
		} break;
case 223:
# line 1650 "grammar.y"
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
			        eprintf ("Identical labels.\n");
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
				coderef(gotopc)->c_args = pc - gotopc - 3;
			    }
			    (l->l_defined)++;
			}
		} break;
case 225:
# line 1684 "grammar.y"
{
			/* Get the address corresponding to the label.
			 */
			struct label *l;

			if (!errcnt) {
			    l = getlabel (stkop(yypvt[-0]));

			    if (l != NULL)
				compile (GOTO, l->l_loc - pc - 3);
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
case 228:
# line 1714 "grammar.y"
{ 
			/* Save pc before compiling statement for loop back
			 */
			stmt_pc = pc;
			n_oarr = 0;
			i_oarr = 0;
			ifseen = NULL;
		} break;
case 229:
# line 1722 "grammar.y"
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
				coderef(push_pc)->c_args = pc - push_pc - 3;

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
case 231:
# line 1761 "grammar.y"
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
		    if (currentask->t_flags & T_SCRIPT) 
			eprintf ("** Syntax error, line %d\n",
			    currentask->t_scriptln);
		    else
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
case 234:
# line 1821 "grammar.y"
{
				if (!errcnt) {
				    push(stkop(yypvt[-0])) ; 
				    ncaseval++;
				}
			} break;
case 237:
# line 1841 "grammar.y"
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
case 238:
# line 1878 "grammar.y"
{
		    if (!errcnt) {
			strncpy (curr_param, stkop(yypvt[-0])->o_val.v_s, SZ_FNAME);
			index_cnt = 0;
		    }
		} break;
case 239:
# line 1885 "grammar.y"
{
		    if (i_oarr > 0  &&  n_oarr == 0)
			n_oarr = i_oarr;
		    i_oarr = 0;
		    lastref = YES;
		} break;
case 240:
# line 1893 "grammar.y"
{
			index_cnt = 1;
		} break;
case 241:
# line 1896 "grammar.y"
{
			index_cnt++;
		} break;
case 243:
# line 1902 "grammar.y"
{
			if (!errcnt)
			    compile (PUSHINDEX, 0);
		} break;
case 244:
# line 1907 "grammar.y"
{ 
			if (!errcnt) {
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
			    compile (PUSHINDEX, 0);
			}
		} break;
case 245:
# line 1913 "grammar.y"
{
			int  i1, i2, mode;

			if (!errcnt) {
			    if (index(curr_param, '.') != NULL) {
				eprintf (exlimits);
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
case 246:
# line 1934 "grammar.y"
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
case 247:
# line 1968 "grammar.y"
{
		    yyval = yypvt[-0];
		} break;
case 248:
# line 1973 "grammar.y"
{
		    yyval = yypvt[-0];
		} break;
case 249:
# line 1978 "grammar.y"
{
		    yyval = yypvt[-0];
		} break;
case 251:
# line 1984 "grammar.y"
{
		    /* If statements are delimited by ';'s, do not execute
		     * until next newline EOST is received.
		     */
		    sawnl = 0;
		} break;
case 257:
# line 2006 "grammar.y"
{ parenlevel++; } break;
case 258:
# line 2009 "grammar.y"
{ --parenlevel; } break;
case 259:
# line 2012 "grammar.y"
{ sawnl = 1; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
