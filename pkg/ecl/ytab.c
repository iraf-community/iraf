/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     Y_SCAN = 258,
     Y_SCANF = 259,
     Y_FSCAN = 260,
     Y_FSCANF = 261,
     Y_OSESC = 262,
     Y_APPEND = 263,
     Y_ALLAPPEND = 264,
     Y_ALLREDIR = 265,
     Y_GSREDIR = 266,
     Y_ALLPIPE = 267,
     D_D = 268,
     D_PEEK = 269,
     Y_NEWLINE = 270,
     Y_CONSTANT = 271,
     Y_IDENT = 272,
     Y_WHILE = 273,
     Y_IF = 274,
     Y_ELSE = 275,
     Y_FOR = 276,
     Y_BREAK = 277,
     Y_NEXT = 278,
     Y_SWITCH = 279,
     Y_CASE = 280,
     Y_DEFAULT = 281,
     Y_RETURN = 282,
     Y_GOTO = 283,
     Y_PROCEDURE = 284,
     Y_BEGIN = 285,
     Y_END = 286,
     Y_BOOL = 287,
     Y_INT = 288,
     Y_REAL = 289,
     Y_STRING = 290,
     Y_FILE = 291,
     Y_STRUCT = 292,
     Y_GCUR = 293,
     Y_IMCUR = 294,
     Y_UKEY = 295,
     Y_PSET = 296,
     Y_IFERR = 297,
     Y_IFNOERR = 298,
     Y_THEN = 299,
     YOP_AOCAT = 300,
     YOP_AODIV = 301,
     YOP_AOMUL = 302,
     YOP_AOSUB = 303,
     YOP_AOADD = 304,
     YOP_OR = 305,
     YOP_AND = 306,
     YOP_NE = 307,
     YOP_EQ = 308,
     YOP_GE = 309,
     YOP_LE = 310,
     YOP_CONCAT = 311,
     UMINUS = 312,
     YOP_NOT = 313,
     YOP_POW = 314
   };
#endif
/* Tokens.  */
#define Y_SCAN 258
#define Y_SCANF 259
#define Y_FSCAN 260
#define Y_FSCANF 261
#define Y_OSESC 262
#define Y_APPEND 263
#define Y_ALLAPPEND 264
#define Y_ALLREDIR 265
#define Y_GSREDIR 266
#define Y_ALLPIPE 267
#define D_D 268
#define D_PEEK 269
#define Y_NEWLINE 270
#define Y_CONSTANT 271
#define Y_IDENT 272
#define Y_WHILE 273
#define Y_IF 274
#define Y_ELSE 275
#define Y_FOR 276
#define Y_BREAK 277
#define Y_NEXT 278
#define Y_SWITCH 279
#define Y_CASE 280
#define Y_DEFAULT 281
#define Y_RETURN 282
#define Y_GOTO 283
#define Y_PROCEDURE 284
#define Y_BEGIN 285
#define Y_END 286
#define Y_BOOL 287
#define Y_INT 288
#define Y_REAL 289
#define Y_STRING 290
#define Y_FILE 291
#define Y_STRUCT 292
#define Y_GCUR 293
#define Y_IMCUR 294
#define Y_UKEY 295
#define Y_PSET 296
#define Y_IFERR 297
#define Y_IFNOERR 298
#define Y_THEN 299
#define YOP_AOCAT 300
#define YOP_AODIV 301
#define YOP_AOMUL 302
#define YOP_AOSUB 303
#define YOP_AOADD 304
#define YOP_OR 305
#define YOP_AND 306
#define YOP_NE 307
#define YOP_EQ 308
#define YOP_GE 309
#define YOP_LE 310
#define YOP_CONCAT 311
#define UMINUS 312
#define YOP_NOT 313
#define YOP_POW 314




/* Copy the first part of user declarations.  */
#line 1 "grammar.y"


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
#include "proto.h"


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
XINT	pipe_pc		= 0;	/* pc of last ADDPIPE instruction	*/
int	posit 		= 0;	/* positional argument count		*/
int	inarglist 	= 0;	/* set when in argument list		*/
int	parenlevel 	= 0;	/* level of paren nesting in command	*/
int 	in_iferr 	= 0;	/* in an iferr block			*/
int	cl_level	= 0;	/* CL calling level			*/

int	index_cnt;		/* Index counter in array ref's		*/
char	curr_param[SZ_FNAME];	/* Parameter name of ref's		*/
char	curr_task[SZ_FNAME];	/* ltaskname of command 		*/
XINT	stmt_pc;		/* PC at beginning of current statement */
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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 339 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  15
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   984

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  80
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  134
/* YYNRULES -- Number of rules.  */
#define YYNRULES  272
/* YYNRULES -- Number of states.  */
#define YYNSTATES  444

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   314

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    64,     2,     2,
      78,    79,    62,    60,    77,    61,    68,    63,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    75,    71,
      55,    45,    56,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    73,     2,    74,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    70,    76,    72,    69,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    57,
      58,    59,    65,    66,    67
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,     8,    13,    15,    17,    20,
      21,    22,    27,    29,    32,    34,    38,    39,    45,    46,
      52,    53,    57,    58,    60,    62,    66,    67,    69,    71,
      74,    76,    78,    81,    82,    87,    89,    91,    93,    95,
      97,    99,   101,   103,   105,   107,   109,   113,   115,   121,
     123,   124,   129,   131,   132,   138,   140,   143,   144,   146,
     150,   152,   156,   158,   162,   164,   169,   171,   173,   176,
     178,   180,   184,   186,   188,   190,   194,   198,   201,   203,
     205,   207,   209,   211,   213,   215,   217,   221,   226,   231,
     236,   241,   246,   251,   256,   261,   266,   271,   276,   281,
     286,   291,   296,   299,   302,   303,   309,   310,   318,   319,
     325,   326,   336,   337,   343,   345,   347,   349,   351,   352,
     354,   358,   359,   361,   365,   367,   370,   373,   376,   379,
     382,   385,   387,   389,   391,   393,   395,   397,   399,   401,
     403,   406,   409,   412,   415,   417,   419,   421,   424,   425,
     426,   433,   434,   438,   442,   446,   447,   452,   454,   456,
     458,   460,   462,   464,   465,   469,   470,   471,   476,   479,
     482,   483,   484,   491,   492,   496,   498,   500,   504,   505,
     507,   509,   513,   517,   520,   523,   526,   529,   532,   535,
     538,   541,   543,   545,   548,   551,   554,   556,   558,   560,
     561,   562,   570,   571,   577,   579,   581,   582,   584,   586,
     587,   595,   596,   602,   603,   604,   613,   614,   615,   616,
     633,   635,   636,   638,   639,   640,   651,   652,   653,   661,
     662,   668,   670,   672,   674,   677,   680,   681,   687,   690,
     692,   695,   696,   699,   701,   704,   706,   710,   712,   713,
     715,   717,   718,   724,   726,   727,   732,   734,   736,   738,
     740,   742,   744,   746,   748,   750,   752,   753,   755,   756,
     758,   760,   762
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      81,     0,    -1,    -1,    68,   213,    -1,    -1,    81,    82,
      83,   194,    -1,    86,    -1,    87,    -1,     1,   213,    -1,
      -1,    -1,    85,   207,    84,    83,    -1,    13,    -1,    14,
      16,    -1,    69,    -1,    89,    94,   117,    -1,    -1,   117,
      88,   135,   198,   189,    -1,    -1,    29,    90,   205,    91,
     207,    -1,    -1,   211,    92,   212,    -1,    -1,    93,    -1,
     205,    -1,    93,   208,   205,    -1,    -1,    95,    -1,    96,
      -1,    95,    96,    -1,   207,    -1,    97,    -1,     1,   213,
      -1,    -1,    99,    98,   100,   207,    -1,    32,    -1,    35,
      -1,    34,    -1,    36,    -1,    38,    -1,    39,    -1,    40,
      -1,    41,    -1,    33,    -1,    37,    -1,   101,    -1,   101,
     208,   100,    -1,   102,    -1,   102,    70,   114,    71,    72,
      -1,   104,    -1,    -1,   104,    45,   103,   109,    -1,   106,
      -1,    -1,   106,   105,    73,   107,    74,    -1,   205,    -1,
      62,   205,    -1,    -1,   108,    -1,   107,   208,   108,    -1,
     111,    -1,   111,    75,   111,    -1,   110,    -1,   109,   208,
     110,    -1,   111,    -1,    16,   211,   111,   212,    -1,    16,
      -1,   112,    -1,   113,    16,    -1,    60,    -1,    61,    -1,
     109,   208,   115,    -1,   109,    -1,   115,    -1,   116,    -1,
     115,   208,   116,    -1,    17,    45,   111,    -1,    30,   213,
      -1,   119,    -1,   199,    -1,   120,    -1,    16,    -1,    38,
      -1,    39,    -1,    40,    -1,    41,    -1,   211,   118,   212,
      -1,   118,    60,   198,   118,    -1,   118,    61,   198,   118,
      -1,   118,    62,   198,   118,    -1,   118,    63,   198,   118,
      -1,   118,    67,   198,   118,    -1,   118,    64,   198,   118,
      -1,   118,    59,   198,   118,    -1,   118,    55,   198,   118,
      -1,   118,    56,   198,   118,    -1,   118,    58,   198,   118,
      -1,   118,    57,   198,   118,    -1,   118,    54,   198,   118,
      -1,   118,    53,   198,   118,    -1,   118,    51,   198,   118,
      -1,   118,    52,   198,   118,    -1,    66,   118,    -1,    61,
     118,    -1,    -1,     3,   211,   121,   128,   212,    -1,    -1,
       4,   211,   122,   127,   208,   128,   212,    -1,    -1,     5,
     211,   123,   128,   212,    -1,    -1,     6,   211,    17,   208,
     124,   127,   208,   128,   212,    -1,    -1,   126,   211,   125,
     129,   212,    -1,   204,    -1,    33,    -1,    34,    -1,   118,
      -1,    -1,    17,    -1,    17,   208,   128,    -1,    -1,   118,
      -1,   129,   208,   118,    -1,   131,    -1,   136,   207,    -1,
     140,   207,    -1,   153,   207,    -1,   154,   207,    -1,   155,
     207,    -1,   156,   207,    -1,   165,    -1,   168,    -1,   157,
      -1,   161,    -1,   170,    -1,   173,    -1,   179,    -1,   181,
      -1,   184,    -1,   186,   207,    -1,   187,   207,    -1,   192,
     207,    -1,   188,   207,    -1,   190,    -1,   193,    -1,   132,
      -1,   132,   213,    -1,    -1,    -1,    70,   133,   135,   198,
     134,    72,    -1,    -1,   135,   198,   194,    -1,   199,   138,
     119,    -1,   199,   138,   199,    -1,    -1,   199,   137,   139,
     118,    -1,    45,    -1,    50,    -1,    49,    -1,    48,    -1,
      47,    -1,    46,    -1,    -1,   145,   141,   142,    -1,    -1,
      -1,   142,   144,   143,   145,    -1,    76,   198,    -1,    12,
     198,    -1,    -1,    -1,   206,   146,   209,   147,   148,   210,
      -1,    -1,   208,   149,   150,    -1,   150,    -1,   151,    -1,
     150,   208,   151,    -1,    -1,   119,    -1,   199,    -1,   199,
      45,   119,    -1,   199,    45,   199,    -1,   205,    60,    -1,
     205,    61,    -1,    55,   152,    -1,    56,   152,    -1,    10,
     152,    -1,     8,   152,    -1,     9,   152,    -1,    11,   152,
      -1,   119,    -1,   205,    -1,   138,   119,    -1,   138,   199,
      -1,   199,   138,    -1,     7,    -1,   138,    -1,   158,    -1,
      -1,    -1,   163,   159,   132,   160,   164,   198,   194,    -1,
      -1,   158,    20,   162,   198,   194,    -1,    42,    -1,    43,
      -1,    -1,    44,    -1,   166,    -1,    -1,    19,   211,   118,
     212,   167,   198,   194,    -1,    -1,   166,    20,   169,   198,
     194,    -1,    -1,    -1,    18,   211,   171,   118,   212,   172,
     198,   194,    -1,    -1,    -1,    -1,    21,   211,   198,   177,
      71,   198,   174,   178,    71,   198,   175,   177,   212,   198,
     176,   130,    -1,   136,    -1,    -1,   118,    -1,    -1,    -1,
      24,   198,   211,   198,   118,   198,   212,   198,   180,   194,
      -1,    -1,    -1,    25,   182,   196,    75,   198,   183,   194,
      -1,    -1,    26,    75,   198,   185,   194,    -1,    23,    -1,
      22,    -1,    27,    -1,    27,   118,    -1,    31,   213,    -1,
      -1,    17,    75,   198,   191,   194,    -1,    28,    17,    -1,
      71,    -1,    71,   213,    -1,    -1,   195,   130,    -1,    97,
      -1,     1,   213,    -1,   197,    -1,   197,   208,   196,    -1,
     111,    -1,    -1,   213,    -1,   205,    -1,    -1,   205,   200,
      73,   201,    74,    -1,   203,    -1,    -1,   203,   202,   208,
     201,    -1,   120,    -1,   199,    -1,    62,    -1,    16,    -1,
      17,    -1,    17,    -1,    17,    -1,   213,    -1,    71,    -1,
      77,    -1,    -1,   211,    -1,    -1,   212,    -1,    78,    -1,
      79,    -1,    15,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   144,   144,   169,   181,   181,   201,   212,   224,   265,
     266,   266,   276,   279,   288,   293,   303,   303,   329,   329,
     352,   355,   361,   364,   367,   372,   379,   380,   383,   384,
     387,   388,   389,   426,   426,   463,   464,   465,   466,   467,
     468,   469,   470,   471,   472,   475,   476,   479,   497,   515,
     519,   519,   527,   532,   532,   551,   555,   566,   570,   571,
     575,   598,   615,   616,   619,   627,   648,   649,   655,   669,
     670,   672,   682,   690,   693,   694,   697,   704,   712,   713,
     726,   727,   731,   739,   743,   747,   753,   755,   759,   763,
     767,   771,   775,   784,   788,   792,   796,   800,   804,   808,
     812,   816,   820,   824,   829,   829,   842,   842,   859,   859,
     873,   873,   894,   894,   911,   912,   920,   926,   933,   939,
     945,   953,   956,   960,   969,   970,   971,   972,   973,   974,
     975,   976,   977,   978,   979,   980,   981,   982,   983,   984,
     985,   986,   987,   988,   989,   990,   996,   997,  1000,  1002,
    1000,  1007,  1008,  1014,  1019,  1030,  1030,  1042,  1047,  1048,
    1049,  1050,  1051,  1054,  1054,  1065,  1066,  1066,  1111,  1114,
    1119,  1143,  1119,  1154,  1154,  1165,  1168,  1169,  1173,  1183,
    1191,  1237,  1242,  1254,  1259,  1264,  1268,  1273,  1278,  1283,
    1288,  1294,  1299,  1310,  1315,  1322,  1329,  1335,  1345,  1355,
    1363,  1355,  1380,  1380,  1400,  1401,  1404,  1405,  1412,  1423,
    1423,  1456,  1456,  1479,  1486,  1479,  1524,  1528,  1544,  1524,
    1573,  1574,  1577,  1580,  1606,  1605,  1623,  1631,  1623,  1650,
    1650,  1668,  1683,  1707,  1711,  1723,  1735,  1735,  1769,  1791,
    1792,  1799,  1799,  1845,  1846,  1907,  1908,  1911,  1927,  1928,
    1931,  1968,  1968,  1983,  1986,  1986,  1992,  1996,  2003,  2024,
    2058,  2063,  2068,  2073,  2074,  2082,  2085,  2086,  2089,  2090,
    2096,  2099,  2102
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "Y_SCAN", "Y_SCANF", "Y_FSCAN",
  "Y_FSCANF", "Y_OSESC", "Y_APPEND", "Y_ALLAPPEND", "Y_ALLREDIR",
  "Y_GSREDIR", "Y_ALLPIPE", "D_D", "D_PEEK", "Y_NEWLINE", "Y_CONSTANT",
  "Y_IDENT", "Y_WHILE", "Y_IF", "Y_ELSE", "Y_FOR", "Y_BREAK", "Y_NEXT",
  "Y_SWITCH", "Y_CASE", "Y_DEFAULT", "Y_RETURN", "Y_GOTO", "Y_PROCEDURE",
  "Y_BEGIN", "Y_END", "Y_BOOL", "Y_INT", "Y_REAL", "Y_STRING", "Y_FILE",
  "Y_STRUCT", "Y_GCUR", "Y_IMCUR", "Y_UKEY", "Y_PSET", "Y_IFERR",
  "Y_IFNOERR", "Y_THEN", "'='", "YOP_AOCAT", "YOP_AODIV", "YOP_AOMUL",
  "YOP_AOSUB", "YOP_AOADD", "YOP_OR", "YOP_AND", "YOP_NE", "YOP_EQ", "'<'",
  "'>'", "YOP_GE", "YOP_LE", "YOP_CONCAT", "'+'", "'-'", "'*'", "'/'",
  "'%'", "UMINUS", "YOP_NOT", "YOP_POW", "'.'", "'~'", "'{'", "';'", "'}'",
  "'['", "']'", "':'", "'|'", "','", "'('", "')'", "$accept", "block",
  "@1", "debug", "@2", "D_XXX", "script_params", "script_body", "@3",
  "proc_stmt", "@4", "bparam_list", "param_list", "xparam_list",
  "var_decls", "var_decl_block", "var_decl_line", "var_decl_stmt", "@5",
  "typedefs", "var_decl_list", "var_decl_plus", "var_decl", "@6",
  "var_def", "@7", "var_name", "init_index_list", "init_index_range",
  "init_list", "init_elem", "const", "number", "sign", "options_list",
  "options", "option", "begin_stmt", "expr", "expr0", "expr1", "@8", "@9",
  "@10", "@11", "@12", "intrinsx", "scanfmt", "scanarg", "intrarg", "stmt",
  "c_stmt", "c_blk", "@13", "@14", "s_list", "assign", "@15", "equals",
  "assign_oper", "cmdlist", "@16", "cmdpipe", "@17", "pipe", "command",
  "@18", "@19", "args", "@20", "arglist", "arg", "file", "immed",
  "inspect", "osesc", "popstk", "iferr", "iferr_stat", "@21", "@22",
  "iferr_else", "@23", "iferr_tok", "op_then", "if", "if_stat", "@24",
  "ifelse", "@25", "while", "@26", "@27", "for", "@28", "@29", "@30",
  "xassign", "xexpr", "switch", "@31", "case", "@32", "@33", "default",
  "@34", "next", "break", "return", "end_stmt", "label_stmt", "@35",
  "goto", "nullstmt", "xstmt", "@36", "const_expr_list", "const_expr",
  "opnl", "ref", "@37", "index_list", "@38", "index", "intrins", "param",
  "tasknam", "EOST", "DELIM", "BARG", "EARG", "LP", "RP", "NL", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,    61,   300,   301,   302,   303,
     304,   305,   306,   307,   308,    60,    62,   309,   310,   311,
      43,    45,    42,    47,    37,   312,   313,   314,    46,   126,
     123,    59,   125,    91,    93,    58,   124,    44,    40,    41
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    80,    81,    81,    82,    81,    81,    81,    81,    83,
      84,    83,    85,    85,    85,    86,    88,    87,    90,    89,
      91,    91,    92,    92,    93,    93,    94,    94,    95,    95,
      96,    96,    96,    98,    97,    99,    99,    99,    99,    99,
      99,    99,    99,    99,    99,   100,   100,   101,   101,   102,
     103,   102,   104,   105,   104,   106,   106,   107,   107,   107,
     108,   108,   109,   109,   110,   110,   111,   111,   112,   113,
     113,   114,   114,   114,   115,   115,   116,   117,   118,   118,
     119,   119,   119,   119,   119,   119,   120,   120,   120,   120,
     120,   120,   120,   120,   120,   120,   120,   120,   120,   120,
     120,   120,   120,   120,   121,   120,   122,   120,   123,   120,
     124,   120,   125,   120,   126,   126,   126,   127,   128,   128,
     128,   129,   129,   129,   130,   130,   130,   130,   130,   130,
     130,   130,   130,   130,   130,   130,   130,   130,   130,   130,
     130,   130,   130,   130,   130,   130,   131,   131,   133,   134,
     132,   135,   135,   136,   136,   137,   136,   138,   139,   139,
     139,   139,   139,   141,   140,   142,   143,   142,   144,   144,
     146,   147,   145,   149,   148,   148,   150,   150,   151,   151,
     151,   151,   151,   151,   151,   151,   151,   151,   151,   151,
     151,   152,   152,   153,   153,   154,   155,   156,   157,   159,
     160,   158,   162,   161,   163,   163,   164,   164,   165,   167,
     166,   169,   168,   171,   172,   170,   174,   175,   176,   173,
     177,   177,   178,   178,   180,   179,   182,   183,   181,   185,
     184,   186,   187,   188,   188,   189,   191,   190,   192,   193,
     193,   195,   194,   194,   194,   196,   196,   197,   198,   198,
     199,   200,   199,   201,   202,   201,   203,   203,   203,   203,
     204,   205,   206,   207,   207,   208,   209,   209,   210,   210,
     211,   212,   213
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     0,     4,     1,     1,     2,     0,
       0,     4,     1,     2,     1,     3,     0,     5,     0,     5,
       0,     3,     0,     1,     1,     3,     0,     1,     1,     2,
       1,     1,     2,     0,     4,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     5,     1,
       0,     4,     1,     0,     5,     1,     2,     0,     1,     3,
       1,     3,     1,     3,     1,     4,     1,     1,     2,     1,
       1,     3,     1,     1,     1,     3,     3,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     4,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       4,     4,     2,     2,     0,     5,     0,     7,     0,     5,
       0,     9,     0,     5,     1,     1,     1,     1,     0,     1,
       3,     0,     1,     3,     1,     2,     2,     2,     2,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     2,     2,     1,     1,     1,     2,     0,     0,
       6,     0,     3,     3,     3,     0,     4,     1,     1,     1,
       1,     1,     1,     0,     3,     0,     0,     4,     2,     2,
       0,     0,     6,     0,     3,     1,     1,     3,     0,     1,
       1,     3,     3,     2,     2,     2,     2,     2,     2,     2,
       2,     1,     1,     2,     2,     2,     1,     1,     1,     0,
       0,     7,     0,     5,     1,     1,     0,     1,     1,     0,
       7,     0,     5,     0,     0,     8,     0,     0,     0,    16,
       1,     0,     1,     0,     0,    10,     0,     0,     7,     0,
       5,     1,     1,     1,     2,     2,     0,     5,     2,     1,
       2,     0,     2,     1,     2,     1,     3,     1,     0,     1,
       1,     0,     5,     1,     0,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     0,     1,
       1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     0,    18,     0,     0,     4,     6,     7,     0,    16,
     272,     8,     0,    77,     3,     1,     9,     0,    35,    43,
      37,    36,    38,    44,    39,    40,    41,    42,   264,     0,
       0,    28,    31,    33,    30,   263,   151,   261,    20,    12,
       0,    14,     0,     0,    32,    15,    29,     0,   248,   270,
       0,    22,    13,     0,   243,     5,     0,    10,     0,     0,
      45,    47,    49,    52,    55,     0,   249,    19,     0,    23,
      24,   244,   196,   262,     0,     0,     0,   232,   231,   248,
     226,     0,   233,     0,   204,   205,   157,   148,   239,   242,
     124,   146,     0,   197,     0,   163,     0,     0,     0,     0,
     133,   198,   134,   199,   131,   208,   132,   135,   136,   137,
     138,   139,     0,     0,     0,   144,     0,   145,   155,   250,
     170,     9,    56,    34,   265,     0,     0,    50,     0,     0,
      17,   152,   271,    21,     0,   248,   213,     0,   248,     0,
       0,   248,     0,     0,     0,     0,    81,   261,   115,   116,
      82,    83,    84,    85,     0,     0,   234,    78,    80,     0,
      79,   114,     0,   238,   151,   240,   147,   125,     0,    78,
      79,   126,   165,   127,   128,   129,   130,   202,     0,   211,
     140,   141,   143,   142,     0,   195,     0,   266,    11,    46,
      66,     0,    69,    70,    72,    62,    64,    67,     0,     0,
      73,    74,     0,    57,   235,    25,   236,     0,     0,   221,
     248,    66,   247,     0,   245,   229,   104,   106,   108,     0,
     103,   102,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   248,   112,     0,   248,
     164,   248,   200,   248,   162,   161,   160,   159,   158,     0,
      78,    79,     0,   171,   267,     0,     0,     0,    68,     0,
       0,    51,     0,    58,    60,     0,     0,   209,   220,     0,
     155,     0,   248,     0,     0,   118,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   121,    86,     0,   248,   248,   166,
       0,   206,     0,   156,    81,   258,    80,    79,     0,   253,
     178,     0,    76,    63,    71,    48,    75,     0,    54,     0,
       0,   237,   214,   248,   248,     0,   248,   227,   246,   230,
     119,     0,   117,     0,     0,   110,   100,   101,    99,    98,
      94,    95,    97,    96,    93,    87,    88,    89,    90,    92,
      91,   122,     0,     0,   169,   168,     0,   203,   207,   248,
     212,   252,     0,     0,     0,     0,     0,     0,     0,    78,
     268,   175,   176,    79,   250,   173,    65,    59,    61,   248,
       0,   216,     0,     0,   118,   105,   118,   109,     0,     0,
     113,   150,   262,   167,     0,     0,    78,   188,   250,   189,
     187,   190,   185,   186,   172,   269,   178,     0,   183,   184,
     178,     0,   210,   223,   248,   228,   120,     0,     0,   123,
     201,   255,   177,    78,    79,   174,   215,   222,     0,   224,
     107,   118,   248,     0,     0,   217,   225,   111,   221,     0,
     248,   218,     0,   219
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     5,    16,    42,   121,    43,     6,     7,    36,     8,
      12,    50,    68,    69,    29,    30,    31,    54,    47,    33,
      59,    60,    61,   202,    62,   128,    63,   262,   263,   194,
     195,   196,   197,   198,   199,   200,   201,     9,   168,   157,
     158,   275,   276,   277,   388,   294,   159,   333,   331,   352,
      89,    90,    91,   164,   353,    48,    92,   184,    93,   249,
      94,   172,   240,   356,   299,    95,   187,   310,   370,   410,
     371,   372,   397,    96,    97,    98,    99,   100,   101,   178,
     301,   102,   241,   103,   359,   104,   105,   323,   106,   243,
     107,   207,   379,   108,   413,   438,   442,   269,   428,   109,
     433,   110,   140,   383,   111,   274,   112,   113,   114,   130,
     115,   265,   116,   117,   131,    56,   213,   214,    65,   160,
     186,   308,   362,   309,   161,   119,   120,    34,   260,   253,
     404,   162,   133,    66
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -256
static const yytype_int16 yypact[] =
{
     576,    -8,  -256,    -8,    -8,    24,  -256,  -256,   785,  -256,
    -256,  -256,    11,  -256,  -256,  -256,    13,    -8,  -256,  -256,
    -256,  -256,  -256,  -256,  -256,  -256,  -256,  -256,  -256,   -17,
     801,  -256,  -256,  -256,  -256,  -256,  -256,  -256,   -30,  -256,
      37,  -256,   742,    21,  -256,  -256,  -256,    42,    -8,  -256,
      21,    11,  -256,    -8,  -256,  -256,   836,  -256,    11,    21,
       6,    36,    52,    18,  -256,   687,  -256,  -256,    31,     6,
    -256,  -256,  -256,   236,   -30,   -30,   -30,  -256,  -256,    -8,
    -256,    39,   409,   104,  -256,  -256,  -256,  -256,    -8,  -256,
    -256,    -8,    21,   409,    21,  -256,    21,    21,    21,    21,
    -256,   106,  -256,  -256,  -256,   112,  -256,  -256,  -256,  -256,
    -256,  -256,    21,    21,    21,  -256,    21,  -256,    88,    65,
    -256,    13,  -256,  -256,  -256,    42,     0,  -256,    78,    -8,
    -256,  -256,  -256,  -256,    11,    -8,  -256,   409,    -8,   -30,
       4,    -8,   -30,   -30,   -30,   -30,  -256,    66,  -256,  -256,
    -256,  -256,  -256,  -256,   409,   409,   886,  -256,  -256,   -30,
    -256,  -256,   409,  -256,  -256,  -256,  -256,  -256,   886,    27,
      28,  -256,  -256,  -256,  -256,  -256,  -256,  -256,    82,  -256,
    -256,  -256,  -256,  -256,   220,   409,    80,   -30,  -256,  -256,
     -30,   109,  -256,  -256,     6,  -256,  -256,  -256,   141,   107,
       6,  -256,    79,     4,  -256,  -256,  -256,   409,   857,    11,
      -8,  -256,  -256,   110,     6,  -256,  -256,  -256,  -256,   170,
     121,   121,    -8,    -8,    -8,    -8,    -8,    -8,    -8,    -8,
      -8,    -8,    -8,    -8,    -8,    -8,    -8,  -256,   857,    -8,
       2,    -8,  -256,    -8,  -256,  -256,  -256,  -256,  -256,   409,
      -3,    10,   513,  -256,  -256,     4,     4,     0,  -256,   117,
     173,     6,   -44,  -256,   120,   742,   857,  -256,  -256,   128,
      88,   409,    -8,     4,   742,   183,   409,   183,     6,   409,
     409,   409,   409,   409,   409,   409,   409,   409,   409,   409,
     409,   409,   409,   409,   409,  -256,   631,    -8,    -8,  -256,
     742,   157,   742,   886,   -40,  -256,    60,    71,   130,   138,
      46,    31,  -256,  -256,     6,  -256,  -256,    79,  -256,     4,
       4,  -256,  -256,    -8,    -8,   409,   831,  -256,  -256,  -256,
       6,    31,   886,     6,    31,  -256,   902,   917,   481,   481,
     421,   421,   421,   421,   160,    53,    53,   121,   121,   121,
    -256,   886,   -39,   135,  -256,  -256,   208,  -256,  -256,    -8,
    -256,  -256,     6,   409,   409,   409,   409,   409,   409,    32,
      31,     6,  -256,    -4,   -42,  -256,  -256,  -256,  -256,    -8,
     742,  -256,    31,   742,   183,  -256,   183,  -256,   409,   409,
    -256,  -256,  -256,  -256,   742,   513,   115,  -256,    17,  -256,
    -256,  -256,  -256,  -256,  -256,  -256,   494,   409,  -256,  -256,
     494,   742,  -256,   409,    -8,  -256,  -256,    31,     6,   886,
    -256,  -256,  -256,   169,   201,     6,  -256,   886,   147,  -256,
    -256,   183,    -8,   742,    31,  -256,  -256,  -256,    11,    31,
      -8,  -256,   836,  -256
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -256,  -256,  -256,   108,  -256,  -256,  -256,  -256,  -256,  -256,
    -256,  -256,  -256,  -256,  -256,  -256,   200,    15,  -256,  -256,
     111,  -256,  -256,  -256,  -256,  -256,  -256,  -256,   -84,    40,
    -248,  -137,  -256,  -256,  -256,   -19,   -13,   212,    43,    54,
    -246,  -256,  -256,  -256,  -256,  -256,  -256,  -145,  -255,  -256,
    -193,  -256,    74,  -256,  -256,    89,  -207,  -256,  -114,  -256,
    -256,  -256,  -256,  -256,  -256,  -101,  -256,  -256,  -256,  -256,
    -154,  -148,   -60,  -256,  -256,  -256,  -256,  -256,  -256,  -256,
    -256,  -256,  -256,  -256,  -256,  -256,  -256,  -256,  -256,  -256,
    -256,  -256,  -256,  -256,  -256,  -256,  -256,  -177,  -256,  -256,
    -256,  -256,  -256,  -256,  -256,  -256,  -256,  -256,  -256,  -256,
    -256,  -256,  -256,  -256,   -37,  -256,    -9,  -256,   -64,   -35,
    -256,  -122,  -256,  -256,  -256,   -12,  -256,   472,   -59,  -256,
    -256,    67,  -138,   380
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -262
static const yytype_int16 yytable[] =
{
      38,   125,   268,   212,   185,    55,   306,    10,  -180,   313,
     134,  -180,  -153,     3,   297,   139,   190,   191,   408,   409,
     211,   118,   334,    32,    15,  -154,    39,    40,    37,  -192,
     318,  -251,  -192,   124,  -259,    64,    10,  -259,   124,    70,
     132,   407,  -193,  -194,  -179,    32,   122,  -179,    49,   142,
     143,   144,   145,    52,   363,   364,   365,   366,   170,    37,
     192,   193,   146,   147,   192,   193,   264,  -180,  -153,   313,
     267,   206,  -180,  -180,   209,  -180,  -153,   215,   298,   148,
     149,  -154,    41,   124,   150,   151,   152,   153,  -192,  -154,
    -251,   -53,    28,  -192,  -192,   190,  -192,   127,  -193,  -194,
     295,   367,   368,  -179,    58,    51,   126,   154,  -179,  -179,
     132,  -179,   155,    64,   141,   233,   234,   235,   311,   312,
     236,   163,   205,   124,    49,   156,   177,  -191,   322,   416,
    -191,   417,   179,    86,  -256,   257,   212,  -256,  -251,   192,
     193,   136,   137,   138,  -260,  -257,   271,   169,  -257,   306,
     251,   203,    87,   252,   256,   273,   325,   258,   279,   280,
     281,   282,   283,   284,   285,   286,   287,   288,   289,   290,
     291,   292,   293,   376,   270,   296,   434,   300,   259,   302,
     208,  -181,   264,   378,  -181,   272,  -191,   278,   236,   315,
     191,  -191,  -191,   385,  -191,   320,   387,   220,   221,   324,
     330,   358,   317,   319,   361,   238,   210,   391,   327,   216,
     217,   218,   219,  -182,   390,  -254,  -182,   307,   432,   335,
     231,   232,   233,   234,   235,   392,   237,   236,   321,   188,
      46,   268,   405,   354,   355,   377,   189,   329,   314,   250,
    -181,    45,   261,   418,   414,  -181,  -181,   316,  -181,   443,
     266,   375,   242,   239,   254,   393,   425,   255,   422,   380,
     381,   439,   382,   357,   328,   360,   244,   245,   246,   247,
     248,   384,  -182,   421,   386,   373,     0,  -182,  -182,   430,
    -182,  -261,  -261,  -261,  -261,  -261,  -261,     0,     0,     0,
     251,     0,   303,   389,     0,   394,   437,     0,   374,     0,
       0,   440,     0,   395,   399,   400,   401,   402,   403,  -261,
       0,   135,   406,     0,   326,   411,     0,     0,     0,   332,
       0,     0,   336,   337,   338,   339,   340,   341,   342,   343,
     344,   345,   346,   347,   348,   349,   350,   351,     0,     0,
       0,     0,     0,   412,     0,     0,   415,     0,     0,     0,
     429,   398,   398,   398,   398,   398,   398,   420,     0,   431,
     307,     0,     0,     0,   369,     0,   406,     0,   435,     0,
       0,   373,   424,     0,   426,   373,   441,     0,     0,   250,
       0,    11,     0,    13,    14,     0,     0,     0,    35,     0,
       0,     0,     0,     0,   374,     0,   436,    44,   374,     0,
       0,     0,     0,   270,     0,     0,     0,   118,     0,     0,
      35,     0,   142,   143,   144,   145,     0,   396,   396,   396,
     396,   396,   396,    35,     0,   146,   147,     0,     0,     0,
      35,   332,   419,    71,     0,     0,     0,     0,     0,    35,
       0,     0,   148,   149,     0,     0,     0,   150,   151,   152,
     153,     0,     0,     0,     0,     0,   427,     0,     0,     0,
     369,   423,     0,     0,   369,     0,     0,     0,   165,     0,
     154,   166,    35,     0,    35,   155,    35,    35,    35,    35,
     230,   231,   232,   233,   234,   235,     0,    49,   236,     0,
       0,     0,    35,    35,    35,     0,    35,   142,   143,   144,
     145,     0,   363,   364,   365,   366,     0,     0,     0,   204,
     146,   147,     0,     0,     0,    57,   142,   143,   144,   145,
       0,     0,    67,     0,     0,     0,     0,   148,   149,   304,
     147,   123,   150,   151,   152,   153,   226,   227,   228,   229,
     230,   231,   232,   233,   234,   235,   148,   149,   236,   367,
     368,   150,   151,   152,   153,   154,     0,     0,     0,     0,
     155,     0,     0,     0,   167,     0,   171,     0,   173,   174,
     175,   176,    49,     0,   154,   305,    -2,     1,     0,   155,
       0,     0,     0,    -2,   180,   181,   182,     0,   183,    -2,
      -2,    49,     0,    -2,    -2,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     2,     3,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    53,     0,     0,     0,     0,     0,  -241,     0,
       0,     0,     0,     0,     4,    -2,    -2,    -2,  -241,  -241,
    -241,     0,  -241,  -241,  -241,  -241,  -241,  -241,  -241,  -241,
       0,     0,     0,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,  -241,  -241,     0,  -241,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    53,     0,
       0,     0,     0,     0,  -241,     0,     0,     0,     0,     0,
       0,  -241,  -241,  -149,  -241,  -241,  -241,     0,  -241,  -241,
    -241,  -241,  -241,  -241,  -241,  -241,     0,     0,   129,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,  -241,
    -241,     0,  -241,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    53,     0,     0,     0,     0,     0,  -241,
       0,     0,     0,     0,     0,     0,     0,  -241,  -241,  -241,
    -241,  -241,     0,  -241,  -241,  -241,  -241,  -241,  -241,  -241,
    -241,     0,     0,     0,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,  -241,  -241,    17,  -241,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      10,     0,    17,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -241,  -241,     0,   -26,    10,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,     0,     0,     0,
       0,   -27,     0,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    72,     0,     0,    10,     0,     0,     0,
       0,     0,     0,    73,    74,    75,    28,    76,    77,    78,
      79,    80,    81,    82,    83,     0,     0,     0,     0,     0,
       0,     0,    28,     0,     0,     0,     0,     0,    84,    85,
       0,    86,   222,   223,   224,   225,   226,   227,   228,   229,
     230,   231,   232,   233,   234,   235,     0,     0,   236,     0,
       0,     0,     0,     0,     0,     0,    87,    88,   222,   223,
     224,   225,   226,   227,   228,   229,   230,   231,   232,   233,
     234,   235,     0,     0,   236,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   132,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,     0,     0,   236,   223,   224,   225,   226,   227,   228,
     229,   230,   231,   232,   233,   234,   235,     0,     0,   236,
     224,   225,   226,   227,   228,   229,   230,   231,   232,   233,
     234,   235,     0,     0,   236
};

static const yytype_int16 yycheck[] =
{
      12,    60,   209,   140,   118,    42,   252,    15,    12,   257,
      69,    15,    15,    30,    12,    79,    16,    17,    60,    61,
      16,    56,   277,     8,     0,    15,    13,    14,    17,    12,
      74,    73,    15,    77,    74,    47,    15,    77,    77,    51,
      79,    45,    15,    15,    12,    30,    58,    15,    78,     3,
       4,     5,     6,    16,     8,     9,    10,    11,    93,    17,
      60,    61,    16,    17,    60,    61,   203,    71,    71,   317,
     208,   135,    76,    77,   138,    79,    79,   141,    76,    33,
      34,    71,    69,    77,    38,    39,    40,    41,    71,    79,
      73,    73,    71,    76,    77,    16,    79,    45,    71,    71,
     238,    55,    56,    71,    62,    38,    70,    61,    76,    77,
      79,    79,    66,   125,    75,    62,    63,    64,   255,   256,
      67,    17,   134,    77,    78,    82,    20,    12,   266,   384,
      15,   386,    20,    45,    74,   194,   273,    77,    73,    60,
      61,    74,    75,    76,    78,    74,   210,    93,    77,   395,
     185,    73,    70,    73,    45,   214,   270,    16,   222,   223,
     224,   225,   226,   227,   228,   229,   230,   231,   232,   233,
     234,   235,   236,   311,   209,   239,   431,   241,    71,   243,
     137,    12,   319,   320,    15,    75,    71,    17,    67,    72,
      17,    76,    77,   331,    79,    75,   334,   154,   155,    71,
      17,    44,   261,   262,    74,   162,   139,    72,   272,   142,
     143,   144,   145,    12,   352,    77,    15,   252,    71,   278,
      60,    61,    62,    63,    64,    17,   159,    67,   265,   121,
      30,   438,   370,   297,   298,   319,   125,   274,   257,   185,
      71,    29,   202,   388,   382,    76,    77,   260,    79,   442,
     207,   310,   178,   164,   187,   356,   410,   190,   406,   323,
     324,   438,   326,   300,   273,   302,    46,    47,    48,    49,
      50,   330,    71,   395,   333,   310,    -1,    76,    77,   417,
      79,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
     325,    -1,   249,   352,    -1,   359,   434,    -1,   310,    -1,
      -1,   439,    -1,   362,   364,   365,   366,   367,   368,    73,
      -1,    75,   371,    -1,   271,   379,    -1,    -1,    -1,   276,
      -1,    -1,   279,   280,   281,   282,   283,   284,   285,   286,
     287,   288,   289,   290,   291,   292,   293,   294,    -1,    -1,
      -1,    -1,    -1,   380,    -1,    -1,   383,    -1,    -1,    -1,
     414,   363,   364,   365,   366,   367,   368,   394,    -1,   418,
     395,    -1,    -1,    -1,   310,    -1,   425,    -1,   432,    -1,
      -1,   406,   407,    -1,   411,   410,   440,    -1,    -1,   325,
      -1,     1,    -1,     3,     4,    -1,    -1,    -1,     8,    -1,
      -1,    -1,    -1,    -1,   406,    -1,   433,    17,   410,    -1,
      -1,    -1,    -1,   438,    -1,    -1,    -1,   442,    -1,    -1,
      30,    -1,     3,     4,     5,     6,    -1,   363,   364,   365,
     366,   367,   368,    43,    -1,    16,    17,    -1,    -1,    -1,
      50,   388,   389,    53,    -1,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    33,    34,    -1,    -1,    -1,    38,    39,    40,
      41,    -1,    -1,    -1,    -1,    -1,   413,    -1,    -1,    -1,
     406,   407,    -1,    -1,   410,    -1,    -1,    -1,    88,    -1,
      61,    91,    92,    -1,    94,    66,    96,    97,    98,    99,
      59,    60,    61,    62,    63,    64,    -1,    78,    67,    -1,
      -1,    -1,   112,   113,   114,    -1,   116,     3,     4,     5,
       6,    -1,     8,     9,    10,    11,    -1,    -1,    -1,   129,
      16,    17,    -1,    -1,    -1,    43,     3,     4,     5,     6,
      -1,    -1,    50,    -1,    -1,    -1,    -1,    33,    34,    16,
      17,    59,    38,    39,    40,    41,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    33,    34,    67,    55,
      56,    38,    39,    40,    41,    61,    -1,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    92,    -1,    94,    -1,    96,    97,
      98,    99,    78,    -1,    61,    62,     0,     1,    -1,    66,
      -1,    -1,    -1,     7,   112,   113,   114,    -1,   116,    13,
      14,    78,    -1,    17,    18,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,     7,    -1,
      -1,    -1,    -1,    -1,    68,    69,    70,    71,    17,    18,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      -1,    -1,    -1,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    -1,    45,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
      -1,    -1,    -1,    -1,     7,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    17,    18,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    -1,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,     7,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    17,
      18,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    -1,    -1,    -1,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,     1,    45,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    -1,    30,    15,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    -1,    -1,    -1,
      -1,    30,    -1,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,     7,    -1,    -1,    15,    -1,    -1,    -1,
      -1,    -1,    -1,    17,    18,    19,    71,    21,    22,    23,
      24,    25,    26,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    42,    43,
      -1,    45,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    67,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    67,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    67,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    67
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,    29,    30,    68,    81,    86,    87,    89,   117,
      15,   213,    90,   213,   213,     0,    82,     1,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    71,    94,
      95,    96,    97,    99,   207,   213,    88,    17,   205,    13,
      14,    69,    83,    85,   213,   117,    96,    98,   135,    78,
      91,   211,    16,     1,    97,   194,   195,   207,    62,   100,
     101,   102,   104,   106,   205,   198,   213,   207,    92,    93,
     205,   213,     7,    17,    18,    19,    21,    22,    23,    24,
      25,    26,    27,    28,    42,    43,    45,    70,    71,   130,
     131,   132,   136,   138,   140,   145,   153,   154,   155,   156,
     157,   158,   161,   163,   165,   166,   168,   170,   173,   179,
     181,   184,   186,   187,   188,   190,   192,   193,   199,   205,
     206,    84,   205,   207,    77,   208,    70,    45,   105,    31,
     189,   194,    79,   212,   208,    75,   211,   211,   211,   198,
     182,    75,     3,     4,     5,     6,    16,    17,    33,    34,
      38,    39,    40,    41,    61,    66,   118,   119,   120,   126,
     199,   204,   211,    17,   133,   213,   213,   207,   118,   119,
     199,   207,   141,   207,   207,   207,   207,    20,   159,    20,
     207,   207,   207,   207,   137,   138,   200,   146,    83,   100,
      16,    17,    60,    61,   109,   110,   111,   112,   113,   114,
     115,   116,   103,    73,   213,   205,   198,   171,   118,   198,
     211,    16,   111,   196,   197,   198,   211,   211,   211,   211,
     118,   118,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    67,   211,   118,   135,
     142,   162,   132,   169,    46,    47,    48,    49,    50,   139,
     119,   199,    73,   209,   211,   211,    45,   208,    16,    71,
     208,   109,   107,   108,   111,   191,   118,   212,   136,   177,
     199,   198,    75,   208,   185,   121,   122,   123,    17,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   125,   212,   198,    12,    76,   144,
     198,   160,   198,   118,    16,    62,   120,   199,   201,   203,
     147,   111,   111,   110,   115,    72,   116,   208,    74,   208,
      75,   194,   212,   167,    71,   138,   118,   198,   196,   194,
      17,   128,   118,   127,   128,   208,   118,   118,   118,   118,
     118,   118,   118,   118,   118,   118,   118,   118,   118,   118,
     118,   118,   129,   134,   198,   198,   143,   194,    44,   164,
     194,    74,   202,     8,     9,    10,    11,    55,    56,   119,
     148,   150,   151,   199,   205,   208,   212,   108,   111,   172,
     198,   198,   198,   183,   208,   212,   208,   212,   124,   208,
     212,    72,    17,   145,   198,   208,   119,   152,   205,   152,
     152,   152,   152,   152,   210,   212,   208,    45,    60,    61,
     149,   198,   194,   174,   212,   194,   128,   128,   127,   118,
     194,   201,   151,   119,   199,   150,   194,   118,   178,   198,
     212,   208,    71,   180,   128,   198,   194,   212,   175,   177,
     212,   198,   176,   130
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 144 "grammar.y"
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
		}
    break;

  case 3:
#line 169 "grammar.y"
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
		}
    break;

  case 4:
#line 181 "grammar.y"
    {
		    if (parse_state == PARSE_PARAMS) {
			errmsg = "Illegal parser state.";
			EYYERROR;
		    }
		}
    break;

  case 5:
#line 187 "grammar.y"
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
		}
    break;

  case 6:
#line 201 "grammar.y"
    {
		    /* Parse the parameters in a script file.  This will
		     * normally be done on a call by pfileread().
		     */
		    if (parse_state != PARSE_PARAMS) {
			eprintf ("Illegal parser state.\n");
			errcnt++;
		    }
		    YYACCEPT;
		}
    break;

  case 7:
#line 212 "grammar.y"
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
		}
    break;

  case 8:
#line 224 "grammar.y"
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
		}
    break;

  case 10:
#line 266 "grammar.y"
    {
		    /* debug are those debugging functions that
		     * should be run directly and not through a
		     * builtin task due to stack or other changes,
		     * ie, don't change what we are trying to show.
		     */
		    printf ("\n");
		}
    break;

  case 12:
#line 276 "grammar.y"
    {
		    d_d(); /* show dictionary/stack pointers */
		}
    break;

  case 13:
#line 279 "grammar.y"
    { /* show a dictionary location	*/
		    if (stkop((yyvsp[(2) - (2)]))->o_type & OT_INT) {
			int	idx;
			idx = stkop((yyvsp[(2) - (2)]))->o_val.v_i;
			eprintf ("%d:\t%d (0%o)\n", idx, stack[idx],
				stack[idx]);
		    } else
			eprintf ("usage: D_PEEK <d. index>\n");
		}
    break;

  case 14:
#line 288 "grammar.y"
    {
		    d_stack (pc, 0, 0);		/* show compiled code	*/
		}
    break;

  case 15:
#line 295 "grammar.y"
    { 
			/* Check for required params.
			 */
			if (!errcnt)
			    proc_params(n_procpar);
		}
    break;

  case 16:
#line 303 "grammar.y"
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
		}
    break;

  case 18:
#line 329 "grammar.y"
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
		}
    break;

  case 20:
#line 352 "grammar.y"
    {
		    n_procpar = 0;
		}
    break;

  case 22:
#line 361 "grammar.y"
    {
		    n_procpar = 0;
		}
    break;

  case 24:
#line 367 "grammar.y"
    { 
		    n_procpar = 1;
		    if (!errcnt)
			push (stkop((yyvsp[(1) - (1)])));
		}
    break;

  case 25:
#line 372 "grammar.y"
    {
		    n_procpar++;
		    if (!errcnt)
			push (stkop((yyvsp[(3) - (3)])));
		}
    break;

  case 32:
#line 389 "grammar.y"
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
		}
    break;

  case 33:
#line 426 "grammar.y"
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

		}
    break;

  case 34:
#line 447 "grammar.y"
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
		}
    break;

  case 35:
#line 463 "grammar.y"
    { vartype = V_BOOL; }
    break;

  case 36:
#line 464 "grammar.y"
    { vartype = V_STRING; }
    break;

  case 37:
#line 465 "grammar.y"
    { vartype = V_REAL; }
    break;

  case 38:
#line 466 "grammar.y"
    { vartype = V_FILE; }
    break;

  case 39:
#line 467 "grammar.y"
    { vartype = V_GCUR; }
    break;

  case 40:
#line 468 "grammar.y"
    { vartype = V_IMCUR; }
    break;

  case 41:
#line 469 "grammar.y"
    { vartype = V_UKEY; }
    break;

  case 42:
#line 470 "grammar.y"
    { vartype = V_PSET; }
    break;

  case 43:
#line 471 "grammar.y"
    { vartype = V_INT; }
    break;

  case 44:
#line 472 "grammar.y"
    { vartype = V_STRUCT; }
    break;

  case 47:
#line 479 "grammar.y"
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
		}
    break;

  case 48:
#line 497 "grammar.y"
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
		}
    break;

  case 49:
#line 515 "grammar.y"
    {
			inited = NO;
			n_aval = 0;
		}
    break;

  case 50:
#line 519 "grammar.y"
    {
			n_aval = 0;
		}
    break;

  case 51:
#line 522 "grammar.y"
    {
			inited = YES;
		}
    break;

  case 52:
#line 527 "grammar.y"
    {
		    index_cnt = 0;
		    if (!errcnt)
			pp = initparam (stkop((yyvsp[(1) - (1)])), do_params, vartype, varlist);
		}
    break;

  case 53:
#line 532 "grammar.y"
    {
		    int  itemp;

		    if (!errcnt) {
			pp = initparam (stkop((yyvsp[(1) - (1)])), do_params, vartype, varlist);

			if (pp != NULL) {
			    itemp = (pp->p_type & OT_BASIC) == pp->p_type;
			    itemp = itemp && !varlist;
			    if (itemp)
				pp->p_type |= PT_ARRAY;
			    else
				cl_error (E_UERR, inval_arr, pp->p_name);
			}
		    }
		}
    break;

  case 55:
#line 551 "grammar.y"
    {
			varlist = NO;
			index_cnt = 0;
		}
    break;

  case 56:
#line 555 "grammar.y"
    {
			if (!do_params) {
			    errmsg = locallist;
			    EYYERROR;
			}
			varlist = YES;
			index_cnt = 0;
			(yyval) = (yyvsp[(2) - (2)]);
		}
    break;

  case 60:
#line 575 "grammar.y"
    {
		    if (!errcnt) {
			if (pp != NULL) {
			    if (stkop((yyvsp[(1) - (1)]))->o_type == OT_INT) {
				push (stkop((yyvsp[(1) - (1)]))->o_val.v_i);
				push (1);
			    } else if (maybeindex) {
				/* Confusion between sexagesimal and index
				 * range.  Maybeindex is set only when operand
				 * is real.
			 	 */
				int  i1,i2;
				sexa_to_index (stkop((yyvsp[(1) - (1)]))->o_val.v_r, &i1, &i2);
				push (i2-i1+1);
				push (i1);
			    } else {
				eprintf (inv_index, pp->p_name);
				EYYERROR;
			    }
			    index_cnt++;
			}
		    }
		}
    break;

  case 61:
#line 598 "grammar.y"
    {
			if (!errcnt) {
			    if (pp != NULL) {
				if (stkop((yyvsp[(1) - (3)]))->o_type != OT_INT  ||
				    stkop((yyvsp[(3) - (3)]))->o_type != OT_INT)
				    cl_error (E_UERR, inv_index, pp->p_name);
				else {
				    push (stkop((yyvsp[(3) - (3)]))->o_val.v_i -
				          stkop((yyvsp[(1) - (3)]))->o_val.v_i + 1);
				    push (stkop((yyvsp[(1) - (3)]))->o_val.v_i);
			        }
			        index_cnt++;
			    }
			}
		}
    break;

  case 64:
#line 619 "grammar.y"
    {
			if (!errcnt) {
			    if (pp != NULL) {
				push (stkop((yyvsp[(1) - (1)])) );
				n_aval++;
			    }
			}
		}
    break;

  case 65:
#line 628 "grammar.y"
    {
			int   cnt;
			
			if (!errcnt)
			    if (pp != NULL) {
			    	if (stkop((yyvsp[(1) - (4)]))->o_type != OT_INT)
				    cl_error (E_UERR, arrdeferr, pp->p_name);

			        cnt = stkop((yyvsp[(1) - (4)]))->o_val.v_i;
			        if (cnt <= 0)
				    cl_error (E_UERR, arrdeferr, pp->p_name);

			        while (cnt-- > 0) {
				    push (stkop((yyvsp[(3) - (4)])));
				    n_aval++;
			        }
			    }
		}
    break;

  case 68:
#line 655 "grammar.y"
    {
		      	if (stkop((yyvsp[(2) - (2)]))->o_type == OT_INT) {
			    stkop((yyvsp[(2) - (2)]))->o_val.v_i *= (yyvsp[(1) - (2)]);
			    (yyval) = (yyvsp[(2) - (2)]);
			} else if (stkop((yyvsp[(2) - (2)]))->o_type == OT_REAL) {
			    stkop((yyvsp[(2) - (2)]))->o_val.v_r *= (yyvsp[(1) - (2)]);
			    (yyval) = (yyvsp[(2) - (2)]);
			} else {
			    errmsg = "Invalid constant in declaration.";
			    EYYERROR;
			}
		}
    break;

  case 69:
#line 669 "grammar.y"
    { (yyval) =  1; }
    break;

  case 70:
#line 670 "grammar.y"
    { (yyval) = -1; }
    break;

  case 71:
#line 672 "grammar.y"
    {
			/* Check if we already had an initialization. 
			 */
		      	if (!errcnt) {
			    if (inited && pp != NULL) {
				eprintf (twoinits, pp->p_name);
				EYYERROR;
			    }
			}
		}
    break;

  case 72:
#line 682 "grammar.y"
    {
		      	if (!errcnt) {
			    if (inited && pp != NULL) {
			        eprintf (twoinits, pp->p_name);
				EYYERROR;
			    }
			}
		}
    break;

  case 76:
#line 697 "grammar.y"
    {
			if (!errcnt)
			    if (pp != NULL)
			    	do_option (pp, stkop((yyvsp[(1) - (3)])), stkop((yyvsp[(3) - (3)])));
		}
    break;

  case 79:
#line 713 "grammar.y"
    {
		    if (!errcnt)
		        compile (PUSHPARAM, stkop((yyvsp[(1) - (1)]))->o_val.v_s);
		}
    break;

  case 81:
#line 727 "grammar.y"
    {
		    if  (!errcnt)
		        compile (PUSHCONST, stkop((yyvsp[(1) - (1)])));
		}
    break;

  case 82:
#line 731 "grammar.y"
    {
		    /* "gcur" is both a keyword and a CL global parameter,
		     * and must be built into the grammar here to permit
		     * reference of the parameter in expressions.
		     */
		    if (!errcnt)
			compile (PUSHPARAM, "gcur");
		}
    break;

  case 83:
#line 739 "grammar.y"
    {
		    if (!errcnt)
			compile (PUSHPARAM, "imcur");
		}
    break;

  case 84:
#line 743 "grammar.y"
    {
		    if (!errcnt)
			compile (PUSHPARAM, "ukey");
		}
    break;

  case 85:
#line 747 "grammar.y"
    {
		    if (!errcnt)
			compile (PUSHPARAM, "pset");
		}
    break;

  case 87:
#line 755 "grammar.y"
    {
		    if (!errcnt)
			compile (ADD);
		}
    break;

  case 88:
#line 759 "grammar.y"
    {
		    if (!errcnt)
			compile (SUB);
		}
    break;

  case 89:
#line 763 "grammar.y"
    {
		    if (!errcnt)
			compile (MUL);
		}
    break;

  case 90:
#line 767 "grammar.y"
    {
		    if (!errcnt)
			compile (DIV);
		}
    break;

  case 91:
#line 771 "grammar.y"
    {
		    if (!errcnt)
			compile (POW);
		}
    break;

  case 92:
#line 775 "grammar.y"
    {
		    struct	operand o;
		    if (!errcnt) {
			o.o_type = OT_INT;
			o.o_val.v_i = 2;
			compile (PUSHCONST, &o);
			compile (INTRINSIC, "mod");
		    }
		}
    break;

  case 93:
#line 784 "grammar.y"
    {
		    if (!errcnt)
			compile (CONCAT);
		}
    break;

  case 94:
#line 788 "grammar.y"
    {
		    if (!errcnt)
			compile (LT);
		}
    break;

  case 95:
#line 792 "grammar.y"
    {
		    if (!errcnt)
			compile (GT);
		}
    break;

  case 96:
#line 796 "grammar.y"
    {
		    if (!errcnt)
			compile (LE);
		}
    break;

  case 97:
#line 800 "grammar.y"
    {
		    if (!errcnt)
			compile (GE);
		}
    break;

  case 98:
#line 804 "grammar.y"
    {
		    if (!errcnt)
			compile (EQ);
		}
    break;

  case 99:
#line 808 "grammar.y"
    {
		    if (!errcnt)
			compile (NE);
		}
    break;

  case 100:
#line 812 "grammar.y"
    {
		    if (!errcnt)
			compile (OR);
		}
    break;

  case 101:
#line 816 "grammar.y"
    {
		    if (!errcnt)
			compile (AND);
		}
    break;

  case 102:
#line 820 "grammar.y"
    {
		    if (!errcnt)
			compile (NOT);
		}
    break;

  case 103:
#line 824 "grammar.y"
    {
		    if (!errcnt)
			compile (CHSIGN);
		}
    break;

  case 104:
#line 829 "grammar.y"
    {
		    /* Free format scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		}
    break;

  case 105:
#line 833 "grammar.y"
    {
		    if (!errcnt) {
			struct	operand o;
			o.o_type = OT_INT;
		        o.o_val.v_i = pop();	/* get total number of args*/
			compile (PUSHCONST, &o);
			compile (SCAN);
		    }
		}
    break;

  case 106:
#line 842 "grammar.y"
    {
		    /* Formatted scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		}
    break;

  case 107:
#line 846 "grammar.y"
    {
		    if (!errcnt) {
			struct	operand o;

			/* Compile number of arguments. */
			o.o_type = OT_INT;
		        o.o_val.v_i = pop();
			compile (PUSHCONST, &o);

			compile (SCANF);
		    }
		}
    break;

  case 108:
#line 859 "grammar.y"
    {
		    /* Free format scan from a parameter.  */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		}
    break;

  case 109:
#line 863 "grammar.y"
    {
		    if (!errcnt) {
			struct	operand o;
			o.o_type = OT_INT;
			o.o_val.v_i = pop();	/* get total number of args*/
			compile (PUSHCONST, &o);
			compile (FSCAN);
		    }
		}
    break;

  case 110:
#line 873 "grammar.y"
    {
		    /* Formatted scan from a parameter.
		     * fscanf (param, format, arg1, ...)
		     */
		    if (!errcnt) {
			compile (PUSHCONST, stkop ((yyvsp[(3) - (4)])));
		        push (1);	/* use control stack to count args */
		    }
		}
    break;

  case 111:
#line 881 "grammar.y"
    {
		    if (!errcnt) {
			struct	operand o;

			/* Compile number of arguments. */
			o.o_type = OT_INT;
			o.o_val.v_i = pop();
			compile (PUSHCONST, &o);

			compile (FSCANF);
		    }
		}
    break;

  case 112:
#line 894 "grammar.y"
    {
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		}
    break;

  case 113:
#line 897 "grammar.y"
    {
		    if (!errcnt) {
			struct	operand o;
			o.o_type = OT_INT;
			o.o_val.v_i = pop();
			compile (PUSHCONST, &o);
			compile (INTRINSIC, stkop((yyvsp[(1) - (5)]))->o_val.v_s);
		    }
		}
    break;

  case 115:
#line 912 "grammar.y"
    {
			/* The YACC value of this must match normal intrinsics
			 * so we must generate an operand with the proper
			 * string. 
			 */
			if (!errcnt)
			    (yyval) = addconst ("int", OT_STRING);
		}
    break;

  case 116:
#line 920 "grammar.y"
    {
			if (!errcnt)
			    (yyval) = addconst ("real", OT_STRING);
		}
    break;

  case 117:
#line 926 "grammar.y"
    {
		    if (!errcnt) {
		        push (pop() + 1);		/* inc num args	*/
		    }
		}
    break;

  case 119:
#line 939 "grammar.y"
    {
                    if (!errcnt) {
                        compile (PUSHCONST, stkop ((yyvsp[(1) - (1)])));
                        push (pop() + 1);               /* inc num args */
                    }
		}
    break;

  case 120:
#line 945 "grammar.y"
    {
                    if (!errcnt) {
                        compile (PUSHCONST, stkop ((yyvsp[(1) - (3)])));
                        push (pop() + 1);               /* inc num args */
                    }
		}
    break;

  case 122:
#line 956 "grammar.y"
    {
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		}
    break;

  case 123:
#line 960 "grammar.y"
    {
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		}
    break;

  case 148:
#line 1000 "grammar.y"
    {
		    bracelevel++;
		}
    break;

  case 149:
#line 1002 "grammar.y"
    {
		    --bracelevel;
		}
    break;

  case 153:
#line 1014 "grammar.y"
    {
			--parenlevel;
 			if (!errcnt)
		      	    compile (ASSIGN, stkop((yyvsp[(1) - (3)]))->o_val.v_s);
		}
    break;

  case 154:
#line 1019 "grammar.y"
    {
			/* Old code pushed a constant rather than a param
			 * when not within braces.  This doesn't seem
			 * to be what most people want.
			 */
			--parenlevel;
			if (!errcnt) {
			    compile (PUSHPARAM, stkop((yyvsp[(3) - (3)]))->o_val.v_s);
		            compile (ASSIGN, stkop((yyvsp[(1) - (3)]))->o_val.v_s);
		    	}
		}
    break;

  case 155:
#line 1030 "grammar.y"
    {
			parenlevel++;
		}
    break;

  case 156:
#line 1033 "grammar.y"
    {
		      	--parenlevel;
			if (!errcnt)
		  	    compile ((yyvsp[(3) - (4)]), stkop((yyvsp[(1) - (4)]))->o_val.v_s);
		}
    break;

  case 157:
#line 1042 "grammar.y"
    {
			parenlevel++;
		}
    break;

  case 158:
#line 1047 "grammar.y"
    { (yyval) = ADDASSIGN; }
    break;

  case 159:
#line 1048 "grammar.y"
    { (yyval) = SUBASSIGN; }
    break;

  case 160:
#line 1049 "grammar.y"
    { (yyval) = MULASSIGN; }
    break;

  case 161:
#line 1050 "grammar.y"
    { (yyval) = DIVASSIGN; }
    break;

  case 162:
#line 1051 "grammar.y"
    { (yyval) = CATASSIGN; }
    break;

  case 163:
#line 1054 "grammar.y"
    {
		    npipes = 0;
		}
    break;

  case 164:
#line 1056 "grammar.y"
    {
		    if (!errcnt) {
			compile (EXEC);
			if (npipes > 0)
			    compile (RMPIPES, npipes);
		    }
		}
    break;

  case 166:
#line 1066 "grammar.y"
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

			if ((yyvsp[(2) - (2)]) == 1)
			    compile (REDIR);
			else
			    compile (ALLREDIR);
			compile (EXEC);

		    } else {
			eprintf ("multiple redirection\n");
			YYERROR;
		    }

		}
    break;

  case 167:
#line 1100 "grammar.y"
    {
		    /* Compile the GETPIPE instruction with the name of the
		     * second task in the current pipe, and backpatch the
		     * matching ADDPIPE instruction with the PC of the GETPIPE.
		     */
		    (coderef(pipe_pc))->c_args = compile (GETPIPE, curr_task);
		    compile (REDIRIN);
		    npipes++;		/* Overflow checking is in ADDPIPE */
		}
    break;

  case 168:
#line 1111 "grammar.y"
    {
		    (yyval) = 1;
		}
    break;

  case 169:
#line 1114 "grammar.y"
    {
		    (yyval) = 2;
		}
    break;

  case 170:
#line 1119 "grammar.y"
    {
		    char    *ltname;

		    ltname = stkop((yyvsp[(1) - (1)]))->o_val.v_s;
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
		}
    break;

  case 171:
#line 1143 "grammar.y"
    {
		    inarglist = 1;
		}
    break;

  case 172:
#line 1145 "grammar.y"
    {
		    extern char *onerr_handler;

		    inarglist = 0;
		    parenlevel = 0;
		    scanstmt = 0;
		}
    break;

  case 173:
#line 1154 "grammar.y"
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
		}
    break;

  case 178:
#line 1173 "grammar.y"
    {
		    if (!errcnt) {
			if (posit > 0) {		/* not first time */
			    compile (POSARGSET, -posit);
			    printstmt = 0;
			    scanstmt = 0;
			}
			posit++;
		    }
		}
    break;

  case 179:
#line 1183 "grammar.y"
    {
		    if (absmode) {
			errmsg = posfirst;
			EYYERROR;
		    } else
			if (!errcnt)
			    compile (POSARGSET, posit++);
		}
    break;

  case 180:
#line 1191 "grammar.y"
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
			    breakout (stkop((yyvsp[(1) - (1)]))->o_val.v_s, &pk, &t, &p, &f);
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
				strcpy (pname, stkop((yyvsp[(1) - (1)]))->o_val.v_s);

			    o = *(stkop((yyvsp[(1) - (1)])));
			    o.o_val.v_s = pname;
			    compile (PUSHCONST, &o);
			    compile (INDIRPOSSET, posit++);

			} else if (parenlevel == 0 || printstmt) {
			    compile (PUSHCONST, stkop((yyvsp[(1) - (1)])));
			    compile (INDIRPOSSET, posit++);
			    /* only first arg of fprint stmt is special. */
			    printstmt = 0;

			} else {
			    compile (PUSHPARAM, stkop((yyvsp[(1) - (1)]))->o_val.v_s);
			    compile (POSARGSET, posit++);
			}
		    }
		}
    break;

  case 181:
#line 1237 "grammar.y"
    {
		    absmode++;
		    if (!errcnt)
			compile (ABSARGSET, stkop((yyvsp[(1) - (3)]))->o_val.v_s); 
		}
    break;

  case 182:
#line 1242 "grammar.y"
    {
		    absmode++;
		    if (!errcnt) {
			if (parenlevel == 0) {
			    compile (PUSHCONST, stkop((yyvsp[(3) - (3)])));
			    compile (INDIRABSSET, stkop((yyvsp[(1) - (3)]))->o_val.v_s); 
			} else {
			    compile (PUSHPARAM, stkop((yyvsp[(3) - (3)]))->o_val.v_s);
			    compile (ABSARGSET, stkop((yyvsp[(1) - (3)]))->o_val.v_s);
			}
		    }
		}
    break;

  case 183:
#line 1254 "grammar.y"
    {
		    absmode++;
		    if (!errcnt)
			compile (SWON, stkop((yyvsp[(1) - (2)]))->o_val.v_s);
		}
    break;

  case 184:
#line 1259 "grammar.y"
    {
		    absmode++;
		    if (!errcnt)
			compile (SWOFF, stkop((yyvsp[(1) - (2)]))->o_val.v_s);
		}
    break;

  case 185:
#line 1264 "grammar.y"
    {
		    if (!errcnt)
			compile (REDIRIN);
		}
    break;

  case 186:
#line 1268 "grammar.y"
    {
		    newstdout++;
		    if (!errcnt)
			compile (REDIR);
		}
    break;

  case 187:
#line 1273 "grammar.y"
    {
		    newstdout++;
		    if (!errcnt)
			compile (ALLREDIR);
		}
    break;

  case 188:
#line 1278 "grammar.y"
    {
		    newstdout++;
		    if (!errcnt)
			compile (APPENDOUT);
		}
    break;

  case 189:
#line 1283 "grammar.y"
    {
		    newstdout++;
		    if (!errcnt)
			compile (ALLAPPEND);
		}
    break;

  case 190:
#line 1288 "grammar.y"
    {
		    if (!errcnt)
			compile (GSREDIR, stkop((yyvsp[(1) - (2)]))->o_val.v_s);
		}
    break;

  case 191:
#line 1294 "grammar.y"
    {
		    absmode++;
		    /* constant already pushed by expr0.
		     */
		}
    break;

  case 192:
#line 1299 "grammar.y"
    {
		    absmode++;
		    if (!errcnt) {
			if (parenlevel == 0)
			    compile (PUSHCONST, stkop((yyvsp[(1) - (1)])));
			else
			    compile (PUSHPARAM, stkop((yyvsp[(1) - (1)]))->o_val.v_s);
			}
		}
    break;

  case 193:
#line 1310 "grammar.y"
    {
			--parenlevel;
			if (!errcnt)
			    compile (IMMED);
		}
    break;

  case 194:
#line 1315 "grammar.y"
    {
		      	--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop((yyvsp[(2) - (2)]))->o_val.v_s);
		}
    break;

  case 195:
#line 1322 "grammar.y"
    {
			--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop((yyvsp[(1) - (2)]))->o_val.v_s);
		}
    break;

  case 196:
#line 1329 "grammar.y"
    {
		    if (!errcnt)
			compile (OSESC, stkop((yyvsp[(1) - (1)]))->o_val.v_s);
		}
    break;

  case 197:
#line 1335 "grammar.y"
    {
		    --parenlevel;
		    if (!errcnt)
			compile (IMMED);
		}
    break;

  case 198:
#line 1345 "grammar.y"
    {
		    /* pop BIFF addr and set branch to just after statement */
		    if (!errcnt) {
		        XINT   biffaddr = pop();
		    	coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
		    }
		    in_iferr = 0;
		}
    break;

  case 199:
#line 1355 "grammar.y"
    {
		    if (++in_iferr > 1) { 
			errmsg = nestediferr; 
			EYYERROR; 
		    } 
		    compile (CALL, "_errpsh");
		    compile (EXEC);

		}
    break;

  case 200:
#line 1363 "grammar.y"
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
		}
    break;

  case 201:
#line 1375 "grammar.y"
    {
		    in_iferr--;
		}
    break;

  case 202:
#line 1380 "grammar.y"
    {
		    if (!errcnt) {
			/* Pop and save BIFF address, compile and push addr 
			 * of GOTO, and set BIFF branch to just after GOTO.
			 */
			XINT  biffaddr = pop();
			push (compile (GOTO, 0));
			coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
		    }

		}
    break;

  case 203:
#line 1390 "grammar.y"
    {
		    if (!errcnt) {
			/* Pop GOTO addr and set branch to just after statement
			 */
		    	XINT  gotoaddr = pop();
			coderef (gotoaddr)->c_args = pc - gotoaddr - SZ_CE;
		    }
		}
    break;

  case 204:
#line 1400 "grammar.y"
    { iferr_tok = 0; }
    break;

  case 205:
#line 1401 "grammar.y"
    { iferr_tok = 1; }
    break;

  case 208:
#line 1412 "grammar.y"
    {
		    /* pop BIFF addr and set branch to just after statement
		     */
		    XINT   biffaddr;
		    if (!errcnt) {
			biffaddr = pop();
		    	coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
		    }
		}
    break;

  case 209:
#line 1423 "grammar.y"
    {
			/* save BIFF addr so branch can be filled in 
			 */
			if (!errcnt)
			    push (compile (BIFF, 0));
		}
    break;

  case 210:
#line 1428 "grammar.y"
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
		}
    break;

  case 211:
#line 1456 "grammar.y"
    {
		    XINT  biffaddr;

		    ifseen = NULL;
		    if (!errcnt) {
			/* Pop and save BIFF address, compile and push addr 
			 * of GOTO, and set BIFF branch to just after GOTO.
			 */
			biffaddr = pop();
			push (compile (GOTO, 0));
			coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
		    }
		}
    break;

  case 212:
#line 1468 "grammar.y"
    {
		    XINT  gotoaddr;
		    if (!errcnt) {
			/* Pop GOTO addr and set branch to just after statement
			 */
			gotoaddr = pop();
			coderef (gotoaddr)->c_args = pc - gotoaddr - SZ_CE;
		    }
		}
    break;

  case 213:
#line 1479 "grammar.y"
    {
		    /* Save starting addr of while expression.
		     */
		    if (!errcnt) {
			push (pc);
			loopincr();
		    }
		}
    break;

  case 214:
#line 1486 "grammar.y"
    {
		    /* Save BIFF addr so branch can be filled in.
		     */
		    if (!errcnt)
			push (compile (BIFF, 0));
		}
    break;

  case 215:
#line 1491 "grammar.y"
    {
		    XINT  biffaddr;

		    if (!errcnt) {
			/* Pop and save addr of BIFF instruction.	   */
			biffaddr = pop();
			/* Pop addr of expression and build a goto there.  */
			compile (GOTO, pop() - pc - SZ_CE);
			/* Now can set BIFF branch to just after statement.*/
			coderef (biffaddr)->c_args = pc - biffaddr - SZ_CE;
			loopdecr();
		    }
		}
    break;

  case 216:
#line 1524 "grammar.y"
    {
			if (!errcnt)
			    push(pc);				/* Loop1: */
		}
    break;

  case 217:
#line 1528 "grammar.y"
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
		}
    break;

  case 218:
#line 1544 "grammar.y"
    {
			XINT  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-SZ_CE); 	/* Goto loop1 */
			    stmtaddr = pop();
			    coderef(stmtaddr)->c_args = pc - stmtaddr - SZ_CE;
			}
		}
    break;

  case 219:
#line 1554 "grammar.y"
    {
			XINT  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-SZ_CE); /* goto loop2 */

			    if (for_expr) {
				stmtaddr = pop();
				coderef(stmtaddr)->c_args = pc-stmtaddr-SZ_CE;
			    }
			    loopdecr();
			}
		}
    break;

  case 222:
#line 1577 "grammar.y"
    {
			for_expr = YES;
		}
    break;

  case 223:
#line 1580 "grammar.y"
    {
			for_expr = NO;
		}
    break;

  case 224:
#line 1606 "grammar.y"
    {
			if (!errcnt) {
			    push (compile(SWITCH));

			    /* Compile GOTO which will branch past end of
			     * switch.  This is needed if there is no DEFAULT.
			     */
			    compile (GOTO, 0);
			}
		}
    break;

  case 225:
#line 1615 "grammar.y"
    {
			/* Set up jumptable and pop space on stack.
			 */
			if (!errcnt)
			    setswitch();
		}
    break;

  case 226:
#line 1623 "grammar.y"
    {
			if (!errcnt) {
			    ncaseval = 0;
			    if (!in_switch()) {
				errmsg = "Improper CASE statement.";
				EYYERROR;
			    }
			}
		}
    break;

  case 227:
#line 1631 "grammar.y"
    {
			XINT  pcase;

			if (!errcnt) {
			    pcase = compile (CASE, ncaseval);

			    /* Fill in argument list. 
			     */
			    caseset (&(coderef(pcase)->c_args), ncaseval);
			    push (pcase);
			}
		}
    break;

  case 228:
#line 1642 "grammar.y"
    {
			/* Branch to end of switch block 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		}
    break;

  case 229:
#line 1650 "grammar.y"
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
		}
    break;

  case 230:
#line 1660 "grammar.y"
    {
		      	/* Branch past jump table. 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		}
    break;

  case 231:
#line 1668 "grammar.y"
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
		}
    break;

  case 232:
#line 1683 "grammar.y"
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
		}
    break;

  case 233:
#line 1707 "grammar.y"
    {
			if (!errcnt)
			    compile (END);
		}
    break;

  case 234:
#line 1711 "grammar.y"
    {
			/* Return values currently not implemented.
			 */
			eprintf ("Warning: return value ignored.\n");
			if (!errcnt)
			    compile (END);
		}
    break;

  case 235:
#line 1723 "grammar.y"
    {
			bracelevel -= PBRACE;
			if (bracelevel < 0) {
			    errmsg = "Too few left braces.";
			    EYYERROR;
			} else if (bracelevel > 0) {
			    errmsg = "Too few right braces.";
			    EYYERROR;
			}
		}
    break;

  case 236:
#line 1735 "grammar.y"
    {
			/* Put symbol in table in dictionary and
			 * process indirect references if present.
			 */
			struct label *l;

			if (!errcnt) {
			    l = getlabel (stkop((yyvsp[(1) - (3)])));

			    if (l == NULL) {
				l = setlabel (stkop((yyvsp[(1) - (3)])));
				l->l_loc = pc;
			    } else if (l->l_defined) {
			        errmsg = "Identical labels.";
				EYYERROR;
			    } else {
				/* Get this GOTO out of the
				 * indirect list so we can use
				 * the argument as the destination
				 */
				XINT  gotopc;
				gotopc = l->l_loc;
				unsetigoto (gotopc);

				/* Fix the indirect reference. 
				 */
				coderef(gotopc)->c_args = pc - gotopc - SZ_CE;
			    }
			    (l->l_defined)++;
			}
		}
    break;

  case 238:
#line 1769 "grammar.y"
    {
			/* Get the address corresponding to the label.
			 */
			struct label *l;

			if (!errcnt) {
			    l = getlabel (stkop((yyvsp[(2) - (2)])));

			    if (l != NULL)
				compile (GOTO, l->l_loc - pc - SZ_CE);
			    else {
				/* Ready for indirect GOTO 
				 */
				l = setlabel (stkop((yyvsp[(2) - (2)])));
				l->l_loc = pc;
				setigoto (compile(GOTO, 0));
				l->l_defined = 0;
			    }
			}
		}
    break;

  case 241:
#line 1799 "grammar.y"
    { 
			/* Save pc before compiling statement for loop back
			 */
			stmt_pc = pc;
			n_oarr = 0;
			i_oarr = 0;
			ifseen = NULL;
		}
    break;

  case 242:
#line 1807 "grammar.y"
    {
		      	/* If there was an open reference compile the
			 * loop increment and goback.
			 */
			XINT push_pc;

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
		}
    break;

  case 244:
#line 1846 "grammar.y"
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
		}
    break;

  case 247:
#line 1911 "grammar.y"
    {
				if (!errcnt) {
				    push(stkop((yyvsp[(1) - (1)]))) ; 
				    ncaseval++;
				}
			}
    break;

  case 250:
#line 1931 "grammar.y"
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

				strncpy (curr_param, stkop((yyvsp[(1) - (1)]))->o_val.v_s, 
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
		}
    break;

  case 251:
#line 1968 "grammar.y"
    {
		    if (!errcnt) {
			strncpy (curr_param, stkop((yyvsp[(1) - (1)]))->o_val.v_s, SZ_FNAME);
			index_cnt = 0;
		    }
		}
    break;

  case 252:
#line 1975 "grammar.y"
    {
		    if (i_oarr > 0  &&  n_oarr == 0)
			n_oarr = i_oarr;
		    i_oarr = 0;
		    lastref = YES;
		}
    break;

  case 253:
#line 1983 "grammar.y"
    {
			index_cnt = 1;
		}
    break;

  case 254:
#line 1986 "grammar.y"
    {
			index_cnt++;
		}
    break;

  case 256:
#line 1992 "grammar.y"
    {
			if (!errcnt)
			    compile (PUSHINDEX, 0);
		}
    break;

  case 257:
#line 1997 "grammar.y"
    { 
			if (!errcnt) {
			    compile (PUSHPARAM, stkop((yyvsp[(1) - (1)]))->o_val.v_s);
			    compile (PUSHINDEX, 0);
			}
		}
    break;

  case 258:
#line 2003 "grammar.y"
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
		}
    break;

  case 259:
#line 2024 "grammar.y"
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
				sexa_to_index (stkop((yyvsp[(1) - (1)]))->o_val.v_r, &i1, &i2);
				mode = make_imloop (i1, i2);
				if (mode)
				    compile (PUSHINDEX, mode);
				else
				    push (compile (PUSHINDEX, mode));
			    } else {
				compile (PUSHCONST, stkop((yyvsp[(1) - (1)])));
				compile (PUSHINDEX, 0);
			    }
			}
		}
    break;

  case 260:
#line 2058 "grammar.y"
    {
		    (yyval) = (yyvsp[(1) - (1)]);
		}
    break;

  case 261:
#line 2063 "grammar.y"
    {
		    (yyval) = (yyvsp[(1) - (1)]);
		}
    break;

  case 262:
#line 2068 "grammar.y"
    {
		    (yyval) = (yyvsp[(1) - (1)]);
		}
    break;

  case 264:
#line 2074 "grammar.y"
    {
		    /* If statements are delimited by ';'s, do not execute
		     * until next newline EOST is received.
		     */
		    sawnl = 0;
		}
    break;

  case 270:
#line 2096 "grammar.y"
    { parenlevel++; }
    break;

  case 271:
#line 2099 "grammar.y"
    { --parenlevel; }
    break;

  case 272:
#line 2102 "grammar.y"
    { sawnl = 1; }
    break;


/* Line 1267 of yacc.c.  */
#line 4426 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 2105 "grammar.y"


#include "lexyy.c"
#include "lexicon.c"

