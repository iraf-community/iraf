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
     YOP_AOCAT = 297,
     YOP_AODIV = 298,
     YOP_AOMUL = 299,
     YOP_AOSUB = 300,
     YOP_AOADD = 301,
     YOP_OR = 302,
     YOP_AND = 303,
     YOP_NE = 304,
     YOP_EQ = 305,
     YOP_GE = 306,
     YOP_LE = 307,
     YOP_CONCAT = 308,
     UMINUS = 309,
     YOP_NOT = 310,
     YOP_POW = 311
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
#define YOP_AOCAT 297
#define YOP_AODIV 298
#define YOP_AOMUL 299
#define YOP_AOSUB 300
#define YOP_AOADD 301
#define YOP_OR 302
#define YOP_AND 303
#define YOP_NE 304
#define YOP_EQ 305
#define YOP_GE 306
#define YOP_LE 307
#define YOP_CONCAT 308
#define UMINUS 309
#define YOP_NOT 310
#define YOP_POW 311




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

int	dobkg = 0;		/* set when want to do code in bkground	*/
int	npipes = 0;		/* number of pipes in a command		*/
XINT	pipe_pc;		/* pc of last ADDPIPE instruction	*/
int	posit = 0;		/* positional argument count		*/
int	inarglist = 0;		/* set when in argument list		*/
int	parenlevel = 0;		/* level of paren nesting in command	*/
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
#line 328 "y.tab.c"

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
#define YYLAST   947

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  77
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  126
/* YYNRULES -- Number of rules.  */
#define YYNRULES  260
/* YYNRULES -- Number of states.  */
#define YYNSTATES  427

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   311

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    61,     2,     2,
      75,    76,    59,    57,    74,    58,    65,    60,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    72,    68,
      52,    42,    53,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    70,     2,    71,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    67,    73,    69,    66,     2,     2,     2,
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
      35,    36,    37,    38,    39,    40,    41,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    54,    55,    56,    62,
      63,    64
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
     382,   385,   387,   389,   391,   393,   395,   397,   399,   402,
     405,   408,   411,   413,   415,   417,   420,   421,   422,   429,
     430,   434,   438,   442,   443,   448,   450,   452,   454,   456,
     458,   460,   461,   465,   466,   467,   472,   475,   478,   479,
     480,   487,   488,   492,   494,   496,   500,   501,   503,   505,
     509,   513,   516,   519,   522,   525,   528,   531,   534,   537,
     539,   541,   544,   547,   550,   552,   554,   556,   557,   565,
     566,   572,   573,   574,   583,   584,   585,   586,   603,   605,
     606,   608,   609,   610,   621,   622,   623,   631,   632,   638,
     640,   642,   644,   647,   650,   651,   657,   660,   662,   665,
     666,   669,   671,   674,   676,   680,   682,   683,   685,   687,
     688,   694,   696,   697,   702,   704,   706,   708,   710,   712,
     714,   716,   718,   720,   722,   723,   725,   726,   728,   730,
     732
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      78,     0,    -1,    -1,    65,   202,    -1,    -1,    78,    79,
      80,   183,    -1,    83,    -1,    84,    -1,     1,   202,    -1,
      -1,    -1,    82,   196,    81,    80,    -1,    13,    -1,    14,
      16,    -1,    66,    -1,    86,    91,   114,    -1,    -1,   114,
      85,   132,   187,   178,    -1,    -1,    29,    87,   194,    88,
     196,    -1,    -1,   200,    89,   201,    -1,    -1,    90,    -1,
     194,    -1,    90,   197,   194,    -1,    -1,    92,    -1,    93,
      -1,    92,    93,    -1,   196,    -1,    94,    -1,     1,   202,
      -1,    -1,    96,    95,    97,   196,    -1,    32,    -1,    35,
      -1,    34,    -1,    36,    -1,    38,    -1,    39,    -1,    40,
      -1,    41,    -1,    33,    -1,    37,    -1,    98,    -1,    98,
     197,    97,    -1,    99,    -1,    99,    67,   111,    68,    69,
      -1,   101,    -1,    -1,   101,    42,   100,   106,    -1,   103,
      -1,    -1,   103,   102,    70,   104,    71,    -1,   194,    -1,
      59,   194,    -1,    -1,   105,    -1,   104,   197,   105,    -1,
     108,    -1,   108,    72,   108,    -1,   107,    -1,   106,   197,
     107,    -1,   108,    -1,    16,   200,   108,   201,    -1,    16,
      -1,   109,    -1,   110,    16,    -1,    57,    -1,    58,    -1,
     106,   197,   112,    -1,   106,    -1,   112,    -1,   113,    -1,
     112,   197,   113,    -1,    17,    42,   108,    -1,    30,   202,
      -1,   116,    -1,   188,    -1,   117,    -1,    16,    -1,    38,
      -1,    39,    -1,    40,    -1,    41,    -1,   200,   115,   201,
      -1,   115,    57,   187,   115,    -1,   115,    58,   187,   115,
      -1,   115,    59,   187,   115,    -1,   115,    60,   187,   115,
      -1,   115,    64,   187,   115,    -1,   115,    61,   187,   115,
      -1,   115,    56,   187,   115,    -1,   115,    52,   187,   115,
      -1,   115,    53,   187,   115,    -1,   115,    55,   187,   115,
      -1,   115,    54,   187,   115,    -1,   115,    51,   187,   115,
      -1,   115,    50,   187,   115,    -1,   115,    48,   187,   115,
      -1,   115,    49,   187,   115,    -1,    63,   115,    -1,    58,
     115,    -1,    -1,     3,   200,   118,   125,   201,    -1,    -1,
       4,   200,   119,   124,   197,   125,   201,    -1,    -1,     5,
     200,   120,   125,   201,    -1,    -1,     6,   200,    17,   197,
     121,   124,   197,   125,   201,    -1,    -1,   123,   200,   122,
     126,   201,    -1,   193,    -1,    33,    -1,    34,    -1,   115,
      -1,    -1,    17,    -1,    17,   197,   125,    -1,    -1,   115,
      -1,   126,   197,   115,    -1,   128,    -1,   133,   196,    -1,
     137,   196,    -1,   150,   196,    -1,   151,   196,    -1,   152,
     196,    -1,   153,   196,    -1,   154,    -1,   157,    -1,   159,
      -1,   162,    -1,   168,    -1,   170,    -1,   173,    -1,   175,
     196,    -1,   176,   196,    -1,   181,   196,    -1,   177,   196,
      -1,   179,    -1,   182,    -1,   129,    -1,   129,   202,    -1,
      -1,    -1,    67,   130,   132,   187,   131,    69,    -1,    -1,
     132,   187,   183,    -1,   188,   135,   116,    -1,   188,   135,
     188,    -1,    -1,   188,   134,   136,   115,    -1,    42,    -1,
      47,    -1,    46,    -1,    45,    -1,    44,    -1,    43,    -1,
      -1,   142,   138,   139,    -1,    -1,    -1,   139,   141,   140,
     142,    -1,    73,   187,    -1,    12,   187,    -1,    -1,    -1,
     195,   143,   198,   144,   145,   199,    -1,    -1,   197,   146,
     147,    -1,   147,    -1,   148,    -1,   147,   197,   148,    -1,
      -1,   116,    -1,   188,    -1,   188,    42,   116,    -1,   188,
      42,   188,    -1,   194,    57,    -1,   194,    58,    -1,    52,
     149,    -1,    53,   149,    -1,    10,   149,    -1,     8,   149,
      -1,     9,   149,    -1,    11,   149,    -1,   116,    -1,   194,
      -1,   135,   116,    -1,   135,   188,    -1,   188,   135,    -1,
       7,    -1,   135,    -1,   155,    -1,    -1,    19,   200,   115,
     201,   156,   187,   183,    -1,    -1,   155,    20,   158,   187,
     183,    -1,    -1,    -1,    18,   200,   160,   115,   201,   161,
     187,   183,    -1,    -1,    -1,    -1,    21,   200,   187,   166,
      68,   187,   163,   167,    68,   187,   164,   166,   201,   187,
     165,   127,    -1,   133,    -1,    -1,   115,    -1,    -1,    -1,
      24,   187,   200,   187,   115,   187,   201,   187,   169,   183,
      -1,    -1,    -1,    25,   171,   185,    72,   187,   172,   183,
      -1,    -1,    26,    72,   187,   174,   183,    -1,    23,    -1,
      22,    -1,    27,    -1,    27,   115,    -1,    31,   202,    -1,
      -1,    17,    72,   187,   180,   183,    -1,    28,    17,    -1,
      68,    -1,    68,   202,    -1,    -1,   184,   127,    -1,    94,
      -1,     1,   202,    -1,   186,    -1,   186,   197,   185,    -1,
      16,    -1,    -1,   202,    -1,   194,    -1,    -1,   194,   189,
      70,   190,    71,    -1,   192,    -1,    -1,   192,   191,   197,
     190,    -1,   117,    -1,   188,    -1,    59,    -1,    16,    -1,
      17,    -1,    17,    -1,    17,    -1,   202,    -1,    68,    -1,
      74,    -1,    -1,   200,    -1,    -1,   201,    -1,    75,    -1,
      76,    -1,    15,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   138,   138,   161,   173,   173,   193,   204,   216,   257,
     258,   258,   268,   271,   280,   285,   295,   295,   321,   321,
     343,   346,   352,   355,   358,   363,   370,   371,   374,   375,
     378,   379,   380,   417,   417,   454,   455,   456,   457,   458,
     459,   460,   461,   462,   463,   466,   467,   470,   488,   506,
     510,   510,   518,   523,   523,   542,   546,   557,   561,   562,
     566,   589,   606,   607,   610,   618,   639,   640,   646,   660,
     661,   663,   673,   681,   684,   685,   688,   695,   703,   704,
     717,   718,   722,   730,   734,   738,   744,   746,   750,   754,
     758,   762,   766,   775,   779,   783,   787,   791,   795,   799,
     803,   807,   811,   815,   820,   820,   833,   833,   850,   850,
     864,   864,   885,   885,   902,   903,   911,   917,   924,   930,
     936,   944,   947,   951,   960,   961,   962,   963,   964,   965,
     966,   967,   968,   969,   970,   971,   972,   973,   974,   975,
     976,   977,   978,   979,   985,   986,   989,   991,   989,   996,
     997,  1003,  1008,  1019,  1019,  1031,  1036,  1037,  1038,  1039,
    1040,  1043,  1043,  1054,  1055,  1055,  1100,  1103,  1108,  1132,
    1108,  1141,  1141,  1152,  1155,  1156,  1160,  1170,  1178,  1224,
    1229,  1241,  1246,  1251,  1255,  1260,  1265,  1270,  1275,  1281,
    1286,  1297,  1302,  1309,  1316,  1322,  1329,  1340,  1340,  1373,
    1373,  1396,  1403,  1396,  1441,  1445,  1461,  1441,  1490,  1491,
    1494,  1497,  1523,  1522,  1540,  1548,  1540,  1567,  1567,  1585,
    1600,  1624,  1628,  1640,  1652,  1652,  1686,  1708,  1709,  1716,
    1716,  1762,  1763,  1819,  1820,  1823,  1839,  1840,  1843,  1880,
    1880,  1895,  1898,  1898,  1904,  1908,  1915,  1936,  1970,  1975,
    1980,  1985,  1986,  1994,  1997,  1998,  2001,  2002,  2008,  2011,
    2014
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
  "Y_STRUCT", "Y_GCUR", "Y_IMCUR", "Y_UKEY", "Y_PSET", "'='", "YOP_AOCAT",
  "YOP_AODIV", "YOP_AOMUL", "YOP_AOSUB", "YOP_AOADD", "YOP_OR", "YOP_AND",
  "YOP_NE", "YOP_EQ", "'<'", "'>'", "YOP_GE", "YOP_LE", "YOP_CONCAT",
  "'+'", "'-'", "'*'", "'/'", "'%'", "UMINUS", "YOP_NOT", "YOP_POW", "'.'",
  "'~'", "'{'", "';'", "'}'", "'['", "']'", "':'", "'|'", "','", "'('",
  "')'", "$accept", "block", "@1", "debug", "@2", "D_XXX", "script_params",
  "script_body", "@3", "proc_stmt", "@4", "bparam_list", "param_list",
  "xparam_list", "var_decls", "var_decl_block", "var_decl_line",
  "var_decl_stmt", "@5", "typedefs", "var_decl_list", "var_decl_plus",
  "var_decl", "@6", "var_def", "@7", "var_name", "init_index_list",
  "init_index_range", "init_list", "init_elem", "const", "number", "sign",
  "options_list", "options", "option", "begin_stmt", "expr", "expr0",
  "expr1", "@8", "@9", "@10", "@11", "@12", "intrinsx", "scanfmt",
  "scanarg", "intrarg", "stmt", "c_stmt", "c_blk", "@13", "@14", "s_list",
  "assign", "@15", "equals", "assign_oper", "cmdlist", "@16", "cmdpipe",
  "@17", "pipe", "command", "@18", "@19", "args", "@20", "arglist", "arg",
  "file", "immed", "inspect", "osesc", "popstk", "if", "if_stat", "@21",
  "ifelse", "@22", "while", "@23", "@24", "for", "@25", "@26", "@27",
  "xassign", "xexpr", "switch", "@28", "case", "@29", "@30", "default",
  "@31", "next", "break", "return", "end_stmt", "label_stmt", "@32",
  "goto", "nullstmt", "xstmt", "@33", "const_expr_list", "const_expr",
  "opnl", "ref", "@34", "index_list", "@35", "index", "intrins", "param",
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
     295,   296,    61,   297,   298,   299,   300,   301,   302,   303,
     304,   305,    60,    62,   306,   307,   308,    43,    45,    42,
      47,    37,   309,   310,   311,    46,   126,   123,    59,   125,
      91,    93,    58,   124,    44,    40,    41
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    77,    78,    78,    79,    78,    78,    78,    78,    80,
      81,    80,    82,    82,    82,    83,    85,    84,    87,    86,
      88,    88,    89,    89,    90,    90,    91,    91,    92,    92,
      93,    93,    93,    95,    94,    96,    96,    96,    96,    96,
      96,    96,    96,    96,    96,    97,    97,    98,    98,    99,
     100,    99,   101,   102,   101,   103,   103,   104,   104,   104,
     105,   105,   106,   106,   107,   107,   108,   108,   109,   110,
     110,   111,   111,   111,   112,   112,   113,   114,   115,   115,
     116,   116,   116,   116,   116,   116,   117,   117,   117,   117,
     117,   117,   117,   117,   117,   117,   117,   117,   117,   117,
     117,   117,   117,   117,   118,   117,   119,   117,   120,   117,
     121,   117,   122,   117,   123,   123,   123,   124,   125,   125,
     125,   126,   126,   126,   127,   127,   127,   127,   127,   127,
     127,   127,   127,   127,   127,   127,   127,   127,   127,   127,
     127,   127,   127,   127,   128,   128,   130,   131,   129,   132,
     132,   133,   133,   134,   133,   135,   136,   136,   136,   136,
     136,   138,   137,   139,   140,   139,   141,   141,   143,   144,
     142,   146,   145,   145,   147,   147,   148,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   149,
     149,   150,   150,   151,   152,   153,   154,   156,   155,   158,
     157,   160,   161,   159,   163,   164,   165,   162,   166,   166,
     167,   167,   169,   168,   171,   172,   170,   174,   173,   175,
     176,   177,   177,   178,   180,   179,   181,   182,   182,   184,
     183,   183,   183,   185,   185,   186,   187,   187,   188,   189,
     188,   190,   191,   190,   192,   192,   192,   192,   193,   194,
     195,   196,   196,   197,   198,   198,   199,   199,   200,   201,
     202
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
       2,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       2,     2,     1,     1,     1,     2,     0,     0,     6,     0,
       3,     3,     3,     0,     4,     1,     1,     1,     1,     1,
       1,     0,     3,     0,     0,     4,     2,     2,     0,     0,
       6,     0,     3,     1,     1,     3,     0,     1,     1,     3,
       3,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       1,     2,     2,     2,     1,     1,     1,     0,     7,     0,
       5,     0,     0,     8,     0,     0,     0,    16,     1,     0,
       1,     0,     0,    10,     0,     0,     7,     0,     5,     1,
       1,     1,     2,     2,     0,     5,     2,     1,     2,     0,
       2,     1,     2,     1,     3,     1,     0,     1,     1,     0,
       5,     1,     0,     4,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     0,     1,     1,     1,
       1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     0,    18,     0,     0,     4,     6,     7,     0,    16,
     260,     8,     0,    77,     3,     1,     9,     0,    35,    43,
      37,    36,    38,    44,    39,    40,    41,    42,   252,     0,
       0,    28,    31,    33,    30,   251,   149,   249,    20,    12,
       0,    14,     0,     0,    32,    15,    29,     0,   236,   258,
       0,    22,    13,     0,   231,     5,     0,    10,     0,     0,
      45,    47,    49,    52,    55,     0,   237,    19,     0,    23,
      24,   232,   194,   250,     0,     0,     0,   220,   219,   236,
     214,     0,   221,     0,   155,   146,   227,   230,   124,   144,
       0,   195,     0,   161,     0,     0,     0,     0,   131,   196,
     132,   133,   134,   135,   136,   137,     0,     0,     0,   142,
       0,   143,   153,   238,   168,     9,    56,    34,   253,     0,
       0,    50,     0,     0,    17,   150,   259,    21,     0,   236,
     201,     0,   236,     0,     0,   236,     0,     0,     0,     0,
      81,   249,   115,   116,    82,    83,    84,    85,     0,     0,
     222,    78,    80,     0,    79,   114,     0,   226,   149,   228,
     145,   125,     0,    78,    79,   126,   163,   127,   128,   129,
     130,   199,   138,   139,   141,   140,     0,   193,     0,   254,
      11,    46,    66,     0,    69,    70,    72,    62,    64,    67,
       0,     0,    73,    74,     0,    57,   223,    25,   224,     0,
       0,   209,   236,   235,     0,   233,   217,   104,   106,   108,
       0,   103,   102,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   112,     0,
     236,   162,   236,   160,   159,   158,   157,   156,     0,    78,
      79,     0,   169,   255,     0,     0,     0,    68,     0,     0,
      51,    66,     0,    58,    60,     0,     0,   197,   208,     0,
     153,     0,   236,     0,     0,   118,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   121,    86,     0,   236,   236,   164,
       0,   154,    81,   246,    80,    79,     0,   241,   176,     0,
      76,    63,    71,    48,    75,     0,    54,     0,     0,   225,
     202,   236,   236,     0,   236,   215,   234,   218,   119,     0,
     117,     0,     0,   110,   100,   101,    99,    98,    94,    95,
      97,    96,    93,    87,    88,    89,    90,    92,    91,   122,
       0,     0,   167,   166,     0,   200,   240,     0,     0,     0,
       0,     0,     0,     0,    78,   256,   173,   174,    79,   238,
     171,    65,    59,    61,   236,     0,   204,     0,     0,   118,
     105,   118,   109,     0,     0,   113,   148,   250,   165,     0,
      78,   186,   238,   187,   185,   188,   183,   184,   170,   257,
     176,     0,   181,   182,   176,     0,   198,   211,   236,   216,
     120,     0,     0,   123,   243,   175,    78,    79,   172,   203,
     210,     0,   212,   107,   118,   236,     0,     0,   205,   213,
     111,   209,     0,   236,   206,     0,   207
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     5,    16,    42,   115,    43,     6,     7,    36,     8,
      12,    50,    68,    69,    29,    30,    31,    54,    47,    33,
      59,    60,    61,   194,    62,   122,    63,   252,   253,   186,
     187,   188,   189,   190,   191,   192,   193,     9,   162,   151,
     152,   265,   266,   267,   373,   284,   153,   321,   319,   340,
      87,    88,    89,   158,   341,    48,    90,   176,    91,   238,
      92,   166,   231,   344,   289,    93,   179,   298,   355,   394,
     356,   357,   381,    94,    95,    96,    97,    98,    99,   311,
     100,   232,   101,   199,   364,   102,   397,   421,   425,   259,
     411,   103,   416,   104,   134,   368,   105,   264,   106,   107,
     108,   124,   109,   255,   110,   111,   125,    56,   204,   205,
      65,   154,   178,   296,   347,   297,   155,   113,   114,    34,
     249,   242,   388,   156,   127,    66
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -261
static const yytype_int16 yypact[] =
{
     609,     0,  -261,     0,     0,    14,  -261,  -261,   405,  -261,
    -261,  -261,     6,  -261,  -261,  -261,    47,     0,  -261,  -261,
    -261,  -261,  -261,  -261,  -261,  -261,  -261,  -261,  -261,     3,
     447,  -261,  -261,  -261,  -261,  -261,  -261,  -261,   -35,  -261,
      28,  -261,   766,    -6,  -261,  -261,  -261,    35,     0,  -261,
      -6,     6,  -261,     0,  -261,  -261,   802,  -261,     6,    -6,
     -23,   -10,    59,    22,  -261,   714,  -261,  -261,    34,   -23,
    -261,  -261,  -261,   -25,   -35,   -35,   -35,  -261,  -261,     0,
    -261,    53,    64,   104,  -261,  -261,     0,  -261,  -261,     0,
      -6,    64,    -6,  -261,    -6,    -6,    -6,    -6,  -261,   110,
    -261,  -261,  -261,  -261,  -261,  -261,    -6,    -6,    -6,  -261,
      -6,  -261,    89,    74,  -261,    47,  -261,  -261,  -261,    35,
      33,  -261,    76,     0,  -261,  -261,  -261,  -261,     6,     0,
    -261,    64,     0,   -35,   132,     0,   -35,   -35,   -35,   -35,
    -261,    75,  -261,  -261,  -261,  -261,  -261,  -261,    64,    64,
     852,  -261,  -261,   -35,  -261,  -261,    64,  -261,  -261,  -261,
    -261,  -261,   852,    19,    21,  -261,  -261,  -261,  -261,  -261,
    -261,  -261,  -261,  -261,  -261,  -261,   112,    64,   105,   -35,
    -261,  -261,   -35,   138,  -261,  -261,   -23,  -261,  -261,  -261,
     172,   121,   -23,  -261,    -4,    83,  -261,  -261,  -261,    64,
     823,     6,     0,  -261,   119,   -23,  -261,  -261,  -261,  -261,
     178,   133,   133,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -261,   823,
       0,    -2,     0,  -261,  -261,  -261,  -261,  -261,    64,    10,
      17,    25,  -261,  -261,    83,    83,    33,  -261,   136,   184,
     -23,  -261,   -47,  -261,   135,   766,   823,  -261,  -261,   140,
      89,    64,     0,   132,   766,   198,    64,   198,   -23,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,  -261,   661,     0,     0,  -261,
     766,   852,    41,  -261,    43,    49,   147,   146,   515,    34,
    -261,  -261,   -23,  -261,  -261,    -4,  -261,    83,    83,  -261,
    -261,     0,     0,    64,   797,  -261,  -261,  -261,   -23,    34,
     852,   -23,    34,  -261,   868,   883,   486,   486,   126,   126,
     126,   126,   169,   152,   152,   133,   133,   133,  -261,   852,
      50,   156,  -261,  -261,   204,  -261,  -261,   -23,    64,    64,
      64,    64,    64,    64,   207,    34,   -23,  -261,   311,    38,
    -261,  -261,  -261,  -261,     0,   766,  -261,    34,   766,   198,
    -261,   198,  -261,    64,    64,  -261,  -261,  -261,  -261,    25,
     227,  -261,   130,  -261,  -261,  -261,  -261,  -261,  -261,  -261,
     554,    64,  -261,  -261,   554,   766,  -261,    64,     0,  -261,
    -261,    34,   -23,   852,  -261,  -261,   393,   460,   -23,  -261,
     852,   163,  -261,  -261,   198,     0,   766,    34,  -261,  -261,
    -261,     6,    34,     0,  -261,   802,  -261
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -261,  -261,  -261,   122,  -261,  -261,  -261,  -261,  -261,  -261,
    -261,  -261,  -261,  -261,  -261,  -261,   206,     8,  -261,  -261,
     124,  -261,  -261,  -261,  -261,  -261,  -261,  -261,   -69,    51,
    -233,  -189,  -261,  -261,  -261,    -5,    -3,   218,   120,   -43,
    -236,  -261,  -261,  -261,  -261,  -261,  -261,  -125,  -260,  -261,
    -173,  -261,  -261,  -261,  -261,    95,  -197,  -261,  -109,  -261,
    -261,  -261,  -261,  -261,  -261,   -90,  -261,  -261,  -261,  -261,
    -138,  -133,   -37,  -261,  -261,  -261,  -261,  -261,  -261,  -261,
    -261,  -261,  -261,  -261,  -261,  -261,  -261,  -261,  -261,  -159,
    -261,  -261,  -261,  -261,  -261,  -261,  -261,  -261,  -261,  -261,
    -261,  -261,  -261,  -261,  -261,  -261,   -41,  -261,     1,  -261,
     -53,   -48,  -261,  -113,  -261,  -261,  -261,   -12,  -261,   321,
     -58,  -261,  -261,    -1,  -123,   406
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -250
static const yytype_int16 yytable[] =
{
      38,    55,   119,   177,   258,   294,   254,   322,   112,    10,
     287,   128,   182,   301,    15,    10,    32,  -249,  -249,  -249,
    -249,  -249,  -249,    37,   306,  -151,   133,   118,   136,   137,
     138,   139,  -152,     3,  -191,    64,  -192,    51,    32,    70,
      49,   292,   141,   164,    52,  -249,   116,   129,   163,   182,
     183,   118,    37,   184,   185,   299,   300,   120,   142,   143,
      39,    40,    28,   144,   145,   146,   147,   136,   137,   138,
     139,   288,   301,   130,   131,   132,   198,   257,  -151,   201,
     140,   141,   206,   148,   293,  -152,  -151,  -191,   149,  -192,
     184,   185,   -53,  -152,    58,   392,   393,   142,   143,   251,
      49,   121,   144,   145,   146,   147,   285,    64,  -239,   400,
     126,   401,  -247,    41,  -244,  -247,   197,  -244,   254,   363,
    -245,   157,   148,  -245,   118,   135,   126,   149,   246,   240,
     171,    84,   202,   310,   239,   207,   208,   209,   210,    49,
     184,   185,  -190,   294,  -239,  -190,   195,   263,   203,   261,
    -248,   313,   228,   260,   417,   233,   234,   235,   236,   237,
     269,   270,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   241,   361,   286,   243,   290,
     245,   244,   221,   222,   223,   224,   225,   226,   247,   248,
     227,   262,   305,   295,   307,   268,   370,   227,  -190,   372,
    -239,   183,   150,  -190,  -190,   303,  -190,   308,   312,   315,
     323,   224,   225,   226,   309,   318,   227,   375,   346,  -177,
    -242,   377,  -177,   317,   258,   376,   222,   223,   224,   225,
     226,   415,   389,   227,   342,   343,    46,   180,   362,  -189,
     360,   302,  -189,   181,   398,   250,   304,    45,   402,   345,
     358,   200,   426,   230,   378,   354,   408,   405,   365,   366,
     369,   367,   422,   371,   316,   240,   404,     0,   211,   212,
     239,     0,     0,     0,     0,  -177,   229,     0,   413,     0,
    -177,  -177,   374,  -177,     0,     0,   359,     0,     0,   379,
       0,     0,     0,     0,   420,  -189,     0,     0,   390,   423,
    -189,  -189,     0,  -189,     0,   380,   380,   380,   380,   380,
     380,   395,   383,   384,   385,   386,   387,     0,     0,   256,
       0,     0,     0,  -178,   396,     0,  -178,   399,     0,     0,
       0,   295,     0,     0,     0,     0,   382,   382,   382,   382,
     382,   382,   358,   407,   414,   412,   358,   354,   406,     0,
     390,   354,     0,   391,   409,     0,     0,     0,   291,     0,
       0,     0,   418,     0,    57,     0,     0,     0,     0,     0,
     424,    67,     0,   260,     0,   419,     0,   112,   359,  -178,
     117,   314,   359,     0,  -178,  -178,   320,  -178,     0,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,  -179,    17,    11,  -179,    13,
      14,   161,     0,   165,    35,   167,   168,   169,   170,     0,
      10,     0,     0,    44,     0,     0,     0,   172,   173,   174,
       0,   175,     0,     0,     0,   -26,    35,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,     0,    17,    35,
       0,     0,     0,     0,     0,     0,    35,     0,     0,    71,
       0,  -179,    10,     0,     0,    35,  -179,  -179,     0,  -179,
       0,     0,  -180,    28,     0,  -180,     0,   -27,     0,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,     0,
       0,     0,   159,   320,   403,   160,    35,     0,    35,     0,
      35,    35,    35,    35,     0,     0,     0,     0,     0,     0,
       0,     0,    35,    35,    35,    28,    35,   410,   136,   137,
     138,   139,     0,   348,   349,   350,   351,     0,  -180,   196,
       0,   140,   141,  -180,  -180,     0,  -180,     0,   217,   218,
     219,   220,   221,   222,   223,   224,   225,   226,   142,   143,
     227,     0,     0,   144,   145,   146,   147,   136,   137,   138,
     139,     0,   348,   349,   350,   351,     0,   352,   353,     0,
     140,   141,     0,   148,     0,     0,     0,     0,   149,     0,
       0,     0,     0,     0,     0,     0,     0,   142,   143,   118,
      49,     0,   144,   145,   146,   147,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   352,   353,     0,    -2,
       1,     0,   148,     0,     0,     0,    -2,   149,     0,     0,
       0,     0,    -2,    -2,     0,     0,    -2,    -2,    -2,    49,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     2,     3,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    53,     0,     0,     0,     0,     0,  -229,     0,
       0,     0,     0,     0,     4,    -2,    -2,    -2,  -229,  -229,
    -229,     0,  -229,  -229,  -229,  -229,  -229,  -229,  -229,  -229,
       0,     0,     0,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,  -229,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    53,     0,     0,     0,     0,
       0,  -229,     0,     0,     0,     0,     0,     0,  -229,  -229,
    -147,  -229,  -229,  -229,     0,  -229,  -229,  -229,  -229,  -229,
    -229,  -229,  -229,     0,     0,   123,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,  -229,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    53,     0,     0,
       0,     0,     0,  -229,     0,     0,     0,     0,     0,     0,
       0,  -229,  -229,  -229,  -229,  -229,     0,  -229,  -229,  -229,
    -229,  -229,  -229,  -229,  -229,     0,     0,     0,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,  -229,    72,
       0,     0,    10,     0,     0,     0,     0,     0,     0,    73,
      74,    75,     0,    76,    77,    78,    79,    80,    81,    82,
      83,     0,     0,  -229,  -229,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,   213,   214,   215,   216,   217,
     218,   219,   220,   221,   222,   223,   224,   225,   226,     0,
       0,   227,     0,     0,     0,     0,     0,     0,     0,    85,
      86,   213,   214,   215,   216,   217,   218,   219,   220,   221,
     222,   223,   224,   225,   226,     0,     0,   227,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   126,
     213,   214,   215,   216,   217,   218,   219,   220,   221,   222,
     223,   224,   225,   226,     0,     0,   227,   214,   215,   216,
     217,   218,   219,   220,   221,   222,   223,   224,   225,   226,
       0,     0,   227,   215,   216,   217,   218,   219,   220,   221,
     222,   223,   224,   225,   226,     0,     0,   227
};

static const yytype_int16 yycheck[] =
{
      12,    42,    60,   112,   201,   241,   195,   267,    56,    15,
      12,    69,    16,   246,     0,    15,     8,    42,    43,    44,
      45,    46,    47,    17,    71,    15,    79,    74,     3,     4,
       5,     6,    15,    30,    15,    47,    15,    38,    30,    51,
      75,    16,    17,    91,    16,    70,    58,    72,    91,    16,
      17,    74,    17,    57,    58,   244,   245,    67,    33,    34,
      13,    14,    68,    38,    39,    40,    41,     3,     4,     5,
       6,    73,   305,    74,    75,    76,   129,   200,    68,   132,
      16,    17,   135,    58,    59,    68,    76,    68,    63,    68,
      57,    58,    70,    76,    59,    57,    58,    33,    34,    16,
      75,    42,    38,    39,    40,    41,   229,   119,    70,   369,
      76,   371,    71,    66,    71,    74,   128,    74,   307,   308,
      71,    17,    58,    74,    74,    72,    76,    63,   186,   177,
      20,    42,   133,   256,   177,   136,   137,   138,   139,    75,
      57,    58,    12,   379,    70,    15,    70,   205,    16,   202,
      75,   260,   153,   201,   414,    43,    44,    45,    46,    47,
     213,   214,   215,   216,   217,   218,   219,   220,   221,   222,
     223,   224,   225,   226,   227,    70,   299,   230,   179,   232,
      42,   182,    56,    57,    58,    59,    60,    61,    16,    68,
      64,    72,   250,   241,   252,    17,   319,    64,    68,   322,
      70,    17,    82,    73,    74,    69,    76,    72,    68,   262,
     268,    59,    60,    61,   255,    17,    64,   340,    71,    12,
      74,    17,    15,   264,   421,    69,    57,    58,    59,    60,
      61,    68,   355,    64,   287,   288,    30,   115,   307,    12,
     298,   246,    15,   119,   367,   194,   249,    29,   373,   290,
     298,   131,   425,   158,   344,   298,   394,   390,   311,   312,
     318,   314,   421,   321,   263,   313,   379,    -1,   148,   149,
     313,    -1,    -1,    -1,    -1,    68,   156,    -1,   401,    -1,
      73,    74,   340,    76,    -1,    -1,   298,    -1,    -1,   347,
      -1,    -1,    -1,    -1,   417,    68,    -1,    -1,   356,   422,
      73,    74,    -1,    76,    -1,   348,   349,   350,   351,   352,
     353,   364,   349,   350,   351,   352,   353,    -1,    -1,   199,
      -1,    -1,    -1,    12,   365,    -1,    15,   368,    -1,    -1,
      -1,   379,    -1,    -1,    -1,    -1,   348,   349,   350,   351,
     352,   353,   390,   391,   402,   398,   394,   390,   391,    -1,
     408,   394,    -1,    42,   395,    -1,    -1,    -1,   238,    -1,
      -1,    -1,   415,    -1,    43,    -1,    -1,    -1,    -1,    -1,
     423,    50,    -1,   421,    -1,   416,    -1,   425,   390,    68,
      59,   261,   394,    -1,    73,    74,   266,    76,    -1,   269,
     270,   271,   272,   273,   274,   275,   276,   277,   278,   279,
     280,   281,   282,   283,   284,    12,     1,     1,    15,     3,
       4,    90,    -1,    92,     8,    94,    95,    96,    97,    -1,
      15,    -1,    -1,    17,    -1,    -1,    -1,   106,   107,   108,
      -1,   110,    -1,    -1,    -1,    30,    30,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    -1,     1,    43,
      -1,    -1,    -1,    -1,    -1,    -1,    50,    -1,    -1,    53,
      -1,    68,    15,    -1,    -1,    59,    73,    74,    -1,    76,
      -1,    -1,    12,    68,    -1,    15,    -1,    30,    -1,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    -1,
      -1,    -1,    86,   373,   374,    89,    90,    -1,    92,    -1,
      94,    95,    96,    97,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,   107,   108,    68,   110,   397,     3,     4,
       5,     6,    -1,     8,     9,    10,    11,    -1,    68,   123,
      -1,    16,    17,    73,    74,    -1,    76,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    33,    34,
      64,    -1,    -1,    38,    39,    40,    41,     3,     4,     5,
       6,    -1,     8,     9,    10,    11,    -1,    52,    53,    -1,
      16,    17,    -1,    58,    -1,    -1,    -1,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    33,    34,    74,
      75,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    52,    53,    -1,     0,
       1,    -1,    58,    -1,    -1,    -1,     7,    63,    -1,    -1,
      -1,    -1,    13,    14,    -1,    -1,    17,    18,    19,    75,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,     7,    -1,
      -1,    -1,    -1,    -1,    65,    66,    67,    68,    17,    18,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      -1,    -1,    -1,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,     7,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      69,    17,    18,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    -1,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    -1,     7,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    68,    17,    18,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    -1,    -1,    -1,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,     7,
      -1,    -1,    15,    -1,    -1,    -1,    -1,    -1,    -1,    17,
      18,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    -1,
      -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      68,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    -1,    -1,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    -1,    -1,    64,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      -1,    -1,    64,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    -1,    -1,    64
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,    29,    30,    65,    78,    83,    84,    86,   114,
      15,   202,    87,   202,   202,     0,    79,     1,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    68,    91,
      92,    93,    94,    96,   196,   202,    85,    17,   194,    13,
      14,    66,    80,    82,   202,   114,    93,    95,   132,    75,
      88,   200,    16,     1,    94,   183,   184,   196,    59,    97,
      98,    99,   101,   103,   194,   187,   202,   196,    89,    90,
     194,   202,     7,    17,    18,    19,    21,    22,    23,    24,
      25,    26,    27,    28,    42,    67,    68,   127,   128,   129,
     133,   135,   137,   142,   150,   151,   152,   153,   154,   155,
     157,   159,   162,   168,   170,   173,   175,   176,   177,   179,
     181,   182,   188,   194,   195,    81,   194,   196,    74,   197,
      67,    42,   102,    31,   178,   183,    76,   201,   197,    72,
     200,   200,   200,   187,   171,    72,     3,     4,     5,     6,
      16,    17,    33,    34,    38,    39,    40,    41,    58,    63,
     115,   116,   117,   123,   188,   193,   200,    17,   130,   202,
     202,   196,   115,   116,   188,   196,   138,   196,   196,   196,
     196,    20,   196,   196,   196,   196,   134,   135,   189,   143,
      80,    97,    16,    17,    57,    58,   106,   107,   108,   109,
     110,   111,   112,   113,   100,    70,   202,   194,   187,   160,
     115,   187,   200,    16,   185,   186,   187,   200,   200,   200,
     200,   115,   115,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    64,   200,   115,
     132,   139,   158,    43,    44,    45,    46,    47,   136,   116,
     188,    70,   198,   200,   200,    42,   197,    16,    68,   197,
     106,    16,   104,   105,   108,   180,   115,   201,   133,   166,
     188,   187,    72,   197,   174,   118,   119,   120,    17,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   122,   201,   187,    12,    73,   141,
     187,   115,    16,    59,   117,   188,   190,   192,   144,   108,
     108,   107,   112,    69,   113,   197,    71,   197,    72,   183,
     201,   156,    68,   135,   115,   187,   185,   183,    17,   125,
     115,   124,   125,   197,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   115,   115,   115,   115,   115,   115,   115,
     126,   131,   187,   187,   140,   183,    71,   191,     8,     9,
      10,    11,    52,    53,   116,   145,   147,   148,   188,   194,
     197,   201,   105,   108,   161,   187,   187,   187,   172,   197,
     201,   197,   201,   121,   197,   201,    69,    17,   142,   197,
     116,   149,   194,   149,   149,   149,   149,   149,   199,   201,
     197,    42,    57,    58,   146,   187,   183,   163,   201,   183,
     125,   125,   124,   115,   190,   148,   116,   188,   147,   183,
     115,   167,   187,   201,   197,    68,   169,   125,   187,   183,
     201,   164,   166,   201,   187,   165,   127
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
#line 138 "grammar.y"
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
		}
    break;

  case 3:
#line 161 "grammar.y"
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
		}
    break;

  case 4:
#line 173 "grammar.y"
    {
		    if (parse_state == PARSE_PARAMS) {
			eprintf ("Illegal parser state.\n");
			EYYERROR;
		    }
		}
    break;

  case 5:
#line 179 "grammar.y"
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
#line 193 "grammar.y"
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
#line 204 "grammar.y"
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
#line 216 "grammar.y"
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
#line 258 "grammar.y"
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
#line 268 "grammar.y"
    {
		    d_d(); /* show dictionary/stack pointers */
		}
    break;

  case 13:
#line 271 "grammar.y"
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
#line 280 "grammar.y"
    {
		    d_stack (pc, 0);		/* show compiled code	*/
		}
    break;

  case 15:
#line 287 "grammar.y"
    { 
			/* Check for required params.
			 */
			if (!errcnt)
			    proc_params(n_procpar);
		}
    break;

  case 16:
#line 295 "grammar.y"
    {
			/* Initialize parser for procedure body.
			 */
			if (cldebug)
			    eprintf ("parse init (script_body)...\n");
ready_();

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
		}
    break;

  case 18:
#line 321 "grammar.y"
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
		}
    break;

  case 20:
#line 343 "grammar.y"
    {
		    n_procpar = 0;
		}
    break;

  case 22:
#line 352 "grammar.y"
    {
		    n_procpar = 0;
		}
    break;

  case 24:
#line 358 "grammar.y"
    { 
		    n_procpar = 1;
		    if (!errcnt)
			push (stkop((yyvsp[(1) - (1)])));
		}
    break;

  case 25:
#line 363 "grammar.y"
    {
		    n_procpar++;
		    if (!errcnt)
			push (stkop((yyvsp[(3) - (3)])));
		}
    break;

  case 32:
#line 380 "grammar.y"
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
#line 417 "grammar.y"
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
#line 438 "grammar.y"
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
#line 454 "grammar.y"
    { vartype = V_BOOL; }
    break;

  case 36:
#line 455 "grammar.y"
    { vartype = V_STRING; }
    break;

  case 37:
#line 456 "grammar.y"
    { vartype = V_REAL; }
    break;

  case 38:
#line 457 "grammar.y"
    { vartype = V_FILE; }
    break;

  case 39:
#line 458 "grammar.y"
    { vartype = V_GCUR; }
    break;

  case 40:
#line 459 "grammar.y"
    { vartype = V_IMCUR; }
    break;

  case 41:
#line 460 "grammar.y"
    { vartype = V_UKEY; }
    break;

  case 42:
#line 461 "grammar.y"
    { vartype = V_PSET; }
    break;

  case 43:
#line 462 "grammar.y"
    { vartype = V_INT; }
    break;

  case 44:
#line 463 "grammar.y"
    { vartype = V_STRUCT; }
    break;

  case 47:
#line 470 "grammar.y"
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
#line 488 "grammar.y"
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
#line 506 "grammar.y"
    {
			inited = NO;
			n_aval = 0;
		}
    break;

  case 50:
#line 510 "grammar.y"
    {
			n_aval = 0;
		}
    break;

  case 51:
#line 513 "grammar.y"
    {
			inited = YES;
		}
    break;

  case 52:
#line 518 "grammar.y"
    {
		    index_cnt = 0;
		    if (!errcnt)
			pp = initparam (stkop((yyvsp[(1) - (1)])), do_params, vartype, varlist);
		}
    break;

  case 53:
#line 523 "grammar.y"
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
#line 542 "grammar.y"
    {
			varlist = NO;
			index_cnt = 0;
		}
    break;

  case 56:
#line 546 "grammar.y"
    {
			if (!do_params) {
			    eprintf (locallist);
			    EYYERROR;
			}
			varlist = YES;
			index_cnt = 0;
			(yyval) = (yyvsp[(2) - (2)]);
		}
    break;

  case 60:
#line 566 "grammar.y"
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
#line 589 "grammar.y"
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
#line 610 "grammar.y"
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
#line 619 "grammar.y"
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
#line 646 "grammar.y"
    {
		      	if (stkop((yyvsp[(2) - (2)]))->o_type == OT_INT) {
			    stkop((yyvsp[(2) - (2)]))->o_val.v_i *= (yyvsp[(1) - (2)]);
			    (yyval) = (yyvsp[(2) - (2)]);
			} else if (stkop((yyvsp[(2) - (2)]))->o_type == OT_REAL) {
			    stkop((yyvsp[(2) - (2)]))->o_val.v_r *= (yyvsp[(1) - (2)]);
			    (yyval) = (yyvsp[(2) - (2)]);
			} else {
			    eprintf ("Invalid constant in declaration.\n");
			    EYYERROR;
			}
		}
    break;

  case 69:
#line 660 "grammar.y"
    { (yyval) =  1; }
    break;

  case 70:
#line 661 "grammar.y"
    { (yyval) = -1; }
    break;

  case 71:
#line 663 "grammar.y"
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
#line 673 "grammar.y"
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
#line 688 "grammar.y"
    {
			if (!errcnt)
			    if (pp != NULL)
			    	do_option (pp, stkop((yyvsp[(1) - (3)])), stkop((yyvsp[(3) - (3)])));
		}
    break;

  case 79:
#line 704 "grammar.y"
    {
		    if (!errcnt)
		        compile (PUSHPARAM, stkop((yyvsp[(1) - (1)]))->o_val.v_s);
		}
    break;

  case 81:
#line 718 "grammar.y"
    {
		    if  (!errcnt)
		        compile (PUSHCONST, stkop((yyvsp[(1) - (1)])));
		}
    break;

  case 82:
#line 722 "grammar.y"
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
#line 730 "grammar.y"
    {
		    if (!errcnt)
			compile (PUSHPARAM, "imcur");
		}
    break;

  case 84:
#line 734 "grammar.y"
    {
		    if (!errcnt)
			compile (PUSHPARAM, "ukey");
		}
    break;

  case 85:
#line 738 "grammar.y"
    {
		    if (!errcnt)
			compile (PUSHPARAM, "pset");
		}
    break;

  case 87:
#line 746 "grammar.y"
    {
		    if (!errcnt)
			compile (ADD);
		}
    break;

  case 88:
#line 750 "grammar.y"
    {
		    if (!errcnt)
			compile (SUB);
		}
    break;

  case 89:
#line 754 "grammar.y"
    {
		    if (!errcnt)
			compile (MUL);
		}
    break;

  case 90:
#line 758 "grammar.y"
    {
		    if (!errcnt)
			compile (DIV);
		}
    break;

  case 91:
#line 762 "grammar.y"
    {
		    if (!errcnt)
			compile (POW);
		}
    break;

  case 92:
#line 766 "grammar.y"
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
#line 775 "grammar.y"
    {
		    if (!errcnt)
			compile (CONCAT);
		}
    break;

  case 94:
#line 779 "grammar.y"
    {
		    if (!errcnt)
			compile (LT);
		}
    break;

  case 95:
#line 783 "grammar.y"
    {
		    if (!errcnt)
			compile (GT);
		}
    break;

  case 96:
#line 787 "grammar.y"
    {
		    if (!errcnt)
			compile (LE);
		}
    break;

  case 97:
#line 791 "grammar.y"
    {
		    if (!errcnt)
			compile (GE);
		}
    break;

  case 98:
#line 795 "grammar.y"
    {
		    if (!errcnt)
			compile (EQ);
		}
    break;

  case 99:
#line 799 "grammar.y"
    {
		    if (!errcnt)
			compile (NE);
		}
    break;

  case 100:
#line 803 "grammar.y"
    {
		    if (!errcnt)
			compile (OR);
		}
    break;

  case 101:
#line 807 "grammar.y"
    {
		    if (!errcnt)
			compile (AND);
		}
    break;

  case 102:
#line 811 "grammar.y"
    {
		    if (!errcnt)
			compile (NOT);
		}
    break;

  case 103:
#line 815 "grammar.y"
    {
		    if (!errcnt)
			compile (CHSIGN);
		}
    break;

  case 104:
#line 820 "grammar.y"
    {
		    /* Free format scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		}
    break;

  case 105:
#line 824 "grammar.y"
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
#line 833 "grammar.y"
    {
		    /* Formatted scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		}
    break;

  case 107:
#line 837 "grammar.y"
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
#line 850 "grammar.y"
    {
		    /* Free format scan from a parameter.  */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		}
    break;

  case 109:
#line 854 "grammar.y"
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
#line 864 "grammar.y"
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
#line 872 "grammar.y"
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
#line 885 "grammar.y"
    {
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		}
    break;

  case 113:
#line 888 "grammar.y"
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
#line 903 "grammar.y"
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
#line 911 "grammar.y"
    {
			if (!errcnt)
			    (yyval) = addconst ("real", OT_STRING);
		}
    break;

  case 117:
#line 917 "grammar.y"
    {
		    if (!errcnt) {
		        push (pop() + 1);		/* inc num args	*/
		    }
		}
    break;

  case 119:
#line 930 "grammar.y"
    {
                    if (!errcnt) {
                        compile (PUSHCONST, stkop ((yyvsp[(1) - (1)])));
                        push (pop() + 1);               /* inc num args */
                    }
		}
    break;

  case 120:
#line 936 "grammar.y"
    {
                    if (!errcnt) {
                        compile (PUSHCONST, stkop ((yyvsp[(1) - (3)])));
                        push (pop() + 1);               /* inc num args */
                    }
		}
    break;

  case 122:
#line 947 "grammar.y"
    {
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		}
    break;

  case 123:
#line 951 "grammar.y"
    {
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		}
    break;

  case 146:
#line 989 "grammar.y"
    {
		    bracelevel++;
		}
    break;

  case 147:
#line 991 "grammar.y"
    {
		    --bracelevel;
		}
    break;

  case 151:
#line 1003 "grammar.y"
    {
			--parenlevel;
 			if (!errcnt)
		      	    compile (ASSIGN, stkop((yyvsp[(1) - (3)]))->o_val.v_s);
		}
    break;

  case 152:
#line 1008 "grammar.y"
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

  case 153:
#line 1019 "grammar.y"
    {
			parenlevel++;
		}
    break;

  case 154:
#line 1022 "grammar.y"
    {
		      	--parenlevel;
			if (!errcnt)
		  	    compile ((yyvsp[(3) - (4)]), stkop((yyvsp[(1) - (4)]))->o_val.v_s);
		}
    break;

  case 155:
#line 1031 "grammar.y"
    {
			parenlevel++;
		}
    break;

  case 156:
#line 1036 "grammar.y"
    { (yyval) = ADDASSIGN; }
    break;

  case 157:
#line 1037 "grammar.y"
    { (yyval) = SUBASSIGN; }
    break;

  case 158:
#line 1038 "grammar.y"
    { (yyval) = MULASSIGN; }
    break;

  case 159:
#line 1039 "grammar.y"
    { (yyval) = DIVASSIGN; }
    break;

  case 160:
#line 1040 "grammar.y"
    { (yyval) = CATASSIGN; }
    break;

  case 161:
#line 1043 "grammar.y"
    {
		    npipes = 0;
		}
    break;

  case 162:
#line 1045 "grammar.y"
    {
		    if (!errcnt) {
			compile (EXEC);
			if (npipes > 0)
			    compile (RMPIPES, npipes);
		    }
		}
    break;

  case 164:
#line 1055 "grammar.y"
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

  case 165:
#line 1089 "grammar.y"
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

  case 166:
#line 1100 "grammar.y"
    {
		    (yyval) = 1;
		}
    break;

  case 167:
#line 1103 "grammar.y"
    {
		    (yyval) = 2;
		}
    break;

  case 168:
#line 1108 "grammar.y"
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

  case 169:
#line 1132 "grammar.y"
    {
		    inarglist = 1;
		}
    break;

  case 170:
#line 1134 "grammar.y"
    {
		    inarglist = 0;
		    parenlevel = 0;
		    scanstmt = 0;
		}
    break;

  case 171:
#line 1141 "grammar.y"
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

  case 176:
#line 1160 "grammar.y"
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

  case 177:
#line 1170 "grammar.y"
    {
		    if (absmode) {
			eprintf (posfirst);
			EYYERROR;
		    } else
			if (!errcnt)
			    compile (POSARGSET, posit++);
		}
    break;

  case 178:
#line 1178 "grammar.y"
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

  case 179:
#line 1224 "grammar.y"
    {
		    absmode++;
		    if (!errcnt)
			compile (ABSARGSET, stkop((yyvsp[(1) - (3)]))->o_val.v_s); 
		}
    break;

  case 180:
#line 1229 "grammar.y"
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

  case 181:
#line 1241 "grammar.y"
    {
		    absmode++;
		    if (!errcnt)
			compile (SWON, stkop((yyvsp[(1) - (2)]))->o_val.v_s);
		}
    break;

  case 182:
#line 1246 "grammar.y"
    {
		    absmode++;
		    if (!errcnt)
			compile (SWOFF, stkop((yyvsp[(1) - (2)]))->o_val.v_s);
		}
    break;

  case 183:
#line 1251 "grammar.y"
    {
		    if (!errcnt)
			compile (REDIRIN);
		}
    break;

  case 184:
#line 1255 "grammar.y"
    {
		    newstdout++;
		    if (!errcnt)
			compile (REDIR);
		}
    break;

  case 185:
#line 1260 "grammar.y"
    {
		    newstdout++;
		    if (!errcnt)
			compile (ALLREDIR);
		}
    break;

  case 186:
#line 1265 "grammar.y"
    {
		    newstdout++;
		    if (!errcnt)
			compile (APPENDOUT);
		}
    break;

  case 187:
#line 1270 "grammar.y"
    {
		    newstdout++;
		    if (!errcnt)
			compile (ALLAPPEND);
		}
    break;

  case 188:
#line 1275 "grammar.y"
    {
		    if (!errcnt)
			compile (GSREDIR, stkop((yyvsp[(1) - (2)]))->o_val.v_s);
		}
    break;

  case 189:
#line 1281 "grammar.y"
    {
		    absmode++;
		    /* constant already pushed by expr0.
		     */
		}
    break;

  case 190:
#line 1286 "grammar.y"
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

  case 191:
#line 1297 "grammar.y"
    {
			--parenlevel;
			if (!errcnt)
			    compile (IMMED);
		}
    break;

  case 192:
#line 1302 "grammar.y"
    {
		      	--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop((yyvsp[(2) - (2)]))->o_val.v_s);
		}
    break;

  case 193:
#line 1309 "grammar.y"
    {
			--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop((yyvsp[(1) - (2)]))->o_val.v_s);
		}
    break;

  case 194:
#line 1316 "grammar.y"
    {
		    if (!errcnt)
			compile (OSESC, stkop((yyvsp[(1) - (1)]))->o_val.v_s);
		}
    break;

  case 195:
#line 1322 "grammar.y"
    {
		    --parenlevel;
		    if (!errcnt)
			compile (IMMED);
		}
    break;

  case 196:
#line 1329 "grammar.y"
    {
		    /* pop BIFF addr and set branch to just after statement
		     */
		    XINT   biffaddr;
		    if (!errcnt) {
			biffaddr = pop();
		    	coderef (biffaddr)->c_args = pc - biffaddr - 3;
		    }
		}
    break;

  case 197:
#line 1340 "grammar.y"
    {
			/* save BIFF addr so branch can be filled in 
			 */
			if (!errcnt)
			    push (compile (BIFF, 0));
		}
    break;

  case 198:
#line 1345 "grammar.y"
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

  case 199:
#line 1373 "grammar.y"
    {
		    XINT  biffaddr;

		    ifseen = NULL;
		    if (!errcnt) {
			/* Pop and save BIFF address, compile and push addr 
			 * of GOTO, and set BIFF branch to just after GOTO.
			 */
			biffaddr = pop();
			push (compile (GOTO, 0));
			coderef (biffaddr)->c_args = pc - biffaddr - 3;
		    }
		}
    break;

  case 200:
#line 1385 "grammar.y"
    {
		    XINT  gotoaddr;
		    if (!errcnt) {
			/* Pop GOTO addr and set branch to just after statement
			 */
			gotoaddr = pop();
			coderef (gotoaddr)->c_args = pc - gotoaddr - 3;
		    }
		}
    break;

  case 201:
#line 1396 "grammar.y"
    {
		    /* Save starting addr of while expression.
		     */
		    if (!errcnt) {
			push (pc);
			loopincr();
		    }
		}
    break;

  case 202:
#line 1403 "grammar.y"
    {
		    /* Save BIFF addr so branch can be filled in.
		     */
		    if (!errcnt)
			push (compile (BIFF, 0));
		}
    break;

  case 203:
#line 1408 "grammar.y"
    {
		    XINT  biffaddr;

		    if (!errcnt) {
			/* Pop and save addr of BIFF instruction.	   */
			biffaddr = pop();
			/* Pop addr of expression and build a goto there.  */
			compile (GOTO, pop() - pc - 3);
			/* Now can set BIFF branch to just after statement.*/
			coderef (biffaddr)->c_args = pc - biffaddr - 3;
			loopdecr();
		    }
		}
    break;

  case 204:
#line 1441 "grammar.y"
    {
			if (!errcnt)
			    push(pc);				/* Loop1: */
		}
    break;

  case 205:
#line 1445 "grammar.y"
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
		}
    break;

  case 206:
#line 1461 "grammar.y"
    {
			XINT  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-3); 	/* Goto loop1 */
			    stmtaddr = pop();
			    coderef(stmtaddr)->c_args = pc - stmtaddr - 3;
			}
		}
    break;

  case 207:
#line 1471 "grammar.y"
    {
			XINT  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-3); /* goto loop2 */

			    if (for_expr) {
				stmtaddr = pop();
				coderef(stmtaddr)->c_args = pc - stmtaddr - 3;
			    }
			    loopdecr();
			}
		}
    break;

  case 210:
#line 1494 "grammar.y"
    {
			for_expr = YES;
		}
    break;

  case 211:
#line 1497 "grammar.y"
    {
			for_expr = NO;
		}
    break;

  case 212:
#line 1523 "grammar.y"
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

  case 213:
#line 1532 "grammar.y"
    {
			/* Set up jumptable and pop space on stack.
			 */
			if (!errcnt)
			    setswitch();
		}
    break;

  case 214:
#line 1540 "grammar.y"
    {
			if (!errcnt) {
			    ncaseval = 0;
			    if (!in_switch()) {
				eprintf ("Improper CASE statement.\n");
				EYYERROR;
			    }
			}
		}
    break;

  case 215:
#line 1548 "grammar.y"
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

  case 216:
#line 1559 "grammar.y"
    {
			/* Branch to end of switch block 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		}
    break;

  case 217:
#line 1567 "grammar.y"
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
		}
    break;

  case 218:
#line 1577 "grammar.y"
    {
		      	/* Branch past jump table. 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		}
    break;

  case 219:
#line 1585 "grammar.y"
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
		}
    break;

  case 220:
#line 1600 "grammar.y"
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
		}
    break;

  case 221:
#line 1624 "grammar.y"
    {
			if (!errcnt)
			    compile (END);
		}
    break;

  case 222:
#line 1628 "grammar.y"
    {
			/* Return values currently not implemented.
			 */
			eprintf ("Warning: return value ignored.\n");
			if (!errcnt)
			    compile (END);
		}
    break;

  case 223:
#line 1640 "grammar.y"
    {
			bracelevel -= PBRACE;
			if (bracelevel < 0) {
			    eprintf ("Too few left braces.\n");
			    EYYERROR;
			} else if (bracelevel > 0) {
			    eprintf ("Too few right braces.\n");
			    EYYERROR;
			}
		}
    break;

  case 224:
#line 1652 "grammar.y"
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
			        eprintf ("Identical labels.\n");
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
				coderef(gotopc)->c_args = pc - gotopc - 3;
			    }
			    (l->l_defined)++;
			}
		}
    break;

  case 226:
#line 1686 "grammar.y"
    {
			/* Get the address corresponding to the label.
			 */
			struct label *l;

			if (!errcnt) {
			    l = getlabel (stkop((yyvsp[(2) - (2)])));

			    if (l != NULL)
				compile (GOTO, l->l_loc - pc - 3);
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

  case 229:
#line 1716 "grammar.y"
    { 
			/* Save pc before compiling statement for loop back
			 */
			stmt_pc = pc;
			n_oarr = 0;
			i_oarr = 0;
			ifseen = NULL;
		}
    break;

  case 230:
#line 1724 "grammar.y"
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
		}
    break;

  case 232:
#line 1763 "grammar.y"
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
		}
    break;

  case 235:
#line 1823 "grammar.y"
    {
				if (!errcnt) {
				    push(stkop((yyvsp[(1) - (1)]))) ; 
				    ncaseval++;
				}
			}
    break;

  case 238:
#line 1843 "grammar.y"
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

  case 239:
#line 1880 "grammar.y"
    {
		    if (!errcnt) {
			strncpy (curr_param, stkop((yyvsp[(1) - (1)]))->o_val.v_s, SZ_FNAME);
			index_cnt = 0;
		    }
		}
    break;

  case 240:
#line 1887 "grammar.y"
    {
		    if (i_oarr > 0  &&  n_oarr == 0)
			n_oarr = i_oarr;
		    i_oarr = 0;
		    lastref = YES;
		}
    break;

  case 241:
#line 1895 "grammar.y"
    {
			index_cnt = 1;
		}
    break;

  case 242:
#line 1898 "grammar.y"
    {
			index_cnt++;
		}
    break;

  case 244:
#line 1904 "grammar.y"
    {
			if (!errcnt)
			    compile (PUSHINDEX, 0);
		}
    break;

  case 245:
#line 1909 "grammar.y"
    { 
			if (!errcnt) {
			    compile (PUSHPARAM, stkop((yyvsp[(1) - (1)]))->o_val.v_s);
			    compile (PUSHINDEX, 0);
			}
		}
    break;

  case 246:
#line 1915 "grammar.y"
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
		}
    break;

  case 247:
#line 1936 "grammar.y"
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

  case 248:
#line 1970 "grammar.y"
    {
		    (yyval) = (yyvsp[(1) - (1)]);
		}
    break;

  case 249:
#line 1975 "grammar.y"
    {
		    (yyval) = (yyvsp[(1) - (1)]);
		}
    break;

  case 250:
#line 1980 "grammar.y"
    {
		    (yyval) = (yyvsp[(1) - (1)]);
		}
    break;

  case 252:
#line 1986 "grammar.y"
    {
		    /* If statements are delimited by ';'s, do not execute
		     * until next newline EOST is received.
		     */
		    sawnl = 0;
		}
    break;

  case 258:
#line 2008 "grammar.y"
    { parenlevel++; }
    break;

  case 259:
#line 2011 "grammar.y"
    { --parenlevel; }
    break;

  case 260:
#line 2014 "grammar.y"
    { sawnl = 1; }
    break;


/* Line 1267 of yacc.c.  */
#line 4294 "y.tab.c"
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


#line 2017 "grammar.y"


#include "lexyy.c"
#include "lexicon.c"

