/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */


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
# define Y_FSCAN 258
# define Y_OSESC 259
# define Y_APPEND 260
# define Y_ALLAPPEND 261
# define Y_ALLREDIR 262
# define Y_GSREDIR 263
# define Y_ALLPIPE 264
# define D_D 265
# define D_PEEK 266
# define Y_NEWLINE 267
# define Y_CONSTANT 268
# define Y_IDENT 269
# define Y_WHILE 270
# define Y_IF 271
# define Y_ELSE 272
# define Y_FOR 273
# define Y_BREAK 274
# define Y_NEXT 275
# define Y_SWITCH 276
# define Y_CASE 277
# define Y_DEFAULT 278
# define Y_RETURN 279
# define Y_GOTO 280
# define Y_PROCEDURE 281
# define Y_BEGIN 282
# define Y_END 283
# define Y_BOOL 284
# define Y_INT 285
# define Y_REAL 286
# define Y_STRING 287
# define Y_FILE 288
# define Y_STRUCT 289
# define Y_GCUR 290
# define Y_IMCUR 291
# define Y_UKEY 292
# define Y_PSET 293
# define YOP_AOADD 294
# define YOP_AOSUB 295
# define YOP_AOMUL 296
# define YOP_AODIV 297
# define YOP_AOCAT 298
# define YOP_OR 299
# define YOP_AND 300
# define YOP_EQ 301
# define YOP_NE 302
# define YOP_LE 303
# define YOP_GE 304
# define YOP_CONCAT 305
# define YOP_POW 306
# define YOP_NOT 307
# define UMINUS 308
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 1936 "grammar.y"


#include "lexyy.c"
#include "lexicon.c"
short yyexca[] ={
-1, 1,
	0, -1,
	256, 3,
	259, 3,
	265, 3,
	266, 3,
	269, 3,
	270, 3,
	271, 3,
	273, 3,
	274, 3,
	275, 3,
	276, 3,
	277, 3,
	278, 3,
	279, 3,
	280, 3,
	284, 3,
	285, 3,
	286, 3,
	287, 3,
	288, 3,
	289, 3,
	290, 3,
	291, 3,
	292, 3,
	293, 3,
	61, 3,
	126, 3,
	123, 3,
	59, 3,
	-2, 0,
-1, 58,
	91, 52,
	-2, 51,
-1, 102,
	61, 243,
	294, 243,
	295, 243,
	296, 243,
	297, 243,
	298, 243,
	91, 243,
	-2, 244,
-1, 105,
	91, 233,
	-2, 232,
-1, 139,
	267, 185,
	59, 185,
	-2, 77,
-1, 140,
	267, 186,
	59, 186,
	-2, 78,
-1, 154,
	40, 242,
	-2, 243,
-1, 194,
	267, 145,
	59, 145,
	41, 145,
	-2, 77,
-1, 195,
	267, 146,
	59, 146,
	41, 146,
	-2, 78,
-1, 283,
	125, 141,
	-2, 223,
-1, 285,
	44, 236,
	-2, 235,
-1, 286,
	93, 238,
	44, 238,
	-2, 79,
-1, 287,
	93, 239,
	44, 239,
	-2, 78,
-1, 289,
	93, 241,
	44, 241,
	-2, 80,
-1, 341,
	264, 171,
	267, 171,
	59, 171,
	124, 171,
	44, 171,
	41, 171,
	-2, 77,
-1, 342,
	264, 172,
	267, 172,
	59, 172,
	124, 172,
	44, 172,
	41, 172,
	-2, 78,
-1, 343,
	91, 233,
	-2, 232,
-1, 375,
	264, 183,
	267, 183,
	59, 183,
	124, 183,
	44, 183,
	41, 183,
	-2, 77,
-1, 376,
	264, 184,
	267, 184,
	59, 184,
	91, 233,
	124, 184,
	44, 184,
	41, 184,
	-2, 232,
-1, 392,
	264, 173,
	267, 173,
	59, 173,
	124, 173,
	44, 173,
	41, 173,
	-2, 77,
-1, 393,
	264, 174,
	267, 174,
	59, 174,
	124, 174,
	44, 174,
	41, 174,
	-2, 78,
	};
# define YYNPROD 255
# define YYLAST 801
short yyact[]={

  61,  66, 141, 167, 284, 148,  68, 119, 275, 340,
 320, 374, 240, 208, 339,   9,  89, 191, 209, 158,
 246,  47, 321, 206, 204,  60, 205,  51, 207, 209,
 355, 183, 164, 209, 206, 204, 230, 205, 206, 207,
 243,   2,  53, 207,  48, 197, 198, 199, 200, 201,
 228,  12,  40,  88, 120,  23,  31,  25,  24,  26,
  32,  27,  28,  29,  30,  45, 180, 177, 112, 365,
  90, 292, 186, 114, 187,  21,  54,  36, 209, 113,
  51, 335, 252, 206, 204, 105, 205, 371, 207, 178,
 234, 189, 166, 107, 140, 242, 161, 115, 399, 179,
 301, 211, 186, 212, 187, 327, 238, 165,  23,  31,
  25,  24,  26,  32,  27,  28,  29,  30, 279, 168,
  46, 163, 372, 192, 373, 113, 113, 192, 299,  65,
  59,  50,  16, 336,   7, 285, 193, 171,  18, 186,
 195, 187, 229, 181,  49, 282, 117,  18,  42,  41,
 124, 281, 364, 203, 162, 219, 220, 209, 400, 136,
 407, 226, 206, 204, 231, 205, 403, 207, 396, 232,
 386, 361, 225, 224, 350,  92, 369, 367, 337, 237,
 211, 290, 212, 139, 235, 172, 106, 302, 174, 175,
 239,  38,  39, 251, 202, 138, 196, 137,  59, 334,
 170,  87, 250,  86,  85, 255, 256, 257, 258, 259,
 260, 261, 262, 263, 264, 265, 266, 267, 268, 269,
  84, 254, 253,  83,  82, 273,  81,  80, 278, 194,
 277, 274,  79, 276, 283,  78, 233, 286, 287,  77,
  76,  75, 280,  65,  74, 288,  73,  72, 150,  21,
  71,   5,  47, 303, 304, 291, 244, 298, 300,  70,
  69, 305, 306, 307, 308, 309, 310, 311, 312, 313,
 314, 315, 316, 317, 318, 319,   8,   9, 324, 249,
 330, 325, 322,  12, 329,  67, 210, 208, 155, 332,
 333,  65, 326, 323, 342, 113, 150, 248, 208, 272,
 209, 153, 208, 338,  65, 206, 204, 295, 205, 150,
 207, 344, 209, 345, 351, 271, 192, 206, 204, 354,
 205, 352, 207, 211, 344, 212, 345, 182, 362, 294,
 363, 331, 195, 247, 357, 211, 360, 212, 356, 293,
 358, 359, 215, 216, 213, 214, 210, 208, 328, 366,
 270, 382, 370, 185, 184, 368, 245, 377, 378, 379,
 380, 381, 385, 116, 182, 183, 384,  58, 383, 286,
 287, 389, 388, 342, 342, 393, 343,  65, 188,  57,
 391, 387, 150, 341, 390, 296, 297, 176, 398,  56,
 394, 209, 397, 395,  55,  44, 206, 204, 293, 205,
 401, 207,  22, 370,  15, 123, 406, 277, 402, 408,
 276,  88, 404, 122, 211,  63, 212,  34, 118,  33,
  14, 194, 405, 213, 214, 210, 208,   6, 110,  37,
 376, 376, 376, 376, 376, 376,   4, 375, 375, 375,
 375, 375, 375, 209, 247, 353,  19,   3, 206, 204,
  10, 205, 103, 207, 107, 343, 343,  12,   1,   0,
 151, 152, 341, 341, 392,   0, 211,   0, 212,   0,
   0, 289, 154,   0,  23,  31,  25,  24,  26,  32,
  27,  28,  29,  30,   0,   0,   0,   0, 156, 157,
   0,   0,   0, 143, 144, 145, 146,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0, 151, 152,
 149, 347, 348, 346, 349,   0, 104,   0,   0, 142,
 154, 151, 152,   0, 347, 348, 346, 349,   0,   0,
  12,   0, 142, 154,   0,   0, 156, 157,   0,   0,
 147, 143, 144, 145, 146,   0,   0,   0,   0, 156,
 157,   0,   0,   0, 143, 144, 145, 146, 149,   0,
   0,   0, 217, 218, 215, 216, 213, 214, 210, 208,
   0, 149,   0,   0, 217, 218, 215, 216, 213, 214,
 210, 208,   0,   0,   0,   0,   0,  64,   0,   0,
   0,   0,   0,   0, 151, 152,   0,   0,   0,   0,
   0,   0,   0,   0,   0, 142, 154,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0, 156, 157,   0,   0,   0, 143, 144, 145,
 146,   0,   0,   0, 159, 160,   0,   0,   0,   0,
   0,   0,   0,   0, 149,   0,   0,   0,   0, 173,
   0,   0,  91, 217, 218, 215, 216, 213, 214, 210,
 208,   0, 102,  93, 108,   0,  94,  99,  98,  95,
  96,  97, 101, 100,   0,   0,   0,   0,   0,  62,
   0,   0,  11,   0,   0,  13,  20,   0,   0,  35,
   0,   0, 221, 222, 223,  20,   0,   0,   0,  43,
   0,   0, 227,   0,  17,   0, 218, 215, 216, 213,
 214, 210, 208, 236,   0,   0,   0,  20,   0,   0,
   0,   0,   0, 241,   0,   0,   0,   0,   0,   0,
   0, 109,   0,   0,  20,   0,   0,   0,   0,   0,
   0,   0,  52,  20,   0,   0,   0,   0,  20,  20,
  20,  20,  20,  20,   0,   0,   0,   0,   0, 111,
   0,  20,  20,  20,  20,   0,   0, 135, 121,   0,
   0,   0,   0, 125, 126, 127, 128, 129, 130,   0,
   0,   0,   0, 169,   0,   0, 131, 132, 133, 134,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
 190 };
short yypact[]={

  -5,-1000,-216,-1000,-1000,-216, 190,-1000,-1000,-216,
 -74,-1000,-1000,-1000,-267, 190,-1000,-1000,-1000,-216,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-248,-1000,-176,  16,-1000,-226,
-1000,-1000,-1000,-1000, -17,-216,  89,-1000,-1000, 393,
-1000,-216,-1000,-1000,  16,  81, -50,  36,-1000,-1000,
-248,-229,-1000,  16,-248,-1000,-1000,-1000,  16,  16,
  16,  16,  16,  16,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,  16,  16,  16,  16,-1000,-1000,-216,  32,-1000,
 337,-1000,-253,  89,  89,-216,-1000,  63,-1000,-1000,
-237, 337,  61,-216,-1000,-1000,-1000,-1000,  89,-1000,
 -74,-1000, -17,-1000,  96,-1000,   0,-1000,-1000,-1000,
-216,-1000,  86,  81,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000, 337,-249,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000, 337, 354, 337,
 337,  89,  89,  89,-1000,-1000,-1000,-1000,-1000,-1000,
-216,  89,-232,-216,-1000, 354,-1000,-1000,-216,-1000,
-1000,  -1,  89, 337,-1000,-1000,  47,  81,  81,-1000,
-1000,-1000,  89,  34,-1000,-228,-1000,-1000,  59,  29,
-1000,-1000,-1000,-248,-1000,-1000, 337,-1000,-1000,-1000,
-1000,-1000, -42, 275,-216,-216,-216,-216,-216,-216,
-216,-216,-216,-216,-216,-216,-216,-216,-216,-1000,
-1000,-1000,-1000,-1000,-216, 337,-248,-216,  60,  81,
-1000,-1000,-1000,-216, 203,-1000,-1000, 275, -54,  96,
-238,  29,  29,-1000,  81,  35,-1000,  42,-1000,-1000,
 354,-1000,-216,-216,-1000, 337, 337, 337, 337, 337,
 337, 337, 337, 337, 337, 337, 337, 337, 337, 337,
-247,-247, 337,-176, 275,  46,-1000,  32, 337,-216,
-232,-176,-176,-176, -12,-1000,-1000,-1000,-1000,-1000,
 251,-1000,-1000,-1000,  81,-1000,  86,-1000,  59,-1000,
  29,  29,-239,-1000,-1000,  -4,  -4,-293,-293,-1000,
-293,  -8, -19, -19, -19, -19, 120, 120, 406,  41,
  86,  81,  86,  82, 354,-1000,-1000,-216, 337, 263,
-1000,-1000,-1000,-1000, -56,-1000,  81,  86,-1000,  81,
-1000,-1000,  26,  79, 337, 337, 337, 337, 337, 337,
-216,-1000,-1000,-1000,-1000,-1000,-1000,-247,-1000,-1000,
 337,-216,-1000,  86,-176,-1000, 203,-1000,-1000, 264,
 264, 337,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-176,-1000, 354,-176, 337,-216,-1000,-1000,
  81,-1000,-1000,-1000,-1000,-1000,  39, 354,-1000,-216,
-176,-1000,-1000,-248,  86,-216,-1000, 393,-1000 };
short yypgo[]={

   0, 458, 679, 450,  77,   7, 447, 436, 429, 704,
 428, 427, 420, 134, 419,  65,   0, 418, 417,  85,
 415, 540, 413,  17, 405,  12, 404, 132, 131, 402,
 395,  76, 394, 389, 387, 379, 378,  67, 367, 363,
 356,  20, 143,  99, 354, 353,  89,  66,   5,  92,
   3,   2, 350,  10, 315, 301, 299, 293, 288,   1,
 285,   6, 260, 259, 250, 247, 246, 244, 241, 240,
 239, 235, 232, 227, 226, 224, 223, 220, 204, 203,
 201, 200, 199,  70, 197, 196,  16, 195, 194, 193,
 187, 186, 185, 184, 181, 178, 177, 176,  14,   9,
  11, 175, 174, 173, 172, 171,   8, 170, 168, 166,
 160, 158, 154,  50, 152, 151, 145, 144, 142, 137,
   4, 135, 133 };
short yyr1[]={

   0,   1,   1,   3,   1,   1,   1,   1,   4,  10,
   4,   8,   8,   8,   6,  14,   7,  18,  11,  20,
  20,  22,  22,  24,  24,  12,  12,  26,  26,  27,
  27,  27,  30,  28,  29,  29,  29,  29,  29,  29,
  29,  29,  29,  29,  31,  31,  32,  32,  33,  36,
  33,  35,  39,  35,  38,  38,  40,  40,  40,  41,
  41,  37,  37,  43,  43,  42,  42,  44,  45,  45,
  34,  34,  34,  46,  46,  47,  13,  48,  48,  49,
  49,  49,  49,  49,  49,  51,  51,  51,  51,  51,
  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,
  51,  51,  51,  52,  51,  54,  51,  56,  51,  55,
  55,  55,  53,  53,  53,  57,  57,  57,  59,  59,
  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,
  59,  59,  59,  59,  59,  59,  59,  59,  60,  60,
  81,  82,  80,  15,  15,  61,  61,  84,  61,  83,
  85,  85,  85,  85,  85,  87,  62,  88,  90,  88,
  89,  89,  92,  94,  86,  97,  95,  95,  98,  98,
  99,  99,  99,  99,  99,  99,  99,  99,  99,  99,
  99,  99,  99, 100, 100,  63,  63,  64,  65,  66,
  67, 102, 101, 103,  68, 104, 105,  69, 107, 109,
 110,  70, 106, 106, 108, 108, 111,  71, 112, 114,
  72, 115,  73,  74,  75,  77,  77,  17, 116,  78,
  76,  79,  79, 117,   5,   5,   5, 113, 113, 118,
  16,  16,  50, 119,  50, 120, 122, 120, 121, 121,
 121, 121,  58,  19,  91,   9,   9,  25,  93,  93,
  96,  96,  21,  23,   2 };
short yyr2[]={

   0,   0,   2,   0,   4,   1,   1,   2,   0,   0,
   4,   1,   2,   1,   3,   0,   5,   0,   5,   0,
   3,   0,   1,   1,   3,   0,   1,   1,   2,   1,
   1,   2,   0,   4,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   3,   1,   5,   1,   0,
   4,   1,   0,   5,   1,   2,   0,   1,   3,   1,
   3,   1,   3,   1,   4,   1,   1,   2,   1,   1,
   3,   1,   1,   1,   3,   3,   2,   1,   1,   1,
   1,   1,   1,   1,   1,   3,   4,   4,   4,   4,
   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,
   4,   2,   2,   0,   5,   0,   5,   0,   5,   1,
   1,   1,   0,   1,   3,   0,   1,   3,   1,   2,
   2,   2,   2,   2,   2,   1,   1,   1,   1,   1,
   1,   1,   2,   2,   2,   2,   1,   1,   1,   2,
   0,   0,   6,   0,   3,   3,   3,   0,   4,   1,
   1,   1,   1,   1,   1,   0,   3,   0,   0,   4,
   2,   2,   0,   0,   6,   0,   3,   1,   1,   3,
   0,   1,   1,   3,   3,   2,   2,   2,   2,   2,
   2,   2,   2,   1,   1,   2,   2,   2,   1,   1,
   1,   0,   7,   0,   5,   0,   0,   8,   0,   0,
   0,  16,   1,   0,   1,   0,   0,  10,   0,   0,
   7,   0,   5,   1,   1,   1,   2,   2,   0,   5,
   2,   1,   2,   0,   2,   1,   2,   1,   3,   1,
   0,   1,   1,   0,   5,   1,   0,   4,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   0,   1,
   0,   1,   1,   1,   1 };
short yychk[]={

-1000,  -1,  46,  -6,  -7, 256, -11, -13, 281, 282,
  -3,  -2, 267,  -2, -12, -26, -27,  -9, -28, 256,
  -2,  59, -29, 284, 287, 286, 288, 290, 291, 292,
 293, 285, 289, -14, -18,  -2,  -4,  -8, 265, 266,
 126, -13, -27,  -2, -30, -15, -19, 269,  -5,-117,
 -28, 256,  -9, 268, -31, -32, -33, -35, -38, -19,
  42, -16,  -2, -20, -21,  40, -59, -60, -61, -62,
 -63, -64, -65, -66, -67, -68, -69, -70, -71, -72,
 -73, -74, -75, -76, -77, -78, -79, -80, -50, -86,
 -83, 259,-101, 270, 273, 276, 277, 278, 275, 274,
 280, 279, 269,  59, 123, -19, -91,  61, 271,  -2,
 -10,  -9, -25,  44, 123,  61, -39, -19, -17,  -5,
 283,  -9, -22, -24, -19,  -9,  -9,  -9,  -9,  -9,
  -9,  -9,  -9,  -9,  -9,  -2, -83, -84, -87, -49,
 -50, -51, 268, 290, 291, 292, 293, -21, -48, 307,
  45, 257, 258, -55, 269, -58, 285, 286, 272, -21,
 -21, -16,-112,  58, 269, -48, -49, -50,  58,  -2,
 -81,-119, -92, -21,  -4, -31, -34, -37, -46, -43,
 -47, -42, 268, 269, -44, -45,  43,  45, -36,  91,
  -2, -23,  41, -25, -49, -50, -85, 294, 295, 296,
 297, 298, -88, -48,  43,  45,  42,  47, 306,  37,
 305,  60,  62, 303, 304, 301, 302, 299, 300, -48,
 -48, -21, -21, -21,-103,-104, -16, -21,-113,-118,
 268, -16, -16, -15,  91, -93, -21, -48,  59, -25,
 -25, -21,  61, 268, -37, -40, -41, -42, 268, -19,
 -48, -89, 124, 264, -23, -16, -16, -16, -16, -16,
 -16, -16, -16, -16, -16, -16, -16, -16, -16, -16,
 -52, -54, -56, -16, -48,-106, -61, -50, -16,  58,
 -25,-115,-116, -16,-120,-121, -51, -50,  42, 268,
 -94, -23, 125, -43, -46, -47, -42, -42, -25,  93,
 -25,  58, -90, -16, -16, -48, -48, -48, -48, -48,
 -48, -48, -48, -48, -48, -48, -48, -48, -48, -48,
 -53, 269, -53, -57, -48,  -5, -23,  59, -83, -48,
 -16,-113,  -5,  -5, -82,  93,-122, -95, -25, -98,
 -99, -49, -50, -19,  60,  62, 262, 260, 261, 263,
-102, -23, -41, -42, -86, 269, -23, -25, -23, -23,
 -25,-105, -16, -16,-114, 125, -25, -96, -23, -97,
 -25,  61,  43,  45,-100, -49, -19,-100,-100,-100,
-100,-100, -16, -53, -48, -16,-107, -23,  -5,-120,
 -98, -99, -49, -50,  -5,  -5,-108, -48, -16,  59,
-111, -16,  -5,-109,-106, -23, -16,-110, -59 };
short yydef[]={

   1,  -2,   0,   5,   6,   0,  25,  15,  17,   0,
   8,   2, 254,   7,   0,  26,  27,  29,  30,   0,
 245, 246,  32,  34,  35,  36,  37,  38,  39,  40,
  41,  42,  43, 143,   0,  76, 223,   0,  11,   0,
  13,  14,  28,  31,   0, 230,  19, 243,   4,   0,
 225,   0,   9,  12,   0,  44,  46,  48,  -2,  54,
   0, 223, 231,   0,  21, 252, 224, 118,   0,   0,
   0,   0,   0,   0, 125, 126, 127, 128, 129, 130,
 131,   0,   0,   0,   0, 136, 137, 138, 147, 155,
 189, 188, 190,   0,   0, 230, 208,   0, 213, 214,
   0, 215,  -2, 221, 140,  -2, 162, 149,   0, 226,
   8,  33,   0, 247,   0,  49,   0,  55,  16, 144,
   0,  18,   0,  22,  23, 119, 120, 121, 122, 123,
 124, 132, 133, 134, 135, 139, 187,   0, 157,  -2,
  -2,  79,  80,  81,  82,  83,  84,   0,   0,   0,
   0,   0,   0,   0,  -2, 109, 110, 111, 193, 195,
 230,   0,   0, 230, 220, 216,  77,  78, 230, 222,
 143,   0, 248,   0,  10,  45,   0,  71,  72,  61,
  73,  63,  65,   0,  66,   0,  68,  69,   0,  56,
 217,  20, 253,   0,  -2,  -2,   0, 150, 151, 152,
 153, 154, 156,   0, 230, 230, 230, 230, 230, 230,
 230, 230, 230, 230, 230, 230, 230, 230, 230, 101,
 102, 103, 105, 107, 230,   0, 203, 230,   0, 227,
 229, 211, 218, 230,   0, 163, 249,   0,   0,   0,
   0,   0,   0,  67,  50,   0,  57,  59,  65,  24,
 148, 158, 230, 230,  85,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
 112, 112, 115, 223,   0,   0, 202, 147,   0, 230,
   0, 223, 223,  -2,   0,  -2,  -2,  -2, 240,  -2,
 170, 191,  47,  62,  70,  74,   0,  75,   0,  53,
   0,   0,   0, 160, 161,  86,  87,  88,  89,  90,
  91,  92,  93,  94,  95,  96,  97,  98,  99, 100,
   0, 113,   0,   0, 116, 194, 196, 230,   0, 230,
 209, 228, 212, 219,   0, 234,   0, 250, 165, 167,
 168,  -2,  -2,  -2,   0,   0,   0,   0,   0,   0,
 230,  64,  58,  60, 159, 244, 104, 112, 106, 108,
   0, 230, 198,   0, 223, 142,   0, 164, 251, 170,
 170,   0, 175, 176, 177,  -2,  -2, 178, 179, 180,
 181, 182, 223, 114, 117, 223, 205, 230, 210, 237,
 166, 169,  -2,  -2, 192, 197,   0, 204, 206, 230,
 223, 199, 207, 203,   0, 230, 200,   0, 201 };
#ifndef lint
static	char yaccpar_sccsid[] = "@(#)yaccpar 1.5 86/08/27 SMI"; /* from UCB 4.1 83/02/11 */
#endif

#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps>= &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			yyerror( "syntax error" );
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 1:
# line 136 "grammar.y"
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
# line 159 "grammar.y"
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
# line 171 "grammar.y"
{
		    if (parse_state == PARSE_PARAMS) {
			eprintf ("Illegal parser state.\n");
			EYYERROR;
		    }
		} break;
case 4:
# line 177 "grammar.y"
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
# line 191 "grammar.y"
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
# line 202 "grammar.y"
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
# line 214 "grammar.y"
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
# line 256 "grammar.y"
{
		    /* debug are those debugging functions that
		     * should be run directly and not through a
		     * builtin task due to stack or other changes,
		     * ie, don't change what we are trying to show.
		     */
		    printf ("\n");
		} break;
case 11:
# line 266 "grammar.y"
{
		    d_d(); /* show dictionary/stack pointers */
		} break;
case 12:
# line 269 "grammar.y"
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
# line 278 "grammar.y"
{
		    d_stack (pc, 0);		/* show compiled code	*/
		} break;
case 14:
# line 285 "grammar.y"
{ 
			/* Check for required params.
			 */
			if (!errcnt)
			    proc_params(n_procpar);
		} break;
case 15:
# line 293 "grammar.y"
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
# line 318 "grammar.y"
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
# line 340 "grammar.y"
{
		    n_procpar = 0;
		} break;
case 21:
# line 349 "grammar.y"
{
		    n_procpar = 0;
		} break;
case 23:
# line 355 "grammar.y"
{ 
		    n_procpar = 1;
		    if (!errcnt)
			push (stkop(yypvt[-0]));
		} break;
case 24:
# line 360 "grammar.y"
{
		    n_procpar++;
		    if (!errcnt)
			push (stkop(yypvt[-0]));
		} break;
case 31:
# line 377 "grammar.y"
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
# line 414 "grammar.y"
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
# line 435 "grammar.y"
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
# line 451 "grammar.y"
{ vartype = V_BOOL; } break;
case 35:
# line 452 "grammar.y"
{ vartype = V_STRING; } break;
case 36:
# line 453 "grammar.y"
{ vartype = V_REAL; } break;
case 37:
# line 454 "grammar.y"
{ vartype = V_FILE; } break;
case 38:
# line 455 "grammar.y"
{ vartype = V_GCUR; } break;
case 39:
# line 456 "grammar.y"
{ vartype = V_IMCUR; } break;
case 40:
# line 457 "grammar.y"
{ vartype = V_UKEY; } break;
case 41:
# line 458 "grammar.y"
{ vartype = V_PSET; } break;
case 42:
# line 459 "grammar.y"
{ vartype = V_INT; } break;
case 43:
# line 460 "grammar.y"
{ vartype = V_STRUCT; } break;
case 46:
# line 467 "grammar.y"
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
# line 485 "grammar.y"
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
# line 503 "grammar.y"
{
			inited = NO;
			n_aval = 0;
		} break;
case 49:
# line 507 "grammar.y"
{
			n_aval = 0;
		} break;
case 50:
# line 510 "grammar.y"
{
			inited = YES;
		} break;
case 51:
# line 515 "grammar.y"
{
		    index_cnt = 0;
		    if (!errcnt)
			pp = initparam (stkop(yypvt[-0]), do_params, vartype, varlist);
		} break;
case 52:
# line 520 "grammar.y"
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
# line 539 "grammar.y"
{
			varlist = NO;
			index_cnt = 0;
		} break;
case 55:
# line 543 "grammar.y"
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
# line 563 "grammar.y"
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
# line 586 "grammar.y"
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
# line 607 "grammar.y"
{
			if (!errcnt) {
			    if (pp != NULL) {
				push (stkop(yypvt[-0]) );
				n_aval++;
			    }
			}
		} break;
case 64:
# line 616 "grammar.y"
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
# line 643 "grammar.y"
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
# line 657 "grammar.y"
{ yyval =  1; } break;
case 69:
# line 658 "grammar.y"
{ yyval = -1; } break;
case 70:
# line 660 "grammar.y"
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
# line 670 "grammar.y"
{
		      	if (!errcnt) {
			    if (inited && pp != NULL) {
			        eprintf (twoinits, pp->p_name);
				EYYERROR;
			    }
			}
		} break;
case 75:
# line 685 "grammar.y"
{
			if (!errcnt)
			    if (pp != NULL)
			    	do_option (pp, stkop(yypvt[-2]), stkop(yypvt[-0]));
		} break;
case 78:
# line 701 "grammar.y"
{
		    if (!errcnt)
		        compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
		} break;
case 80:
# line 715 "grammar.y"
{
		    if  (!errcnt)
		        compile (PUSHCONST, stkop(yypvt[-0]));
		} break;
case 81:
# line 719 "grammar.y"
{
		    /* "gcur" is both a keyword and a CL global parameter,
		     * and must be built into the grammar here to permit
		     * reference of the parameter in expressions.
		     */
		    if (!errcnt)
			compile (PUSHPARAM, "gcur");
		} break;
case 82:
# line 727 "grammar.y"
{
		    if (!errcnt)
			compile (PUSHPARAM, "imcur");
		} break;
case 83:
# line 731 "grammar.y"
{
		    if (!errcnt)
			compile (PUSHPARAM, "ukey");
		} break;
case 84:
# line 735 "grammar.y"
{
		    if (!errcnt)
			compile (PUSHPARAM, "pset");
		} break;
case 86:
# line 743 "grammar.y"
{
		    if (!errcnt)
			compile (ADD);
		} break;
case 87:
# line 747 "grammar.y"
{
		    if (!errcnt)
			compile (SUB);
		} break;
case 88:
# line 751 "grammar.y"
{
		    if (!errcnt)
			compile (MUL);
		} break;
case 89:
# line 755 "grammar.y"
{
		    if (!errcnt)
			compile (DIV);
		} break;
case 90:
# line 759 "grammar.y"
{
		    if (!errcnt)
			compile (POW);
		} break;
case 91:
# line 763 "grammar.y"
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
# line 772 "grammar.y"
{
		    if (!errcnt)
			compile (CONCAT);
		} break;
case 93:
# line 776 "grammar.y"
{
		    if (!errcnt)
			compile (LT);
		} break;
case 94:
# line 780 "grammar.y"
{
		    if (!errcnt)
			compile (GT);
		} break;
case 95:
# line 784 "grammar.y"
{
		    if (!errcnt)
			compile (LE);
		} break;
case 96:
# line 788 "grammar.y"
{
		    if (!errcnt)
			compile (GE);
		} break;
case 97:
# line 792 "grammar.y"
{
		    if (!errcnt)
			compile (EQ);
		} break;
case 98:
# line 796 "grammar.y"
{
		    if (!errcnt)
			compile (NE);
		} break;
case 99:
# line 800 "grammar.y"
{
		    if (!errcnt)
			compile (OR);
		} break;
case 100:
# line 804 "grammar.y"
{
		    if (!errcnt)
			compile (AND);
		} break;
case 101:
# line 808 "grammar.y"
{
		    if (!errcnt)
			compile (NOT);
		} break;
case 102:
# line 812 "grammar.y"
{
		    if (!errcnt)
			compile (CHSIGN);
		} break;
case 103:
# line 817 "grammar.y"
{
		    /* someday scan will be in with intrins.
		     * it at least has the same syntax...
		     */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 104:
# line 823 "grammar.y"
{
		    struct	operand o;
		    if (!errcnt) {
			o.o_type = OT_INT;
		        o.o_val.v_i = pop();	/* get total number of args*/
			compile (PUSHCONST, &o);
			compile (SCAN);
		    }
		} break;
case 105:
# line 833 "grammar.y"
{
		    /* someday fscan will be in with intrins.
		     * it at least has the same syntax...
		     */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 106:
# line 839 "grammar.y"
{
		    struct	operand o;
		    if (!errcnt) {
			o.o_type = OT_INT;
			o.o_val.v_i = pop();	/* get total number of args*/
			compile (PUSHCONST, &o);
			compile (FSCAN);
		    }
		} break;
case 107:
# line 849 "grammar.y"
{
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} break;
case 108:
# line 852 "grammar.y"
{
		    struct	operand o;
		    if (!errcnt) {
			o.o_type = OT_INT;
			o.o_val.v_i = pop();
			compile (PUSHCONST, &o);
			compile (INTRINSIC, stkop(yypvt[-4])->o_val.v_s);
		    }
		} break;
case 110:
# line 867 "grammar.y"
{
			/* The YACC value of this must match normal intrinsics
			 * so we must generate an operand with the proper
			 * string. 
			 */
			if (!errcnt)
			    yyval = addconst ("int", OT_STRING);
		} break;
case 111:
# line 875 "grammar.y"
{
			if (!errcnt)
			    yyval = addconst ("real", OT_STRING);
		} break;
case 113:
# line 887 "grammar.y"
{
		    if (!errcnt) {
			compile (PUSHCONST, stkop (yypvt[-0]));
		        push (pop() + 1);		/* inc num args	*/
		    }
		} break;
case 114:
# line 893 "grammar.y"
{
		    if (!errcnt) {
			compile (PUSHCONST, stkop (yypvt[-2]));
		        push (pop() + 1);		/* inc num args	*/
		    }
		} break;
case 116:
# line 904 "grammar.y"
{
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		} break;
case 117:
# line 908 "grammar.y"
{
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		} break;
case 140:
# line 946 "grammar.y"
{
		    bracelevel++;
		} break;
case 141:
# line 948 "grammar.y"
{
		    --bracelevel;
		} break;
case 145:
# line 960 "grammar.y"
{
			--parenlevel;
 			if (!errcnt)
		      	    compile (ASSIGN, stkop(yypvt[-2])->o_val.v_s);
		} break;
case 146:
# line 965 "grammar.y"
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
case 147:
# line 976 "grammar.y"
{
			parenlevel++;
		} break;
case 148:
# line 979 "grammar.y"
{
		      	--parenlevel;
			if (!errcnt)
		  	    compile (yypvt[-1], stkop(yypvt[-3])->o_val.v_s);
		} break;
case 149:
# line 988 "grammar.y"
{
			parenlevel++;
		} break;
case 150:
# line 993 "grammar.y"
{ yyval = ADDASSIGN; } break;
case 151:
# line 994 "grammar.y"
{ yyval = SUBASSIGN; } break;
case 152:
# line 995 "grammar.y"
{ yyval = MULASSIGN; } break;
case 153:
# line 996 "grammar.y"
{ yyval = DIVASSIGN; } break;
case 154:
# line 997 "grammar.y"
{ yyval = CATASSIGN; } break;
case 155:
# line 1000 "grammar.y"
{
		    npipes = 0;
		} break;
case 156:
# line 1002 "grammar.y"
{
		    if (!errcnt) {
			compile (EXEC);
			if (npipes > 0)
			    compile (RMPIPES, npipes);
		    }
		} break;
case 158:
# line 1012 "grammar.y"
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
case 159:
# line 1046 "grammar.y"
{
		    /* Compile the GETPIPE instruction with the name of the
		     * second task in the current pipe, and backpatch the
		     * matching ADDPIPE instruction with the PC of the GETPIPE.
		     */
		    (coderef(pipe_pc))->c_args = compile (GETPIPE, curr_task);
		    compile (REDIRIN);
		    npipes++;		/* Overflow checking is in ADDPIPE */
		} break;
case 160:
# line 1057 "grammar.y"
{
		    yyval = 1;
		} break;
case 161:
# line 1060 "grammar.y"
{
		    yyval = 2;
		} break;
case 162:
# line 1065 "grammar.y"
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

		    absmode = 0;
		    posit = 0;
		    newstdout = 0;
		    parenlevel = 0;
		} break;
case 163:
# line 1083 "grammar.y"
{
		    inarglist = 1;
		} break;
case 164:
# line 1085 "grammar.y"
{
		    inarglist = 0;
		    parenlevel = 0;
		} break;
case 165:
# line 1091 "grammar.y"
{
		    /* (,x) equates to nargs == 2.  Call posargset with
		     * negative dummy argument to bump nargs.
		     */
		    if (!errcnt) {
			compile (POSARGSET, -1);
			posit++;
			printstmt = 0;
		    }
		} break;
case 170:
# line 1109 "grammar.y"
{
		    if (!errcnt) {
			if (posit > 0) {		/* not first time */
			    compile (POSARGSET, -posit);
			    printstmt = 0;
			}
			posit++;
		    }
		} break;
case 171:
# line 1118 "grammar.y"
{
		    if (absmode) {
			eprintf (posfirst);
			EYYERROR;
		    } else
			if (!errcnt)
			    compile (POSARGSET, posit++);
		} break;
case 172:
# line 1126 "grammar.y"
{
		    if (absmode) {
			eprintf (posfirst);
			EYYERROR;
		    } else if (!errcnt) {
			if (parenlevel == 0 || printstmt) {
			    compile (PUSHCONST, stkop(yypvt[-0]));
			    compile (INDIRPOSSET, posit++);
			    /* only first arg of fprint stmt is special.
			     */
			    printstmt = 0;
			} else {
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
			    compile (POSARGSET, posit++);
			}
		    }
		} break;
case 173:
# line 1143 "grammar.y"
{
		    absmode++;
		    if (!errcnt)
			compile (ABSARGSET, stkop(yypvt[-2])->o_val.v_s); 
		} break;
case 174:
# line 1148 "grammar.y"
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
case 175:
# line 1160 "grammar.y"
{
		    absmode++;
		    if (!errcnt)
			compile (SWON, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 176:
# line 1165 "grammar.y"
{
		    absmode++;
		    if (!errcnt)
			compile (SWOFF, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 177:
# line 1170 "grammar.y"
{
		    if (!errcnt)
			compile (REDIRIN);
		} break;
case 178:
# line 1174 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (REDIR);
		} break;
case 179:
# line 1179 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (ALLREDIR);
		} break;
case 180:
# line 1184 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (APPENDOUT);
		} break;
case 181:
# line 1189 "grammar.y"
{
		    newstdout++;
		    if (!errcnt)
			compile (ALLAPPEND);
		} break;
case 182:
# line 1194 "grammar.y"
{
		    if (!errcnt)
			compile (GSREDIR, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 183:
# line 1200 "grammar.y"
{
		    absmode++;
		    /* constant already pushed by expr0.
		     */
		} break;
case 184:
# line 1205 "grammar.y"
{
		    absmode++;
		    if (!errcnt) {
			if (parenlevel == 0)
			    compile (PUSHCONST, stkop(yypvt[-0]));
			else
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
			}
		} break;
case 185:
# line 1216 "grammar.y"
{
			--parenlevel;
			if (!errcnt)
			    compile (IMMED);
		} break;
case 186:
# line 1221 "grammar.y"
{
		      	--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop(yypvt[-0])->o_val.v_s);
		} break;
case 187:
# line 1228 "grammar.y"
{
			--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop(yypvt[-1])->o_val.v_s);
		} break;
case 188:
# line 1235 "grammar.y"
{
		    if (!errcnt)
			compile (OSESC, stkop(yypvt[-0])->o_val.v_s);
		} break;
case 189:
# line 1241 "grammar.y"
{
		    --parenlevel;
		    if (!errcnt)
			compile (IMMED);
		} break;
case 190:
# line 1248 "grammar.y"
{
		    /* pop BIFF addr and set branch to just after statement
		     */
		    int   biffaddr;
		    if (!errcnt) {
			biffaddr = pop();
		    	coderef (biffaddr)->c_args = pc - biffaddr - 3;
		    }
		} break;
case 191:
# line 1259 "grammar.y"
{
			/* save BIFF addr so branch can be filled in 
			 */
			if (!errcnt)
			    push (compile (BIFF, 0));
		} break;
case 192:
# line 1264 "grammar.y"
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
case 193:
# line 1292 "grammar.y"
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
case 194:
# line 1304 "grammar.y"
{
		    int  gotoaddr;
		    if (!errcnt) {
			/* Pop GOTO addr and set branch to just after statement
			 */
			gotoaddr = pop();
			coderef (gotoaddr)->c_args = pc - gotoaddr - 3;
		    }
		} break;
case 195:
# line 1315 "grammar.y"
{
		    /* Save starting addr of while expression.
		     */
		    if (!errcnt) {
			push (pc);
			loopincr();
		    }
		} break;
case 196:
# line 1322 "grammar.y"
{
		    /* Save BIFF addr so branch can be filled in.
		     */
		    if (!errcnt)
			push (compile (BIFF, 0));
		} break;
case 197:
# line 1327 "grammar.y"
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
case 198:
# line 1360 "grammar.y"
{
			if (!errcnt)
			    push(pc);				/* Loop1: */
		} break;
case 199:
# line 1364 "grammar.y"
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
case 200:
# line 1380 "grammar.y"
{
			int  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-3); 	/* Goto loop1 */
			    stmtaddr = pop();
			    coderef(stmtaddr)->c_args = pc - stmtaddr - 3;
			}
		} break;
case 201:
# line 1390 "grammar.y"
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
case 204:
# line 1413 "grammar.y"
{
			for_expr = YES;
		} break;
case 205:
# line 1416 "grammar.y"
{
			for_expr = NO;
		} break;
case 206:
# line 1442 "grammar.y"
{
			if (!errcnt) {
			    push (compile(SWITCH));

			    /* Compile GOTO which will branch past end of
			     * switch.  This is needed if there is no DEFAULT.
			     */
			    compile (GOTO, 0);
			}
		} break;
case 207:
# line 1451 "grammar.y"
{
			/* Set up jumptable and pop space on stack.
			 */
			if (!errcnt)
			    setswitch();
		} break;
case 208:
# line 1459 "grammar.y"
{
			if (!errcnt) {
			    ncaseval = 0;
			    if (!in_switch()) {
				eprintf ("Improper CASE statement.\n");
				EYYERROR;
			    }
			}
		} break;
case 209:
# line 1467 "grammar.y"
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
case 210:
# line 1478 "grammar.y"
{
			/* Branch to end of switch block 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		} break;
case 211:
# line 1486 "grammar.y"
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
case 212:
# line 1496 "grammar.y"
{
		      	/* Branch past jump table. 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		} break;
case 213:
# line 1504 "grammar.y"
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
case 214:
# line 1519 "grammar.y"
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
case 215:
# line 1543 "grammar.y"
{
			if (!errcnt)
			    compile (END);
		} break;
case 216:
# line 1547 "grammar.y"
{
			/* Return values currently not implemented.
			 */
			eprintf ("Warning: return value ignored.\n");
			if (!errcnt)
			    compile (END);
		} break;
case 217:
# line 1559 "grammar.y"
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
case 218:
# line 1571 "grammar.y"
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
case 220:
# line 1605 "grammar.y"
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
case 223:
# line 1635 "grammar.y"
{ 
			/* Save pc before compiling statement for loop back
			 */
			stmt_pc = pc;
			n_oarr = 0;
			i_oarr = 0;
			ifseen = NULL;
		} break;
case 224:
# line 1643 "grammar.y"
{
		      	/* If there was an open reference compile the
			 * loop increment and goback.
			 */
			int  push_pc;

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
case 226:
# line 1682 "grammar.y"
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
case 229:
# line 1742 "grammar.y"
{
				if (!errcnt) {
				    push(stkop(yypvt[-0])) ; 
				    ncaseval++;
				}
			} break;
case 232:
# line 1762 "grammar.y"
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
case 233:
# line 1799 "grammar.y"
{
		    if (!errcnt) {
			strncpy (curr_param, stkop(yypvt[-0])->o_val.v_s, SZ_FNAME);
			index_cnt = 0;
		    }
		} break;
case 234:
# line 1806 "grammar.y"
{
		    if (i_oarr > 0  &&  n_oarr == 0)
			n_oarr = i_oarr;
		    i_oarr = 0;
		    lastref = YES;
		} break;
case 235:
# line 1814 "grammar.y"
{
			index_cnt = 1;
		} break;
case 236:
# line 1817 "grammar.y"
{
			index_cnt++;
		} break;
case 238:
# line 1823 "grammar.y"
{
			if (!errcnt)
			    compile (PUSHINDEX, 0);
		} break;
case 239:
# line 1828 "grammar.y"
{ 
			if (!errcnt) {
			    compile (PUSHPARAM, stkop(yypvt[-0])->o_val.v_s);
			    compile (PUSHINDEX, 0);
			}
		} break;
case 240:
# line 1834 "grammar.y"
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
case 241:
# line 1855 "grammar.y"
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
case 242:
# line 1889 "grammar.y"
{
		    yyval = yypvt[-0];
		} break;
case 243:
# line 1894 "grammar.y"
{
		    yyval = yypvt[-0];
		} break;
case 244:
# line 1899 "grammar.y"
{
		    yyval = yypvt[-0];
		} break;
case 246:
# line 1905 "grammar.y"
{
		    /* If statements are delimited by ';'s, do not execute
		     * until next newline EOST is received.
		     */
		    sawnl = 0;
		} break;
case 252:
# line 1927 "grammar.y"
{ parenlevel++; } break;
case 253:
# line 1930 "grammar.y"
{ --parenlevel; } break;
case 254:
# line 1933 "grammar.y"
{ sawnl = 1; } break;
		}
		goto yystack;  /* stack new state and value */

	}
