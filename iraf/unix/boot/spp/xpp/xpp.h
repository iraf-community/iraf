#define	import_spp
#include <iraf.h>

/*  XPP error codes.
 */
#define	XPP_OK		OSOK		/* no problems */
#define	XPP_COMPERR	101		/* compiler error */
#define	XPP_BADXFILE	102		/* cannot open .x file */
#define	XPP_SYNTAX	104		/* language error */

/* Contexts.
 */
#define	GLOBAL		01
#define	DECL		02
#define	BODY		04
#define	DEFSTMT		010
#define	DATASTMT	020
#define	PROCSTMT	040

/* String type codes.
 */
#define	STR_INLINE	0
#define	STR_DEFINE	1
#define	STR_DECL	2

/* SPP keywords.  The datatype keywords bool through pointer must be assigned
 * the lowest numbers.
 */ 
#define	XTY_BOOL	1
#define	XTY_CHAR	2
#define	XTY_SHORT	3
#define	XTY_INT		4
#define	XTY_LONG	5
#define	XTY_REAL	6
#define	XTY_DOUBLE	7
#define	XTY_COMPLEX	8
#define	XTY_POINTER	9
#define	XTY_PROC	10
#define	XTY_TRUE	11
#define	XTY_FALSE	12
#define	XTY_IFERR	13
#define	XTY_IFNOERR	14
#define	XTY_EXTERN	15
#define	XTY_ERROR	16
#define	MAX_KEY		16

/* RPP type keywords (must match type codes above).
 */
#define	RPP_TYPES {\
	"",\
	"x$bool",\
	"x$short", 	/* MACHDEP */\
	"x$short",\
	"x$int",\
	"x$long",\
	"x$real",\
	"x$dble",\
	"x$cplx",\
	"x$pntr",\
	"x$fcn",\
	".true.",\
	".false.",\
	"iferr",\
	"ifnoerr",\
	"x$extn",\
	"error"\
}

/* xppcode.c */
extern	int linenum[];			/* line numbers in files	*/
extern	int istkptr;			/* istk pointer			*/
extern	int str_idnum;			/* for ST0000 string names	*/
extern	int nbrace;			/* count of braces		*/
extern	int nswitch;			/* number of "switch" stmts	*/
extern	int errchk;			/* sef if error checking	*/
extern	int context;			/* lexical context flags	*/
extern	char fname[][SZ_PATHNAME];
extern	const char *type_decl[];
extern	void xpp_warn ( const char * );
extern	void error ( int, const char * );
extern	void yy_unput( char );
extern	int yy_input( void );
extern	int yywrap( void );
extern	void pushcontext ( int );
extern	void process_task_statement( void );
extern	void setline( void );
extern	void put_dictionary( void );
extern	void put_interpreter( void );
extern	void skip_helpblock( void );
extern	void begin_code( void );
extern	void str_enter( void );
extern	void end_code( void );
extern	void do_string ( char, int );
extern	void skipnl( void );
extern	void do_include( void );
extern	void mapident( void );
extern	void hms ( char * );
extern	void int_constant ( char *, size_t, int );
extern	void output ( char );
extern	void do_hollerith( void );
extern	int popcontext( void );
extern	void outstr ( const char * );
/* decl.c */
extern	int d_gettok ( char *, size_t );
extern	void d_newproc ( const char *, int );
extern	void d_declaration ( int );
extern	void d_runtime ( char *, size_t );
extern	void d_codegen ( FILE * );
/* lexyy.c */
/* External and internal data stuctures.  We need access to the LEX i/o
 * buffers because we use the LEX i/o macros, which provide pushback,
 * because we must change the streams to process includes, and so on.
 * These definitions are VERY Lex dependent.
 */
extern	char yytext[];			/* LEX character buffer		*/
extern	int yyleng;			/* length of string in yytext	*/
extern	FILE *yyin, *yyout;		/* LEX input, output files	*/
extern	int yytchar;
extern	char *yysptr, yysbuf[];
extern	int yylineno;
extern	int yylex( void );
/* xppmain.c */
extern	int errflag;			/* set if compiler error	*/
extern	int hbindefs;
#if 0
extern	int foreigndefs;
#endif
