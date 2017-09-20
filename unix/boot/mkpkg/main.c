/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#define	import_spp
#define	import_knames
#define	import_error

#include <iraf.h>

#include "mkpkg.h"
#include "../bootProto.h"

/*
 * MKPKG -- Make a package or library, following the instructions given in
 * the mkpkg file in the current directory.
 *
 *	mkpkg [-flags] [module] [sym=val ...]
 *
 *		-dddd		output debug info; up to 4 levels
 *		-i		ignore errors (cannot ignore interrupt)
 *		-f fname	set mkpkg filename; default "mkpkg"
 *		-n		no execute, just go through the motions
 *		-p pkg		load environment for the named package
 *		-u		forcibly update library module dates
 *		-v		verbose: show actions (implied by -n)
 *
 * The switch "-f stdin" causes MKPKG to read its commands from the standard
 * input, e.g., the terminal.  If a module name is given execution will start
 * at the mkpkg entry for the module, else execution starts at the beginning
 * of file.  See the manual page, etc. for additional documentation.
 */

char	sbuf[SZ_SBUF];			/* string buffer		*/
struct	symbol symtab[MAX_SYMBOLS];	/* symbol table (macros)	*/
struct	context *topcx;			/* currently active context	*/
char	*cp = sbuf;			/* pointer into sbuf		*/
char	*ctop = &sbuf[SZ_SBUF];		/* top of sbuf			*/
int	npkg = 0;			/* number of packages		*/
char	*pkgenv[MAX_PKGENV];		/* package environments		*/
char	v_pkgenv[SZ_PKGENV+1];		/* buffer for pkgenv names	*/
char	irafdir[SZ_PATHNAME+1];		/* iraf root directory		*/
int	nsymbols = 0;			/* number of defined symbols	*/
int	ifstate[SZ_IFSTACK];		/* $IF stack			*/
int	iflev;				/* $IF stack pointer		*/
int	debug = 0;			/* print debug messages		*/
int	dbgout = 0;			/* compile for debugging	*/
int	verbose = NO;			/* print informative messages	*/
int	ignore = YES;			/* ignore warns			*/
int	execute = YES;			/* think but don't act?		*/
int	exit_status;			/* exit status of last syscall	*/
int	forceupdate = NO;		/* forcibly update libmod dates	*/
extern	char *os_getenv(char *);


void warns (char *fmt, char *arg);
void fatals (char *fmt, char *arg);

extern  int  ZZSTRT (void);
extern  int  ZZSTOP (void);

extern  int  do_mkpkg (struct context *cx, int islib);



void 
zzpause (void) { printf ("ready ...."); (void) getc(stdin); }


/* MAIN -- Entry point of mkpkg.e
 */
int
main (int argc, char *argv[])
{
	struct	context *cx;
	char	flags[SZ_LINE+1];
	char	*symargs[MAX_ARGS], *modules[MAX_ARGS];
	int	islib, nsymargs=0, nmodules=0, i;
	char	**argp, *ip, *op;

	ZZSTRT();

	/* Initialize the MKPKG context.
	 */
	irafdir[0] = EOS;
	topcx = cx = (struct context *) calloc (1, sizeof (struct context));
	if (cx == NULL)
	    fatals ("out of memory (%s)", "mkpkg.e");

	strcpy (cx->mkpkgfile, MKPKGFILE);
	os_fpathname ("", cx->dirpath, SZ_PATHNAME);
	m_fninit (0);
	m_fdinit (0);

	exit_status = OK;
	ifstate[0]  = PASS;
	iflev       = 0;
	flags[0]    = EOS;
	islib       = YES;
	npkg	    = 0;

	/* Process the command line.
	 */
	for (argp = &argv[1];  *argp;  ) {
	    if (**argp == '-') {
		/* A Mkpkg switch, or a flag to be passed on to XC.
		 */
		for (ip = *argp++ + 1;  *ip;  ip++) {
		    switch (*ip) {
		    case 'f':
			if (*argp == NULL)
			    warns ("missing argument to switch `-f'", NULL);
			else
			    strcpy (cx->mkpkgfile, *argp++);
			break;
		    case 'i':
			ignore = YES;
			break;
		    case 'd':
			/* There are multiple levels of "debug"; each
			 * -d in the arg list adds a level.
			 */
			debug++;
			verbose = YES;
			break;
		    case 'x':
		    case 'g':
			dbgout++;
			goto addflag;
		    case 'n':
			execute = NO;
			verbose = YES;
			break;
		    case 'p':
			if (*argp == NULL)
			    warns ("missing argument to switch `-p'", NULL);
			else {
			    pkgenv[npkg] = *argp++;
			    loadpkgenv (pkgenv[npkg]);
			    if (npkg++ >= MAX_PKGENV)
				fatals ("too many -p package arguments", NULL);
			}
			break;
		    case 'u':
			forceupdate = YES;
			break;
		    case 'v':
			verbose = YES;
			break;
		    case 'w':
			zzpause();
			break;
		    case 'r':
			if (*argp == NULL)
			    warns ("missing argument to switch `-r'", NULL);
			else
			    strcpy (irafdir, *argp++);
			break;
		    default:
addflag:		for (op=flags;  *op;  op++)
			    ;
			*op++ = ' ';
			*op++ = '-';
			*op++ = *ip;
			*op++ = EOS;
			break;
		    }
		}

	    } else if (index (*argp, '=') != NULL) {
		/* Mark the position of a symbol definition argument.  Wait
		 * to enter this into the symbol table until after the command
		 * line has been processed and the mkpkg global include file
		 * has been read in, but go ahead and update the environment
		 * in case a logical name is affected which is referenced while
		 * processing the rest of the argument list.
		 */
		char    symbol[SZ_FNAME+1];
		char    *ip, *op;

		ip = symargs[nsymargs++] = *argp++;
		for (op=symbol;  (*op = *ip++) != '=';  op++)
		    ;
		*op = EOS;
		os_putenv (symbol, ip);

	    } else {
		/* The name of a module to be processed.
		 */
		modules[nmodules++] = *argp++;
	    }
	}

	if (debug) {
	    printf ("mkpkg");
	    for (argp = &argv[1];  *argp;  argp++)
		printf (" %s", *argp);
	    printf ("\n");
	    fflush (stdout);
	}

	/* Initialize the package environment.  This has already been done
	 * if any -p pkgname arguments were given on the command line,
	 * otherwise look for the name PKGENV in the user's environment.
	 */
	if (npkg <= 0)
	    if ((pkgenv[0] = os_getenv (PKGENV))) {
		char    *ip;

		strcpy (v_pkgenv, pkgenv[0]);
		for (ip=v_pkgenv;  *ip;  ) {
		    while (isspace (*ip))
			ip++;
		    pkgenv[npkg] = ip;
		    while (*ip && !isspace (*ip))
			ip++;
		    *ip++ = EOS;
		    loadpkgenv (pkgenv[npkg]);
		    if (npkg++ >= MAX_PKGENV)
			fatals ("too many -p package arguments", NULL);
		}
	    }

	/* Initialize the symbol table from the system dependent global
	 * MKPKG include file.
	 */
	do_include (cx, MKPKGINC);

	/* Likewise load the package global mkpkg.inc files for each
	 * reference package.
	 */
	if (npkg > 0) {
	    char   fname[SZ_PATHNAME+1];
	    int    i;

	    for (i=0;  i < npkg;  i++) {
		sprintf (fname, "%s$lib/mkpkg.inc", pkgenv[i]);
		do_include (cx, fname);
	    }
	}

	/* Append any flags given on the command line to XFLAGS.
	 */
	if (flags[0]) {
	    char   new_xflags[SZ_LINE+1];
	    sprintf (new_xflags, "%s %s", getsym(XFLAGS), flags);
	    putsym (XFLAGS, new_xflags);
	}

	/* Append any flags given on the command line to XVFLAGS.
	 */
	if (flags[0]) {
	    char   new_xvflags[SZ_LINE+1];
	    sprintf (new_xvflags, "%s %s", getsym(XVFLAGS), flags);
	    putsym (XVFLAGS, new_xvflags);
	}

	/* Append any flags given on the command line to LFLAGS.
	 */
	if (flags[0]) {
	    char   new_lflags[SZ_LINE+1];
	    sprintf (new_lflags, "%s %s", getsym(LFLAGS), flags);
	    putsym (LFLAGS, new_lflags);
	}

	/* Define the symbol "DEBUG" if building for debugging (-x).
	 */
	if (dbgout)
	    putsym (DEBUGSYM, "1");

	/* Enter any symbols or macros defined on the command line into the
	 * symbol table and environment.  Must be given without embedded
	 * whitespace, e.g., "symbol=value".
	 */
	for (i=0;  i < nsymargs;  i++) {
	    char    symbol[SZ_FNAME+1];
	    char    *ip, *op, *value;

	    for (ip = symargs[i], op=symbol;  (*op = *ip++) != '=';  op++)
		;
	    *op = EOS;
	    value = ip;
	    putsym (symbol, value);
	    os_putenv (symbol, value);
	}

	/* Process the named modules (or the first module in the mkpkg file
	 * if no modules were named.
	 */
	if (nmodules == 0) {
	    cx->library[0] = EOS;
	    exit_status = do_mkpkg (cx, islib = 0);
	} else {
	    for (i=0;  i < nmodules;  i++) {
		/* If the module is a library specification, the module name,
		 * which is the filename of the library, must end in ".a".
		 */
		char    *ip, *op;
		for (ip = modules[i], op=cx->library;  (*op = *ip++);  op++)
		    ;
		islib = (strcmp (op - 2, ".a") == 0);
		exit_status += do_mkpkg (cx, islib);
	    }
	}

	free (cx);
	m_fninit (debug);
	m_fdinit (debug);

	ZZSTOP();
	exit (exit_status == OK ? OSOK : exit_status);
}


/* WARNS -- Print error message with one string argument but do not terminate
 * program execution.
 */
void
warns (char *fmt, char *arg)
{
	char	errmsg[SZ_LINE+1];

	sprintf (errmsg, fmt, arg);
	printf ("Warning, %s line %d: %s\n", topcx->mkpkgfile, topcx->lineno,
	    errmsg);
	fflush (stdout);
}


/* FATALS -- Print error message with one string argument and terminate
 * program execution.
 */
void
fatals (char *fmt, char	*arg)
{
	char	errmsg[SZ_LINE+1];

	sprintf (errmsg, fmt, arg);
	printf ("Fatal error, %s line %d: %s\n", topcx->mkpkgfile,
	    topcx->lineno, errmsg);
	fflush (stdout);
	exit (OSOK+1);
}
