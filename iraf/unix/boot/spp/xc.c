/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include "xpp.h"

#define NOKNET
#define import_kernel
#define import_knames
#include <iraf.h>

#include "../bootlib/bootlib.h"


#if defined(LINUX) || defined(BSD)
# ifdef SOLARIS
#  undef SOLARIS
# endif
#endif

/*
 * XC -- Main entry point of the XC compiler front-end used by the IRAF
 * system.
 */

#define VERSION		"IRAFNET XC V2.3 May 21 2006"

#define ERR		(-1)
#define EOS		'\0'
#define YES		1
#define NO		0
#define MAXFLAG		64			/* maximum option flags */
#define MAXFILE		1024			/* maximum files on cmdline */
#define SZ_CMDBUF	4096			/* maximum command buffer */
#define SZ_BUFFER	4096			/* library names, flags */
#define SZ_LIBBUF	4096			/* full library names */
#define SZ_FNAME	255
#define SZ_PATHNAME	511
#define SZ_PKGENV	256
#define DEF_PKGENV	"iraf"

#ifdef MACOSX
#define CCOMP		"cc"			/* C compiler (also .s etc.) */
#define LINKER		"cc"			/* Linking utility */
#else
#define CCOMP		"gcc"			/* C compiler (also .s etc.) */
#define LINKER		"gcc"			/* Linking utility */
#endif
#define F77COMP		"f77"			/* Fortran compiler */
#define DEBUGFLAG	'g'			/* host flag for -x */
#define USEF2C		1			/* use Fortran to C trans. */

#define LIBCINCLUDES	"hinclude$"		/* IRAF LIBC include dir */
#define LOCALBINDIR	"/usr/local/bin/"	/* standard local BIN */
#define SYSBINDIR	"/usr/bin/"		/* special system BIN */

#define XPP		"xpp.e"
#define RPP		"rpp.e"
#define EDSYM		"edsym.e"
#define SHIMAGE		"S.e"
#define LIBMAIN		"libmain.o"
#define SHARELIB	"libshare.a"
#define IRAFLIB1	"libex.a"
#define IRAFLIB2	"libsys.a"
#define IRAFLIB3	"libvops.a"
#define IRAFLIB4	"libos.a"

#ifdef LINUX
static const char *fortlib[] = { "-lf2c",	/*  0  (host progs) */
		    "-lf2c",			/*  1  */
		    "-lm",			/*  2  */
#ifndef LINUXPPC
#if 0
		    "-lcompat",			/*  3  */
#endif
		    "",				/*  3  */
#else
		    "-lg2c",			/*  3  */
#endif
		    "",				/*  4  */
		    "",				/*  5  */
		    "",				/*  6  */
		    "",				/*  7  */
		    "",				/*  8  */
		    "",				/*  9  */
		    0};				/* EOF */

static const char *opt_flags[] = { "-O",	/*  0  */
		    0};				/* EOF */
static int nopt_flags = 1;			/* No. optimizer flags */

#else
#ifdef BSD
static const char *fortlib[] = { "-lf2c",	/*  0  (host progs) */
		    "-lf2c",			/*  1  */
		    "-lm",			/*  2  */
		    "-lcompat",			/*  3  */
		    "",				/*  4  */
		    "",				/*  5  */
		    "",				/*  6  */
		    "",				/*  7  */
		    "",				/*  8  */
		    "",				/*  9  */
		    0};				/* EOF */

static const char *opt_flags[] = { "-O",	/*  0  */
		    0};				/* EOF */
static int nopt_flags = 1;			/* No. optimizer flags */

#else
#ifdef MACOSX
static const char *fortlib[] = { "-lf2c",	/*  0  (host progs) */
		    "-lf2c",			/*  1  */
		    "-lm",			/*  2  */
		    "",				/*  3  */
		    "",				/*  4  */
		    "",				/*  5  */
		    "",				/*  6  */
		    "",				/*  7  */
		    "",				/*  8  */
		    "",				/*  9  */
		    0};				/* EOF */

static const char *opt_flags[] = { "-O3",	/*  0  */
		    0};				/* EOF */

/* As of Dec2007 there remains an unexplained optimizer bug in
** the system which has the effect of disabling FPE handling on
** Mac Intel/PPC systems.  For the moment, we'll disable the optimization
** until this is better understood or fixed in future GCC versions.
*/
static int nopt_flags = 0;			/* No. optimizer flags */

#else
#ifdef SOLARIS
static const char *fortlib[] = { "-lf2c",	/*  0  (host progs) */
		    "-lf2c",			/*  1  */
		    "-lm",			/*  2  */
		    "-lsocket",			/*  3  */
		    "-lnsl",			/*  4  */
		    "-lintl",			/*  5  */
		    "-ldl",			/*  6  */
		    "-lelf",			/*  7  */
		    "",				/*  8  */
		    "",				/*  9  */
		    0};				/* EOF */

static const char *opt_flags[] = { "-O",	/*  0  */
		    0};				/* EOF */
static int nopt_flags = 1;			/* No. optimizer flags */

#ifdef CYGWIN
static const char *fortlib[] = { "-lf2c",	/*  0  (host progs) */
		    "-lf2c",			/*  1  */
		    "-lm",			/*  2  */
		    "-lcompat",			/*  3  */
		    "",				/*  4  */
		    "",				/*  5  */
		    "",				/*  6  */
		    "",				/*  7  */
		    "",				/*  8  */
		    "",				/*  9  */
		    0};				/* EOF */

static const char *opt_flags[] = { "-O",	/*  0  */
		    0};				/* EOF */
static int nopt_flags = 1;			/* No. optimizer flags */

#else
static const char *fortlib[] = { "-lU77",	/*  0  (host progs) */
		    "-lm",			/*  1  */
		    "-lF77",			/*  2  */
		    "-lI77",			/*  3  */
		    "-lm",			/*  4  */
		    "",				/*  5  */
		    "",				/*  6  */
		    "",				/*  7  */
		    "",				/*  8  */
		    "",				/*  9  */
		    0};				/* EOF */

static const char *opt_flags[] = { "-O",	/*  0  */
		    0};				/* EOF */
static int nopt_flags = 1;			/* No. optimizer flags */

#endif
#endif
#endif
#endif
#endif

#ifdef BSD
#define F_STATIC	"-static"
#define F_SHARED	"-shared"
#else
#ifdef MACOSX
#define F_STATIC	"-static"
#define F_SHARED	"-shared"
#else
#ifdef LINUX
#define F_STATIC	"-Wl,-Bstatic"
#define F_SHARED	"-Wl,-Bdynamic"
#else
#ifdef SOLARIS
#define F_STATIC	"-Wl,-Bstatic"
#define F_SHARED	"-Wl,-Bdynamic"
#endif
#endif
#endif
#endif

#define isxfile(str)	(getextn(str) == 'x')
#define isffile(str)	(getextn(str) == 'f')
#define iscfile(str)	(getextn(str) == 'c')
#define issfile(str)	(getextn(str) == 's')
#define isefile(str)	(getextn(str) == 'e')
#define isafile(str)	(getextn(str) == 'a')
#define isofile(str)	(getextn(str) == 'o')


#ifdef SOLARIS
#ifdef X86
static int usesharelib = NO;
static int noedsym     = YES;
#else
static int usesharelib = YES;
static int noedsym     = NO;
#endif

#else
#ifdef SHLIB
static int usesharelib = YES;
static int noedsym     = NO;
#else
static int usesharelib = NO;
static int noedsym     = YES;
#endif
#endif

static int outputargs	= YES;
static int stripexe	= NO;
static int notvsym	= NO;
static int noshsym	= NO;
static int errflag	= NO;
static int objflags	= NO;
static int keepfort	= NO;
static int mkobject	= YES;
static int mktask	= YES;
static int optimize	= YES;
static int cflagseen	= NO;
static int nfileargs	= 0;
static int link_static	= NO;
static int link_nfs	= NO;
static int debug	= NO;
static int dbgout	= NO;
static int hostprog	= NO;
static int voslibs	= YES;
static int nolibc	= NO;
static int usef2c	= YES;
static int useg95 	= YES;
static int userincs	= NO;
#ifdef LINUXPPC
static int useg2c	= YES;
#else
static int useg2c	= NO;
#endif
static int host_c_main	= NO;

static char ccomp[SZ_FNAME]   = CCOMP;
static char f77comp[SZ_FNAME] = F77COMP;
static char linker[SZ_FNAME]  = LINKER;
static char f2cpath[SZ_FNAME] = "/usr/bin/f2c";
/* static char g77path[SZ_FNAME] = "/usr/bin/g77"; */

static char outfile[SZ_FNAME]  = "";
static char tempfile[SZ_FNAME] = "";
static char *lflags[MAXFLAG];
static char *lfiles[MAXFILE];		/* all files		*/
static char *hlibs[MAXFILE];		/* host libraries	*/
static char *lxfiles[MAXFILE];		/* .x files		*/
static char *lffiles[MAXFILE];		/* .f files		*/
static char buffer[SZ_BUFFER+1];
static char libbuf[SZ_LIBBUF+1];
static char *bp = buffer;
static char *maxbp = buffer + SZ_BUFFER+1 -1;
static char *libp = libbuf;
static char *maxlibp = libbuf + SZ_LIBBUF+1 -1;
static const char *pkgenv = NULL;
static char v_pkgenv[SZ_PKGENV+1];
static int nflags, nfiles, nhlibs, nxfiles, nffiles;
static signal_handler_t sig_int, sig_quit, sig_hup, sig_term;
/* static const char *shellname = "/bin/sh"; */
static int foreigndefs = NO;
static const char *foreign_defsfile = "";
static const char *irafarch = "";		/* IRAFARCH string */
static char floatoption[32] = "";		/* f77 arch flag, if any */
static int pid;

static void add_include_dir ( const char * );
static int isv13( void );
static char *findexe ( const char *, char * );
static char *iraflib ( const char * );
static void enbint ( signal_handler_t );
static void fatal ( const char * );
static int getextn ( const char * );
static void xtof ( char * );
static int addtolist( const char *, const char ***, int * );
static int addflags ( const char *, const char ***, int * );
static void printargs ( const char *, const char *[], int );
static int run ( const char *, const char *[] );
static void done ( int );
static int sys ( const char * );
static void fatalstr ( const char *, const char * );
static void rmfiles( void );
static void chdot ( char *, char );
static void interrupt( int );
static const char *mkfname ( const char * );
static int await ( int );


/* MAIN -- Execution begins here.  Interpret command line arguments and
 * pass commands to UNIX to execute the various passes, i.e.:
 *
 *	xpp		SPP to modified-ratfor
 *	rpp		modified-ratfor to Fortran
 *	f77		UNIX fortran compiler
 *	cc		compile other sources, link if desired
 *
 * The Fortran source is left behind if the -F flag is given.  The IRAF root
 * directory must either be given on the command line as "-r pathname" or in
 * the environment as the variable "irafdir".
 */
int main ( int argc, char *argv[] )
{
	int i, j, nargs, ncomp, status, noperands;
	const char **arglist = NULL;
	char *arg;
	const char *ep;

	/* Initialization. */
	ZZSTRT();
	isv13();

#if defined(LINUX) || defined(BSD) || defined(X86) || defined(MACOSX)
	if (os_sysfile ("f77.sh", "hscripts", f77comp, SZ_FNAME) < 0) {
	    strcpy (f77comp, "f77");
	    usef2c = 0;
	} else
	    usef2c = 1;
	if (os_sysfile ("f2c.e", "hbin", tempfile, SZ_FNAME) > 0)
	    strcpy (f2cpath, tempfile);
#else
	strcpy (f77comp, "f77");
#endif

	nflags = nfiles = nhlibs = nxfiles = nffiles = 0;

	sig_int  = signal (SIGINT,  SIG_IGN);
	sig_quit = signal (SIGQUIT, SIG_IGN);
	sig_hup  = signal (SIGHUP,  SIG_IGN);
	sig_term = signal (SIGTERM, SIG_IGN);

	enbint (interrupt);
	pid = getpid();

	/* Load any XC related environment definitions.
	 */
	if ((ep = os_getenv ("XC-CC")) || (ep = os_getenv ("XC_CC")))
	    safe_strcpy (ccomp, SZ_FNAME, ep);
	if ((ep = os_getenv ("XC-F77")) || (ep = os_getenv ("XC_F77"))) {
	    safe_strcpy (f77comp, SZ_FNAME, ep);
	    usef2c = (strncmp (f77comp, "f77", 3) == 0 ? 1 : 0);
	    useg95 = (strncmp (f77comp, "g95", 3) == 0 ? 1 : 0);
	}
	if ((ep = os_getenv ("XC-LINKER")) || (ep = os_getenv ("XC_LINKER")))
	    safe_strcpy (linker, SZ_FNAME, ep);



        /* Always load the default IRAF package environment. */
	loadpkgenv (DEF_PKGENV);

	/* Count the number of file arguments.  Load the environment for
	 * any packages named on the command line.
	 */
	pkgenv = NULL;
	v_pkgenv[0] = EOS;
	for (i=1, nfileargs=0;  argv[i] != NULL;  i++)
	    if (argv[i][0] != '-')
		nfileargs++;
	    else if (strcmp (argv[i], "-p") == 0 && argv[i+1]) {
		loadpkgenv (argv[++i]);
		safe_strcat (v_pkgenv, SZ_PKGENV+1, v_pkgenv[0] ? " -p " : "-p ");
		safe_strcat (v_pkgenv, SZ_PKGENV+1, argv[i]);
		pkgenv = v_pkgenv;
	    }

	/* If no package environment was specified see if the user has
	 * specified a default package in their user environment.
	 */
	if (!pkgenv) {
	    char *pp, u_pkgenv[SZ_PKGENV+1];
	    const char *pkgname;

	    if (ep = os_getenv ("PKGENV")) {
		safe_strcpy (pp = u_pkgenv, SZ_PKGENV+1, ep);
		while (*pp) {
		    while (isspace(*pp))
			pp++;
		    pkgname = pp;
		    while (*pp && !isspace(*pp))
			pp++;
		    if (*pp)
			*pp++ = EOS;

		    if (pkgname[0]) {
			loadpkgenv (pkgname);
			safe_strcat (v_pkgenv, SZ_PKGENV+1, v_pkgenv[0] ? " -p " : "-p ");
			safe_strcat (v_pkgenv, SZ_PKGENV+1, pkgname);
			pkgenv = v_pkgenv;
		    }
		}
	    }
	}

	/* Process command line options, make file lists.
	 * Convert ".x" files to ".f".
	 */
	for (i=1;  (arg = argv[i]) != NULL;  i++) {
	    if (arg[0] == '-') {
		const char *ip;
		char *s;
		switch (arg[1]) {
		case '/':
		    /* Pass flag on without further interpretation.
		     *     -/hostflag
		     */
		    if ( MAXFLAG <= nflags )
			fatal ("Too many compiler options");
		    lflags[nflags] = bp;
		    nflags++;
		    if ( bp < maxbp ) *bp++ = '-';
		    for ( ip = &arg[2] ; (*ip) ; ip++ ) {
			if ( bp < maxbp ) {
			    *bp = *ip;
			    bp++;
			}
			else {
			    fatal ("Out of buffer space for options");
			}
		    }
		    *bp = EOS;
		    if ( bp < maxbp ) bp++;
#ifdef sun
		    /* Check for an explicit architecture setting.  If given
		     * this will override IRAFARCH.
		     */
		    if (strncmp (arg, "-/f", 3) == 0)
			snprintf (floatoption, 32, "-%s", &arg[2]);
#endif
		    break;

		case 'D':
		    /* Pass a -D<define> flag on to the host compiler.
		     */
		    if ( MAXFLAG <= nflags )
			fatal ("Too many compiler options");
		    lflags[nflags] = bp;
		    nflags++;
		    for ( ip = &arg[0] ; (*ip) ; ip++ ) {
			if ( bp < maxbp ) {
			    *bp = *ip;
			    bp++;
			}
			else {
			    fatal ("Out of buffer space for options");
			}
		    }
		    *bp = EOS;
		    if ( bp < maxbp ) bp++;
		    break;

		case 'I':
		    /* Pass a -I<include-dir> flag on to the host compiler.
		     * A special case is "-Inolibc" which disables automatic
		     * inclusion of the IRAF LIBC includes (hinclude$).
		     */
		    if (strcmp (&arg[2], "nolibc") == 0)
			nolibc++;
		    else {
			const char *vp;
			if ( MAXFLAG <= nflags )
			    fatal ("Too many compiler options");
			lflags[nflags] = bp;
			nflags++;
			if ( bp < maxbp ) *bp++ = arg[0];
			if ( bp < maxbp ) *bp++ = arg[1];
			vp = vfn2osfn (&arg[2], 0);
			safe_strcpy (bp, SZ_BUFFER+1 - (bp-buffer), vp);
			if ( strlen(bp) < strlen(vp) )
			    fatal ("Out of buffer space for options");
			bp += strlen (bp);
			if ( bp < maxbp ) bp++;
		    }
		    break;

		case 'l':
		case 'L':
		    /* Library file (-llib) or library directory (-Ldir)
		     * reference.
		     */
		    s = iraflib (arg);
		    if ( s == NULL ) {
			if ( MAXFILE <= nhlibs )
			    fatal ("Too many files");
			hlibs[nhlibs] = arg;
		        nhlibs++;
		    } else {
			if ( MAXFILE <= nfiles )
			    fatal ("Too many files");
			lfiles[nfiles] = s;
			nfiles++;
		    }

		    objflags = YES;
		    mkobject = YES;
		    mktask = YES;
		    break;

		case 'o':
		    /* Set output file name.
		     */
		    if ((arg = argv[++i]) == NULL)
			i--;
		    else
			safe_strcpy (outfile, SZ_FNAME, arg);
		    mkobject = YES;
		    mktask = YES;
		    objflags = YES;
		    break;

		case 'p':
		    /* Ignore since the -p args were already processed above.
		     */
		    i++;
		    break;

		case 'r':
		    /* Not used anymore */
		    if ((arg = argv[++i]) == EOS)
			i--;
		    break;

		case 'h':
		    /* Host program: do not link in IRAF main or search
		     * standard IRAF libraries unless explicitly referenced
		     * on command line.
		     */
		    voslibs = 0;
		    /* fall through */

		case 'H':
		    /* Link a host program, but include the VOS libraries.
		     */
		    hostprog++;
		    noedsym++;
		    nolibc++;
		    break;

		case 'G':
		    /* Force a program to link w/ libg2c.a instead of libf2c.a
		     */
		    useg2c++;
		    break;

		case 'A':
		    /* Force arch-specific include files.
		     */
		    userincs++;
		    break;

		case 'C':
		    /* Link a host program which has a C main.  We may need
		     * to tweak the command line as a special case here since
		     * we normally assume Fortran sources.  This is currently
		     * only needed for host C programs under LinuxPPC.
		     */
		    host_c_main++;
		    break;

		case 'V':
		    /* Print XC version identification.
		     */
		    fprintf (stderr, "%s\n", VERSION);
		    fflush (stderr);
		    break;

		default:
		    if (strcmp (&arg[1], "Nh") == 0) {
			if ((arg = argv[++i]) == EOS)
			    i--;
			else {
			    foreigndefs++;
			    foreign_defsfile = arg;
			    continue;
			}
		    }

		    if ( MAXFLAG <= nflags )
			fatal ("Too many compiler options");
		    lflags[nflags] = bp;
		    if ( bp < maxbp ) *bp++ = '-';

		    /* Process list of flags without arguments, e.g. "-xyz"
		     * which is the same as "-x -y -z".
		     */
		    for (ip = &arg[1];  *ip != EOS;  ip++)
			if (*ip == 'c') {
			    mkobject = YES;
			    mktask = NO;
			    objflags = YES;
			    cflagseen = YES;

			} else if (*ip == 'd') {
			    debug++;
			} else if (*ip == 'q') {
			    optimize = NO;
			} else if (*ip == 'O') {
			    optimize = YES;

			} else if (*ip == 'F' || *ip == 'f') {
			    keepfort = YES;
			    if (objflags == NO) {
				mkobject = NO;
				mktask = NO;
			    }
			} else if (*ip == 'x') {
			    dbgout++;
			    optimize = NO;
			    if ( bp < maxbp ) *bp++ = DEBUGFLAG;
			    else
				fatal ("Out of buffer space for options");
			} else if (*ip == 'z') {
			    usesharelib = NO;
			} else if (*ip == 'e') {
			    noedsym = YES;
			} else if (*ip == 't') {
			    notvsym = YES;
			} else if (*ip == 'T') {
			    noshsym = YES;
			} else if (*ip == 's') {
			    stripexe = YES;
			    goto passflag;
			} else if (*ip == 'N') {
			    /* "NFS" link option.  Generate the output temp
			     * file in /tmp during the link, then move it to
			     * the output directory in one operation when done.
			     * For cases such as linking in an NFS-mounted
			     * directory, where all the NFS i/o may slow the
			     * link down excessively.
			     */
			    link_nfs = YES;
			} else {
passflag:		    mkobject = YES;
			    if (!cflagseen)
				mktask = YES;
			    if ( bp < maxbp) *bp++ = *ip;
			    else
				fatal ("Out of buffer space for options");
		        }

		    if (bp - lflags[nflags] <= 1) {
			lflags[nflags] = NULL;
			if ( bp - lflags[nflags] == 1 ) bp--;
		    } else {
			*bp = EOS;
			if ( bp < maxbp ) bp++;
			nflags++;
		    }
		}

	    } else {
		const char *ip;
		char *op, *maxop, *last_dot, *s;

		/* Get default name for output executable file, if not given
		 * as arg.  The default extension is ".e".
		 */
		if (outfile[0] == EOS) {
		    last_dot = NULL;
		    maxop = outfile + SZ_FNAME -1;
		    for ( ip=arg, op=outfile ; op < maxop && *ip != EOS ; op++, ip++ ) {
			*op = *ip;
			if (*op == '.') last_dot = op;
		    }
		    if (last_dot != NULL) op = last_dot;
		    *op = EOS;
		    safe_strcat (outfile, SZ_FNAME, ".e");
		}

		/* Munge filename if file is a library. */
		if (isafile(arg) && (s = iraflib(arg)))
		    arg = s;

		if (access (arg,0) == -1) {
		    fprintf (stderr, "Warning: file `%s' not found\n", arg);
		    fflush (stderr);
		} else {
		    if ( MAXFILE <= nfiles )
			fatal ("Too many files");
		    lfiles[nfiles] = arg;
		    nfiles++;

		    if (isxfile (arg)) {
			xtof (arg);
			if (errflag & (XPP_BADXFILE | XPP_COMPERR)) {
			    nfiles--;
			    errflag &= ~(XPP_BADXFILE | XPP_COMPERR);
			}
		    } else if (isffile (arg)) {
			if ( MAXFILE <= nffiles )
			    fatal ("too many files");
			lffiles[nffiles] = arg;
			nffiles++;
		    } else if (isefile (arg))
			fatal ("no .e files permitted in file list");
		}
	    }
	}
	
	if (!mkobject) {
	    if (debug) {
		fprintf (stderr, "quit, fortran only\n");
		fflush (stderr);
	    }
	    ZZSTOP();
	    exit (errflag);
	}

	/* Add -I<include-dir> to lflags for each directory in the cincludes
	 * package library list.  cincludes is a comma delimited list of VFN
	 * directory names formed by loading the core system and layered
	 * package environments.
	 */
	add_include_dir ( os_getenv ("cincludes") );

	/* Now check for any alternative compiler definitions or commandline
	 * flags which will affect out link line.  Some systems like LinuxPPC
	 * will require use of -lg2c even though we can continue to use the
	 * hscripts$f77.sh the fortran compiler script on that system.
	 */
	if (useg2c || strncmp (f77comp, "g77", 3) == 0) {
	    fortlib[0] = fortlib[1] = "-lg2c";
	}


#ifdef sun
	/* Determine if any special architecture dependent compilation flags
	 * are needed.  For the Sun V1.3 compiler, since FLOAT_OPTION is no
	 * longer supported, we look for IRAFARCH and generate the -f68881
	 * or -ffpa compiler switches automatically if we are compiling on a
	 * Sun-3 and no -/f* has already been specified on the command line.
	 */
	if (!floatoption[0] && (irafarch = os_getenv("IRAFARCH")))
	    if (irafarch[0] == 'f')
		snprintf (floatoption, 32, "-%s", irafarch);
#endif
	/* Compile all F77 source files with F77 to produce object code.
	 * This compilation is separate from that used for the '.x' files,
	 * because we do not want to use the UNIX "-u" flag (requires that
	 * everything be declared) for raw Fortran files.
	 */
	arglist = NULL;
	nargs = 0;
	addtolist (f77comp, &arglist, &nargs);
	addtolist ("-c", &arglist, &nargs);
	if (usef2c == YES) {
	    addtolist ("-f2c", &arglist, &nargs);
	    addtolist (f2cpath, &arglist, &nargs);
	}

#ifdef MACOSX
	if (useg95 == 0) {
	    if ((irafarch = os_getenv("IRAFARCH"))) {
		addtolist ("-arch", &arglist, &nargs)
	        if (strcmp (irafarch, "macosx") == 0) 
		    addtolist ("ppc", &arglist, &nargs)
	        else if (strcmp (irafarch, "macintel") == 0) 
		    addtolist ("i386", &arglist, &nargs)
	    }
	}
#endif

#ifdef LINUXAOUT
	addtolist ("-b", &arglist, &nargs);
	addtolist ("i486-linuxaout", &arglist, &nargs);
#endif
#ifdef sun
	if (floatoption[0])
	    addtolist (floatoption, &arglist, &nargs);
#endif
        if (optimize) {
	    for (i=0;  i < nopt_flags;  i++)
	        addtolist (opt_flags[i], &arglist, &nargs);
	}

	/* Add the user-defined flags last so they can override the 
	 * hardwired options.
	 */
	if ((ep = os_getenv("XC-FFLAGS")) || (ep = os_getenv("XC_FFLAGS")))
	    addflags (ep, &arglist, &nargs);

	for (i=0;  i < nflags;  i++)
	    addtolist (lflags[i], &arglist, &nargs);
	
	for (i=0;  i < nffiles;  i++)
	    addtolist (lffiles[i], &arglist, &nargs);
	arglist[nargs] = NULL;

	if (i > 0) {
	    if (outputargs)
		printargs (f77comp, arglist, nargs);
	    status = run (f77comp, arglist);
#ifdef LINUX
	/* This kludge is to work around a bug in the F2C based F77 script
	 * on Linux, which returns an exit status of 4 when successfully
	 * compiling a Fortran file.
	 */
	if (status == 4)
	    status = 0;
#endif
	    errflag += status;
	}


	/* Compile the remaining Fortran source files with F77 to produce
	 * object code.
	 */
	nargs = 0;
	addtolist (f77comp, &arglist, &nargs);
	addtolist ("-c", &arglist, &nargs);
	addtolist ("-u", &arglist, &nargs);
	addtolist ("-x", &arglist, &nargs);
	if (usef2c == YES) {
	    addtolist ("-f2c", &arglist, &nargs);
	    addtolist (f2cpath, &arglist, &nargs);
	}

#ifdef MACOSX
	if (useg95 == 0) {
	    if ((irafarch = os_getenv("IRAFARCH"))) {
		addtolist ("-arch", &arglist, &nargs)
	        if (strcmp (irafarch, "macosx") == 0) 
		    addtolist ("ppc", &arglist, &nargs)
	        else if (strcmp (irafarch, "macintel") == 0) 
		    addtolist ("i386", &arglist, &nargs)
	    }
	}
#endif

#ifdef LINUXAOUT
        addtolist ("-b", &arglist, &nargs);
	addtolist ("i486-linuxaout", &arglist, &nargs);
#endif
#ifdef sun
	if (floatoption[0])
	    addtolist (floatoption, &arglist, &nargs);
#endif
        if (optimize) {
	    for (i=0;  i < nopt_flags;  i++)
	       addtolist (opt_flags[i], &arglist, &nargs);
	}

	/* Add the user-defined flags last so they can override the 
	 * hardwired options.
	 */
	if ((ep = os_getenv("XC-FFLAGS")) || (ep = os_getenv("XC_FFLAGS")))
	    addflags (ep, &arglist, &nargs);

	for (i=0;  i < nflags;  i++)
	    addtolist (lflags[i], &arglist, &nargs);
	
	/* Make list of files to be compiled.  Do not include F77 files,
	 * as they were already compiled above.
	 */
	for (i=0, noperands=0;  i < nfiles;  i++) {
	    for (j=0;  j < nffiles && lffiles[j] != lfiles[i];  j++)
		;
	    if (j >= nffiles && isffile (lfiles[i])) {
		addtolist (lfiles[i], &arglist, &nargs);
		noperands++;
	    }
	}
	arglist[nargs] = NULL;

	if (noperands > 0) {
	    if (outputargs)
		printargs (f77comp, arglist, nargs);
	    status = run (f77comp, arglist);
#ifdef LINUX
	    /* This kludge is to work around a bug in the F2C based F77 script
	     * on Linux, which returns an exit status of 4 when successfully
	     * compiling a Fortran file.
	     */
	    if (status == 4)
		status = 0;
#endif
	    errflag += status;
	}


	/* Compile the remaining non-Fortran source files with CC to produce
	 * object code.
	 */
	nargs = 0;
	addtolist (ccomp, &arglist, &nargs);
	addtolist ("-c", &arglist, &nargs);

#ifdef LINUX
	addtolist ("-DLINUX", &arglist, &nargs);
#ifdef REDHAT
	addtolist ("-DREDHAT", &arglist, &nargs);
#endif
#ifdef LINUXPPC
	addtolist ("-DLINUXPPC", &arglist, &nargs);
#endif
	addtolist ("-DPOSIX", &arglist, &nargs);
	addtolist ("-DSYSV", &arglist, &nargs);
#endif

#ifdef BSD
	addtolist ("-DBSD", &arglist, &nargs);
#endif

#ifdef MACOSX
	addtolist ("-DMACOSX", &arglist, &nargs);
	if (useg95 == 0) {
	    if ((irafarch = os_getenv("IRAFARCH"))) {
		addtolist ("-arch", &arglist, &nargs);
	        if (strcmp (irafarch, "macosx") == 0) 
		    addtolist ("ppc", &arglist, &nargs);
	        else if (strcmp (irafarch, "macintel") == 0) 
		    addtolist ("i386", &arglist, &nargs);
	    }
	}
#endif

#ifdef MACOSX
	addtolist ("-DMACOSX", &arglist, &nargs);
#endif

#ifdef SOLARIS
	addtolist ("-DSOLARIS", &arglist, &nargs);
#ifdef X86
	addtolist ("-DX86", &arglist, &nargs);
#endif
	addtolist ("-DPOSIX", &arglist, &nargs);
	addtolist ("-DSYSV", &arglist, &nargs);
#endif

#ifdef LINUXAOUT
        addtolist ("-b", &arglist, &nargs);
	addtolist ("i486-linuxaout", &arglist, &nargs);
#endif

#ifdef sun
	if (floatoption[0])
	    addtolist (floatoption, &arglist, &nargs);
#endif
	if (optimize) {
	    for (i=0;  i < nopt_flags;  i++)
	        addtolist (opt_flags[i], &arglist, &nargs);
	}

	/* Add the user-defined flags last so they can override the 
	 * hardwired options.
	 */
	if ((ep = os_getenv("XC-CFLAGS")) || (ep = os_getenv("XC_CFLAGS")))
	    addflags (ep, &arglist, &nargs);

	for (i=0;  i < nflags;  i++)
	    addtolist (lflags[i], &arglist, &nargs);
	
	/* Make list of files to be compiled.  Only C and assembler files
	 * are included.
	 */
	for (i=0, noperands=0;  i < nfiles;  i++) {
	    if (iscfile (lfiles[i]) || issfile (lfiles[i])) {
		addtolist (lfiles[i], &arglist, &nargs);
		noperands++;
	    }
	}
	arglist[nargs] = NULL;

	if (noperands > 0) {
	    if (outputargs)
		printargs (ccomp, arglist, nargs);
	    errflag += run (ccomp, arglist);
	}


	/* If "-c" (compile only), or there was a compiler error, do not
	 * proceed with the link.
	 */
	if (!mktask || cflagseen || errflag)
	    done (errflag);


	/* Link the object files and libraries to produce the "-o" task.
	 */
	nargs = 0;
	addtolist (linker, &arglist, &nargs);
	if ((ep = os_getenv("XC-LFLAGS")) || (ep = os_getenv("XC_LFLAGS")))
	    addflags (ep, &arglist, &nargs);

#ifdef MACOSX
	if (useg95 == 0 && (irafarch = os_getenv("IRAFARCH"))) {
	    addtolist ("-arch", &arglist, &nargs);
	    if (strcmp (irafarch, "macosx") == 0) 
		addtolist ("ppc", &arglist, &nargs);
	    else if (strcmp (irafarch, "macintel") == 0) 
		addtolist ("i386", &arglist, &nargs);
	}
#endif

#ifdef SOLARIS
	addtolist ("-Wl,-t", &arglist, &nargs);
#endif
/*
#ifdef LINUX
	addtolist ("-Wl,--defsym,mem_=0", &arglist, &nargs);
#endif
*/
#ifdef NEED_GCC_SPECS
	{
	    char gcc_specs[SZ_PATHNAME];
	    static char cmd[SZ_CMDBUF];

	    if (os_sysfile ("gcc-specs", "hconfig", gcc_specs, SZ_PATHNAME) < 0)
		addtolist ("/iraf/iraf/unix/bin/gcc-specs", &arglist, &nargs);
	    snprintf (cmd, SZ_CMDBUF, "-specs=%s", gcc_specs);
	    addtolist (cmd, &arglist, &nargs);
	}
#endif
#ifdef LINUXAOUT
        addtolist ("-b", &arglist, &nargs);
	addtolist ("i486-linuxaout", &arglist, &nargs);
#endif
	addtolist ("-o", &arglist, &nargs);

	if (link_nfs) {
	    snprintf (tempfile, SZ_FNAME, "/tmp/T_%s.XXXXXX", outfile);
	    mkstemp (tempfile);
	} else
	    snprintf (tempfile, SZ_FNAME, "T_%s", outfile);
	addtolist (tempfile, &arglist, &nargs);

	ncomp = 0;
	for (i=0;  i < nfiles;  i++) {
	    char *pp;
	    if (*(pp = lfiles[i]) != '-') {
		while (*pp++ != EOS)
		    ;
		while (*--pp != '.' && pp >= lfiles[i])
		    ;
		if (*pp == '.') {
		    switch (pp[1]) {
		    case 'f':
		    case 'r':
		    case 'c':
		    case 's':
		    case 'e':
			pp[1] = 'o';
			ncomp++;
		    }
		}
	    }
	}

	/* Link options. */
	link_static = 0;
	for (i=0;  i < nflags;  i++) {
	    addtolist (lflags[i], &arglist, &nargs);
	    if (strcmp (lflags[i], F_STATIC) == 0)
		link_static = 1;
	    else if (strcmp (lflags[i], F_SHARED) == 0)
		link_static = 0;
	}

#ifdef sun
	/* Need to pass -f<float> to CC for the C libraries. */
	if (floatoption[0])
	    addtolist (floatoption, &arglist, &nargs);

	/* If we are using the V1.3 Sun Fortran compiler, the V1.3 "f77"
	 * should be a symbolic link pointing to the BIN directory for the
	 * new compiler.  Construct the path to this directory and put it
	 * out as a -Ldir flag on the link line to ensure that the library
	 * is searched for linking.
	 */
	if (isv13()) {
	    char libpath[SZ_PATHNAME];
	    char dir[SZ_PATHNAME];
	    const char *path;
	    char *pp, *op;
	    int n;

	    path = findexe ("f77", dir);

	    strcpy (libpath, "-L");
	    safe_strcat (libpath, SZ_PATHNAME, dir);
	    for (op=libpath;  *op;  op++)
		;
	    if ((n = readlink (path, op, SZ_PATHNAME - (op-libpath) -1)) > 0) {
		const char *ip, *vp;

		op[n] = EOS;

		for (pp=op;  *pp;  pp++)
		    if (*pp == '/')
			op = pp;
		*op = EOS;

		/* Search, e.g., /usr/lang/SC0.0/ffpa first if Sun-3. */
		if (floatoption[0]) {
		    vp = floatoption + 1;
		    *op = '/';
		    safe_strcpy (op+1, SZ_PATHNAME - (op+1 - libpath), vp);
		    safe_strcpy (libp, SZ_LIBBUF+1 - (libp-libbuf), libpath);
		    libp += strlen (ip = libp);
		    if ( libp < maxlibp ) libp++;
		    addtolist (ip, &arglist, &nargs);
		}

		/* Search /usr/lang/SC0.0 (or whatever). */
		*op = EOS;
		safe_strcpy (libp, SZ_LIBBUF+1 - (libp-libbuf), libpath);
		libp += strlen (ip = libp);
		if ( libp < maxlibp ) libp++;
		addtolist (ip, &arglist, &nargs);
	    }
	}
#endif

	/* File to link. */
	for (i=0;  i < nfiles;  i++)
	    addtolist (lfiles[i], &arglist, &nargs);

	/* Libraries to link against.
	 */
	if (hostprog) {
#ifdef LINUXPPC
	    /* LinuxPPC (YellowDog anyway) requires this library to resolve
	     * the MAIN__ generated by the fortran program statement into
	     * the 'main'.
	     */
	    if (host_c_main == 0)
	        addtolist ("-lfrtbegin", &arglist, &nargs);
#else
	    if (!isv13())
		addtolist (mkfname (fortlib[0]), &arglist, &nargs);
#endif
	} else
	    addtolist (mkfname (LIBMAIN), &arglist, &nargs);

	if (voslibs) {
	    if (usesharelib) {
		addtolist (mkfname (SHARELIB), &arglist, &nargs);
		addtolist (mkfname (IRAFLIB4), &arglist, &nargs);
	    } else {
		addtolist (mkfname (IRAFLIB1), &arglist, &nargs);
		addtolist (mkfname (IRAFLIB2), &arglist, &nargs);
		addtolist (mkfname (IRAFLIB3), &arglist, &nargs);
		addtolist (mkfname (IRAFLIB4), &arglist, &nargs);
	    }
	}

	/* Host libraries, searched after iraf libraries. */
	for (i=0;  i < nhlibs;  i++)
	    addtolist (hlibs[i], &arglist, &nargs);

	/* The remaining system libraries depend upon which version of
	 * the SunOS compiler we are using.  The V1.3 compilers use only
	 * -lF77 and -lm.
	 */
	if (isv13()) {
	    addflags (fortlib[2], &arglist, &nargs);
	    addflags (fortlib[4], &arglist, &nargs);
	} else {
	    addflags (fortlib[1], &arglist, &nargs);
	    addflags (fortlib[2], &arglist, &nargs);
	    addflags (fortlib[3], &arglist, &nargs);
	    addflags (fortlib[4], &arglist, &nargs);
	    addflags (fortlib[5], &arglist, &nargs);
	    addflags (fortlib[6], &arglist, &nargs);
	    addflags (fortlib[7], &arglist, &nargs);
	    addflags (fortlib[8], &arglist, &nargs);
	    addflags (fortlib[9], &arglist, &nargs);
	}
	arglist[nargs] = NULL;

	if (ncomp) {
	    fprintf (stderr, "link:\n");
	    fflush (stderr);
	}
	if (outputargs)
	    printargs (linker, arglist, nargs);

	/* If the link is successful, replace the old executable with the
	 * new one.  Do not delete the bad executable if the link fails,
	 * as we might want to examine its symbol table.
	 */
	if ((status = run (linker, arglist)) == 0) {
	    unlink (outfile);

	    if (link_nfs) {
		char command[SZ_CMDBUF];
		snprintf (command, SZ_CMDBUF, "/bin/cp -f %s %s", tempfile, outfile);
		if (outputargs)
		    printargs (command, NULL, 0);
		status = sys (command);
	    } else
		link (tempfile, outfile);

	    unlink (tempfile);
	}
	errflag += status;

	/* If we are linking against the iraf shared library and symbol editing
	 * has not been disabled, edit the symbol table of the new executable
	 * to provide symbols within the shared image.
	 */
	if (usesharelib && !noedsym && !stripexe) {
	    char    shlib[SZ_PATHNAME+1];
	    char    edsym[SZ_PATHNAME+1];
	    char    command[SZ_CMDBUF];

	    /* The os_sysfile(SHIMAGE) below assumes the existence of a file
	     * entry "S.e" in the directory containing the real shared image
	     * "S<n>.e".  We can't easily look directly for S<n>.e because
	     * the process symbol table and image has to be examined to
	     * determine the shared image version number.
	     */
	    if (os_sysfile (SHIMAGE, "hbin", shlib, SZ_PATHNAME+1) > 0) {
		if (os_sysfile (EDSYM, "hbin", edsym, SZ_PATHNAME+1) > 0) {
		    snprintf (command, SZ_CMDBUF, "%s %s %s", edsym, outfile, shlib);
		    if (noshsym)
			safe_strcat (command, SZ_CMDBUF, " -T");
		    else if (notvsym)
			safe_strcat (command, SZ_CMDBUF, " -t");
		    status = sys (command);
		}
	    }
	}
	errflag += status;

	if ( arglist ) free(arglist);

	done (errflag);

	return 0;
}


/* Add -I<include-dir> to lflags for each directory in the env_vals.
 * env_vals is a comma delimited list of VFN directory names formed by
 * loading the core system and layered package environments.
 */
static void add_include_dir ( const char *env_vals )
{
	const char *ip, *vp;
	char *op, *maxop, fname[SZ_FNAME];

	if ( env_vals == NULL ) return;

	for ( ip=env_vals ; *ip ; ) {
	    while ( *ip && isspace(*ip) || *ip == ',' )
		ip++;
	    maxop = fname + SZ_FNAME -1;
	    for ( op=fname ; *ip && !(isspace (*ip) || *ip == ',') ; ip++ ) {
		if ( op < maxop ) {
		    *op = *ip;
		    op++;
		}
	    }
	    *op++ = EOS;
	    if (*fname == EOS)
		break;

	    /* Omit the LIBC includes if -Inolibc was specified. */
	    if (! (nolibc && strcmp (fname, LIBCINCLUDES) == 0)) {
		if ( MAXFLAG <= nflags )
		    fatal ("Too many compiler options");
		lflags[nflags] = bp;
		nflags++;
		if ( bp < maxbp ) *bp++ = '-';
		if ( bp < maxbp ) *bp++ = 'I';
		for ( vp=vfn2osfn(fname,0) ; (*vp) ; vp++ ) {
		    if ( bp < maxbp ) {
			*bp = *vp;
			bp++;
		    }
		    else {
			fatal ("Out of buffer space for options");
		    }
		}
		if (*(bp-1) == '/') {
		    --bp;
		}
		*bp = EOS;
		if ( bp < maxbp ) bp++;
	    }

	    while (*ip && (isspace(*ip) || *ip == ','))
		ip++;
	}
}

/* MKFNAME -- Make the UNIX pathname of an IRAF library file.  Use os_sysfile
 * the get the vfn of the library file, so that we do not have to know what
 * system directory the library file is in.
 */
static const char *mkfname ( const char *i_fname )
{
	char fname[SZ_PATHNAME+1];
	const char *oname;

	/* Library referenced as -lXXX */
	if (strncmp (i_fname, "-l", 2) == 0) {
	    snprintf (fname, SZ_PATHNAME+1, "lib%s.a", &i_fname[2]);
	    if (oname = iraflib (fname))
		return (oname);
	    else
		return (i_fname);
	}

	/* Must be a library filename or pathname */
	safe_strcpy (fname, SZ_PATHNAME+1, i_fname);
	if (oname = iraflib (fname))
	    safe_strcpy (libp, SZ_LIBBUF+1 - (libp-libbuf), oname);
	else
	    safe_strcpy (libp, SZ_LIBBUF+1 - (libp-libbuf), fname);

	oname = libp;
	libp += strlen (libp);
	if ( libp < maxlibp ) libp++;

	return (oname);
}


static int addtolist( const char *arg, const char ***p_arglist, int *p_nargs )
{
	void *tmp_ptr;

	tmp_ptr = realloc( *p_arglist, sizeof(**p_arglist)*(*p_nargs + 2) );
	if ( tmp_ptr == NULL ) {
	    fatal ("addtolist(): realloc() failed");
	}
	*p_arglist = tmp_ptr;
	(*p_arglist)[*p_nargs] = arg;
	(*p_nargs)++;
	(*p_arglist)[*p_nargs] = NULL;

	return 0;
}


/* ADDFLAGS -- Add one or more flags to an argument list.  Ignore null flags,
 * separate multiple flags on whitespace.
 */
static int addflags ( const char *flag, const char ***p_arglist, int *p_nargs )
{
	int i, len;
	const char *fp;
	char *fs, lflag[SZ_FNAME];

	if (flag && *flag) {

	    for (fp = flag; *fp; ) {
		while (*fp && isspace(*fp))		/* skip leading space */
		    fp++;
		for (i=0; *fp && !isspace(*fp); ) {	/* collect flag       */
		    if ( i < SZ_FNAME -1 ) lflag[i++] = *fp;
		    fp++;
		}
		lflag[i] = '\0';
		len = strlen (lflag);
		fs = malloc(len+1);
		if ( fs == NULL ) fatal ("malloc() failed");
		strcpy (fs, lflag);

	        if (strcmp (lflag, F_STATIC) == 0) {
		    link_static = 1;
	        } else if (strcmp (lflag, F_SHARED) == 0) {
		    link_static = 0;
#if defined(LINUX) || defined(BSD) || defined(X86) || defined(MACOSX)
	        } else if ((strcmp (lflag, "-lf2c") == 0) || 
			   (strcmp (lflag, "-lcompat") == 0)) {
		        /* Use the IRAF version of libf2c.a or libcompat.a,
		         * not the host version which may or may not be present.
		         */
		        addtolist (mkfname (lflag), p_arglist, p_nargs);
		        return (1);
	        }
#endif
#ifdef SOLARIS
	        else if (strcmp (lflag, "-ldl") == 0) {
		    /* This beastie has to be linked dynamic on Solaris, but
		     * we don't want to have to know this everywhere so we do
		     * it automatically there.
		     */
		    if (link_static)
		        addtolist (F_SHARED, p_arglist, p_nargs);
		    addtolist (fs, p_arglist, p_nargs);
		    if (link_static)
		        addtolist (F_STATIC, p_arglist, p_nargs);
		    return (1);
	        }
#endif
	        addtolist (fs, p_arglist, p_nargs);
	    }

	    return (1);
	}

	return (0);
}


/* IRAFLIB -- Determine if "libname" is an IRAF library.  If so return
 * the pathname of the library, else return NULL.
 */
static char *iraflib ( const char *libref )
{
	char *absname;
	char *op, *maxop;
	const char *ip;
	char savename[SZ_PATHNAME+1];
	char libname[SZ_PATHNAME+1];
	char fname[SZ_PATHNAME+1];
	char path[SZ_PATHNAME+1];
	int foundit, dbg = dbgout;

	safe_strcpy (savename, SZ_PATHNAME+1, libref);

	/* If dbgout is enabled try the debug library first, but fall back
	 * to the normal library if thie debug library is not found.
	 */
again:
	if (strncmp (libref, "-l", 2) == 0) { 
	    snprintf (libname, SZ_PATHNAME+1, "lib%s.a", libref+2);
	    libref = libname;
	    goto again;
	} else
	    safe_strcpy (libname, SZ_PATHNAME+1, libref);

	/* Position IP to EOS. */
	for (ip=libref;  *ip;  ip++)
	    ;

	if (!(*(ip-2) == '.' && *(ip-1) == 'a')) {
	    /* Not a library file, leave it alone.
	     */
	    safe_strcpy (fname, SZ_PATHNAME+1, libref);

	} else {
	    /* Normalize the library file name, "libXXX[_p].a".
	     */
	    maxop = fname + SZ_PATHNAME+1 -1;
	    for ( ip=libref, op=fname ; op < maxop && (*ip) ; op++, ip++ )
		*op = *ip;
	    if ( fname <= op-2 && (*(op-2) == '.' && *(op-1) == 'a') ) {
		op -= 2;
	    }
	    /* ???
	    else {
		if ( fname < op ) op -= 1;
	    }
	    */

	    if ( dbg && fname <= op-2 && !(*(op-2) == '_' && *(op-1) == 'p') ) {
		if ( op < maxop ) *op++ = '_';
		if ( op < maxop ) *op++ = 'p';
	    }
	    if ( op < maxop ) *op++ = '.';
	    if ( op < maxop ) *op++ = 'a';
	    *op = '\0';
	}

	foundit = 0;
	if (access (fname, 0) == 0) {
	    strcpy (path, fname);
	    foundit++;
	} else {
	    if (os_sysfile (fname, "extlibs", path, SZ_PATHNAME+1) > 0)
		foundit++;
	}

	if (foundit) {
	    const char *vp = vfn2osfn (path, 0);
	    absname=bp;
	    safe_strcpy (bp, SZ_BUFFER+1 - (bp-buffer), vp);
	    if ( strlen(bp) < strlen(vp) )
		fatal ("Out of space for library names");
	    bp += strlen (bp);
	    if ( bp < maxbp ) bp++;
	    if (debug > 1)
		fprintf (stderr, "iraflib: %s -> %s\n", savename, absname);
	    return (absname);
	} else if (dbg) {
	    dbg = 0;
	    goto again;
	} else {
	    if (debug > 1)
		fprintf (stderr, "iraflib: %s -> %s\n", savename, savename);
	    return (NULL);
	}
}


/* PRINTARGS -- Echo a UNIX command on the standard error output.
 */
static void printargs ( const char *cmd, const char *arglist[], int nargs )
{
	int i;

	fputs (cmd, stderr);
	for (i=1;  i < nargs;  i++)
	    fprintf (stderr, " %s", arglist[i]);
	putc ('\n', stderr);
	fflush (stderr);
}


/* XTOF -- Convert a ".x" file into a ".f" file, i.e., call up the preprocessor
 * to translate an SPP file into Fortran.
 */
static void xtof ( char *file )
{
	static char xpp_path[SZ_PATHNAME+1], rpp_path[SZ_PATHNAME+1];
	char cmdbuf[SZ_CMDBUF], fname[SZ_FNAME];

	if ( MAXFILE <= nxfiles )
	    fatal ("too many files");
	lxfiles[nxfiles] = file;
	nxfiles++;

	if (nfileargs > 1 || mkobject) {
	    fprintf (stderr, "%s:\n", file);
	    fflush (stderr);
	}

	if (!xpp_path[0])
	    if (os_sysfile (XPP, "sppincludes", xpp_path, SZ_PATHNAME+1) <= 0)
		strcpy (xpp_path, XPP);

	if (userincs) {
	    if (pkgenv)
	        snprintf (cmdbuf, SZ_CMDBUF, "%s %s -A -R %s", xpp_path, pkgenv, file);
	    else
	        snprintf (cmdbuf, SZ_CMDBUF, "%s -A -R %s", xpp_path, file);
	} else {
	    if (pkgenv)
	        snprintf (cmdbuf, SZ_CMDBUF, "%s %s -R %s", xpp_path, pkgenv, file);
	    else
	        snprintf (cmdbuf, SZ_CMDBUF, "%s -R %s", xpp_path, file);
	}

	if (foreigndefs) {
	    safe_strcat (cmdbuf, SZ_CMDBUF, " -h ");
	    safe_strcat (cmdbuf, SZ_CMDBUF, foreign_defsfile);
	}

	errflag |= sys (cmdbuf);
	chdot (file, 'r');

	safe_strcpy (fname, SZ_FNAME, file);
	chdot (fname, 'f');

	if (!rpp_path[0])
	    if (os_sysfile (RPP, "sppincludes", rpp_path, SZ_PATHNAME+1) <= 0)
		strcpy (rpp_path, RPP);
	snprintf (cmdbuf, SZ_CMDBUF, "%s %s%s >%s",
	    rpp_path, dbgout ? "-g " : "", file, fname);
	if (!(errflag & XPP_BADXFILE))
	    errflag |= sys (cmdbuf);

	unlink (file);			/* remove ".r" file */
	chdot (file, 'f');		/* change name to ".f" */
}


/* GETEXTN -- Get a one letter extension from a file name (BPS 07.23.96)
 */
static int getextn ( const char *fname )
{
	const char *ip, *dot;
        int ch;

	for (ip=fname, dot=NULL;  *ip != EOS;  ip++)
	    if (*ip == '.')
		dot = ip;

	if ( dot == NULL || dot+2 != ip ) {
	    ch = EOS;
	} else {
	    ch = *(dot+1);
	}

	return (ch);
}
	

/* CHDOT -- Change the filename extension, i.e., the single character
 * following the "." at the end of the filename, to the indicated character.
 */
static void chdot ( char *fname, char dotchar )
{
	char	*p, *p_dot = NULL;

	p = fname;
	while (*p++ != EOS)
	    ;
	while ( fname < p ) {
	    p--;
	    if ( *p == '.' && *(p+1) != EOS ) {
		p_dot = p;
		break;
	    }
	}
	if ( p_dot ) *(p_dot+1) = dotchar;
}


/* RUN -- Send a command to UNIX and return the execution status to our
 * caller at the completion of the command.
 */
static int run ( const char *task, const char *argv_in[] )
{
	char *cmdbuf;
	char **argv;
	char path[SZ_PATHNAME];
	int waitpid, i, len_str, num_arg;

	len_str = 0;
	num_arg = 0;
	for ( i=0 ; argv_in[i] != NULL ; i++ ) {
	    len_str += strlen(argv_in[i]) + 1;
	    num_arg++;
	}
	if ( len_str <= 0 ) return 0;

	cmdbuf = (char *)malloc(len_str);
	if ( cmdbuf == NULL ) {
	    fatalstr ("%s failed.", "malloc()");
	    return -1;
	}
	argv = (char **)malloc( sizeof(char *) * (num_arg+1) );
	if ( argv == NULL ) {
	    free(cmdbuf);
	    fatalstr ("%s failed.", "malloc()");
	    return -1;
	}

	len_str = 0;
	for ( i=0 ; argv_in[i] != NULL ; i++ ) {
	    argv[i] = cmdbuf + len_str;
	    strcpy(argv[i], argv_in[i]);
	    len_str += strlen(argv_in[i]) + 1;
	}
	argv[i] = NULL;

	if ((waitpid = fork()) == 0) {
	    enbint (SIG_DFL);

	    execvp (task, argv);	/* use user PATH for search */
	    strcpy (path, SYSBINDIR);
	    safe_strcat (path, SZ_PATHNAME, task);
	    execv  (path, argv);	/* look in SYSBINDIR */
	    strcpy (path, LOCALBINDIR);
	    safe_strcat (path, SZ_PATHNAME, task);
	    execv  (path, argv);	/* look in LOCALBINDIR */

	    fatalstr ("Cannot execute %s", task);
	}

	i = await (waitpid);
	free(argv);
	free(cmdbuf);

	return ( i );
}


/*
 * Task execution and interrupt handling routines,
 * taken with minor modifications the F77 driver.
 */


/* SYS -- Execute a general UNIX command passed as a string.  The command may
 * contain i/o redirection metacharacters.  The full path of the command to
 * be executed should be given (and always is in the case of XC).
 */
static int sys ( const char *cmnd )
{
	char cmd[SZ_CMDBUF];
	char *ip;
	char *argv[256];
	const char *inname, *outname;
	int append, waitpid, argc;

	strncpy(cmd,cmnd,SZ_CMDBUF);
	cmd[SZ_CMDBUF-1] = '\0';

	if (debug) {
	    fprintf (stderr, "debug: %s\n", cmd);
	    fflush (stderr);
	}

	inname  = NULL;
	outname = NULL;
	append = NO;
	argc = 0;

	/* Parse command string into argv array, inname, and outname.
	 */
	ip = cmd;
	while (isspace (*ip))
	    ++ip;
	while (*ip) {
	    if (*ip == '<')
		inname = ip+1;
	    else if (*ip == '>') {
		if (ip[1] == '>') {
		    append = YES;
		    outname = ip+2;
		} else {
		    append = NO;
		    outname = ip+1;
		}
	    } else {
		if ( argc < 256-1 ) 
		    argv[argc++] = ip;
		else {
		    fprintf (stderr, "sys.c: Warning: 256-1 <= argc \n");
		    fflush (stderr);
		}
	    }
	    while ( !isspace (*ip) && *ip != '\0' )
		++ip;
	    if (*ip) {
		*ip++ = '\0';
		while (isspace (*ip))
		    ++ip;
	    }
	}

	if (argc <= 0)				/* no command */
	    return (-1);
	argv[argc] = 0;

	/* Execute the command. */
	if ((waitpid = fork()) == 0) {
	    if (inname)
		freopen (inname, "r", stdin);
	    if (outname)
		freopen (outname, (append ? "a" : "w"), stdout);
	    enbint (SIG_DFL);

	    execv (argv[0], argv);
	    fatalstr ("Cannot execute %s", argv[0]);
	}

	return (await (waitpid));
}


/* DONE -- Called at process shutdown to cleanup.  Primary action is to delete
 * the intermediate Fortran files, unless the -F flag was given on the command
 * line.
 */
static void done ( int k )
{
	static	int recurs = NO;

	if (recurs == NO) {
	    recurs = YES;
	    if (!keepfort)
	        rmfiles();
	}

	ZZSTOP();
	exit (k);
}


/* ENBINT -- Post an exception handler function to be executed if any sort
 * of interrupt occurs.
 */
static void enbint ( signal_handler_t handler )
{
	if (sig_int != SIG_ERR)
	    signal (SIGINT, handler);
	if (sig_quit != SIG_ERR)
	    signal (SIGQUIT, handler);
	if (sig_hup != SIG_ERR)
	    signal (SIGHUP, handler);
	if (sig_term != SIG_ERR)
	    signal (SIGTERM, handler);
}


/* INTERRUPT -- Exception handler, called if an interrupt is received
 * during compilation.
 */
static void interrupt( int junk )
{
	done (2);
}


/* AWAIT -- Wait for an asynchronous child process to terminate.
 */
static int await ( int waitpid )
{
	int	w, status;

	enbint (SIG_IGN);
	while ((w = wait (&status)) != waitpid)
	    if (w == -1)
		fatal ("bad wait code");
	enbint (interrupt);
	if (status & 0377) {
	    if (status != SIGINT) {
		fprintf (stderr, "Termination code %d", status);
		fflush (stderr);
	    }
	    done (2);
	}
	return (status>>8);
}


/* RMFILES -- Delete all of the ".f" intermediate Fortran files.
 */
static void rmfiles( void )
{
	int	i;

	for (i=0;  i < nxfiles;  i++) {
	    chdot (lxfiles[i], 'f');
	    unlink (lxfiles[i]);
	}
}


/* FATALSTR -- Fatal error with an sprintf format and one string argument.
 */
static void fatalstr ( const char *s1, const char *s2 )
{
	char out[SZ_CMDBUF];

	snprintf (out, SZ_CMDBUF, s1, s2);
	fatal (out);
}


/* FATAL -- A fatal error has occurred.  Print error message and terminate
 * process execution.
 */
static void fatal ( const char *s )
{
	fprintf (stderr, "Fatal compiler error: %s\n", s);
	fflush (stderr);
	done (1);
}


#if 0
/* ERROR -- Print a warning message but do not terminate the process.
 */
void error ( const char *s )
{
	fprintf (stderr, "Error: %s\n", s);
	fflush (stderr);
}
#endif

#if 0
/* ISV3 -- Test if we are using the version 3.x (or greater) Sunsoft Compilers.
 * This returns true unless SC2.x.x is detected.
 */
static int isv3( void )
{
	static int v3 = -1;

#ifndef SOLARIS
	return (v3 = 0);
#else
	DIR *dirp;
	const char *path;
	char dir[SZ_PATHNAME];
	char link[SZ_PATHNAME];
	int n;

        if (v3 != -1)
            return (v3);

        if ((path = findexe ("f77", dir))) {
            /* Check if command is a symlink to someplace else, if so look
             * for the string 'SC2.0' in the link path indicating this is
             * a V2 compiler installation.  Otherwise check the path to a
             * file for the same thing.  This assumes that V3 is true
             * unless something is found to say otherwise.
             */
            if ((n = readlink (path, link, SZ_PATHNAME -1)) > 0) {
                link[n] = '\0';
                if (strstr (link, "SC2.0"))
                    return (v3 = 0);
            } else
                if (strstr (dir, "SC2.0"))
                    return (v3 = 0);
        }

        return (v3 = 1);
#endif
}
#endif

/* ISV13 -- Test if we are using the version 1.3 Sun Fortran compiler.
 * There is no simple, reliable way to do this.  The heuristic used is
 * to first locate the "f77" we will use, then see if there is a file
 * named "f77-1.3*" in the same directory.
 */
static int isv13( void )
{
	static int v13 = -1;
	struct dirent *dp;
	char dir[SZ_PATHNAME];
	const char *name;
	DIR *dirp;

#ifdef SOLARIS
	return (v13 = 0);
#else

	if (v13 != -1)
	    return (v13);

	if (findexe ("f77", dir) && (dirp = opendir(dir)) != NULL) {
	    while (dp = readdir(dirp)) {
		/* Actually, we don't want to be too picky about the
		 * version number of this won't work for future versions,
		 * so just match up to the version number.
		 */
		name = dp->d_name;
		if (!strncmp (name, "f77-1.3", 4) && isdigit(name[4])) {
		    closedir (dirp);
		    return (v13 = 1);
		}
	    }
	    closedir (dirp);
	}

	return (v13 = 0);
#endif
}


/* FINDEXE -- Search for the named file and return the path if found, else
 * NULL.  If "dir" is non-NULL the directory in which the file resides is
 * returned in the string buffer pointed to.  The user's PATH is searched,
 * followed by SYSBINDIR, then LOCALBINDIR.
 */
static char *findexe ( const char *prog, char *dir )
/* prog : file to search for */
/* dir  : pointer to output string buf, or NULL */
{
	static char path[SZ_PATHNAME];
	char dirpath[SZ_PATHNAME];
	char *dp = dir ? dir : dirpath;
	const char *pathp;

	/* Look for the program in the directories in the user's path.
	 */
	pathp = os_getenv ("PATH");
	if ( pathp ) {
	    char *op, *maxop;
	    const char *ip;
	    ip = pathp;
	    do {
		maxop = dp + SZ_PATHNAME -1;
		for ( op=dp ; op < maxop && (*ip) && *ip != ':' ; op++, ip++ )
		    *op = *ip;
		if ( op < maxop ) *op++ = '/';
		*op = EOS;
		safe_strcpy (path, SZ_PATHNAME, dp);
		safe_strcat (path, SZ_PATHNAME, prog);
		if (access (path, 0) != -1)
		    return (path);
	    } while ( *ip++ == ':' );
	}

	/* Look in SYSBINDIR. */
	strcpy (dp, SYSBINDIR);
	strcpy (path, dp);
	safe_strcat (path, SZ_PATHNAME, prog);

	if (access (path, 0) != -1) {
	    static char envpath[8192];
	    const char *oldpath;

	    /* Add SYSBINDIR to the user's path.  This is required to
	     * use the V1.3 compiler.  Note that this code should only be
	     * executed once, since the next time findexe is called the
	     * SYSBINDIR directory will be in the default path, above.
	     */
	    if (oldpath = pathp) {
		snprintf (envpath, 8192, "PATH=%s:%s", SYSBINDIR, oldpath);
		putenv (envpath);
	    }

	    return (path);
	}

	/* Look in LOCALBINDIR. */
	strcpy (dp, LOCALBINDIR);
	strcpy (path, dp);
	safe_strcat (path, SZ_PATHNAME, prog);
	if (access (path, 0) != -1)
	    return (path);

	/* Not found. */
	return (NULL);
}
