/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <dirent.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "xpp.h"
#include "../bootProto.h"

#define	NOKNET
#define	import_kernel
#define	import_knames
#include <iraf.h>

/*
 * XC -- Main entry point of the XC compiler front-end used by the IRAF
 * system.
 */

#define VERSION		"IRAFNET XC V2.4 Jan 21 2010"

#define	ERR		(-1)
#define	EOS		'\0'
#define	YES		1
#define	NO		0
#define	MAXFLAG		64			/* maximum option flags */
#define MAXFILE		1024			/* maximum files on cmdline */
#define	SZ_CMDBUF	4096			/* maximum command buffer */
#define	SZ_BUFFER	4096			/* library names, flags */
#define	SZ_LIBBUF	4096			/* full library names */
#define	SZ_FNAME	255
#define	SZ_PATHNAME	511
#define	SZ_PKGENV	256
#define DEF_PKGENV	"iraf"

#if (defined (__APPLE__) || defined (__freebsd__))
#define	CCOMP		"cc"			/* C compiler (also .s etc.) */
#define	LINKER		"cc"			/* Linking utility */
#define	F_STATIC	"-static"
#define	F_SHARED	"-shared"
#else
#define	CCOMP		"gcc"			/* C compiler (also .s etc.) */
#define	LINKER		"gcc"			/* Linking utility */
#define	F_STATIC	"-Wl,-Bstatic"
#define	F_SHARED	"-Wl,-Bdynamic"
#endif
#define	F77COMP		"f77"			/* Fortran compiler */
#define	DEBUGFLAG	'g'			/* host flag for -x */
#define	USEF2C		1			/* use Fortran to C trans. */

#define	LIBCINCLUDES	"hlib$libc/"		/* IRAF LIBC include dir */
#define	LOCALBINDIR	"/usr/local/bin/"	/* standard local BIN */
#define	SYSBINDIR	"/usr/bin/"		/* special system BIN */

#define	XPP		"xpp.e"
#define	RPP		"rpp.e"
#define	EDSYM		"edsym.e"
#define	SHIMAGE		"S.e"
#define LIBMAIN		"libmain.o"
#define SHARELIB	"libshare.a"
#define IRAFLIB1	"libex.a"
#define IRAFLIB2	"libsys.a"
#define IRAFLIB3	"libvops.a"
#define IRAFLIB4	"libos.a"
#define IRAFLIB5	"libVOTable.a"
#define IRAFLIB6	"libcfitsio.a"
#define IRAFLIB7	"liblapack.a"

char *fortlib[] = { "-lf2c",			/*  0  (host progs) */
		    "-lf2c",			/*  1  */
		    "-lm",			/*  2  */
		    "-lcurl",			/*  3  */
		    "-lexpat",			/*  4  */
#if (defined (__linux__) || defined (__gnu_hurd__))
		    "-lpthread",		/*  5  */
#else
		    "",				/*  5  */
#endif
		    "",				/*  6  */		    
		    "",				/*  7  */
		    "",				/*  8  */
		    "",				/*  9  */
		    0};				/* EOF */

char *opt_flags[] = { "-O2",			/*  0  */
		    0};				/* EOF */

int  nopt_flags	   = 1;				/* No. optimizer flags */

#define isxfile(str)	(getextn(str) == 'x')
#define isffile(str)	(getextn(str) == 'f')
#define iscfile(str)	(getextn(str) == 'c')
#define issfile(str)	(getextn(str) == 's')
#define isefile(str)	(getextn(str) == 'e')
#define isafile(str)	(getextn(str) == 'a')
#define isofile(str)	(getextn(str) == 'o')
#define ispfile(str)	(getextn(str) == 'P')	/* func prototypes	*/


int	usesharelib = NO;
int	noedsym = YES;

int	stripexe 	= NO;
int	notvsym 	= NO;
int	noshsym 	= NO;
int	errflag 	= NO;
int	objflags 	= NO;
int	keepfort 	= NO;
int	mkobject 	= YES;
int	mktask 		= YES;
int	optimize 	= YES;
int	cflagseen 	= NO;
int	nfileargs 	= 0;
int	link_static 	= NO;
int	link_nfs 	= NO;
int	debug 		= NO;
int	dbgout 		= NO;
int	hostprog 	= NO;
int	voslibs 	= YES;
int	nolibc 		= NO;
int	usef2c 		= YES;
int	useg95 		= NO;
int	userincs	= NO;
int	useg2c 		= NO;
int	host_c_main 	= NO;

char	ccomp[SZ_FNAME] 	= CCOMP;
char	f77comp[SZ_FNAME] 	= F77COMP;
char	linker[SZ_FNAME] 	= LINKER;
char	f2cpath[SZ_FNAME] 	= "/usr/bin/f2c";
char	g77path[SZ_FNAME] 	= "/usr/bin/g77";

char	outfile[SZ_FNAME] = "";
char	tempfile[SZ_FNAME] = "";
char	*lflags[MAXFLAG+1];
char	*lfiles[MAXFILE+1];			/* all files		*/
char	*hlibs[MAXFILE+1];			/* host libraries	*/
char	*lxfiles[MAXFILE+1];			/* .x files		*/
char	*lffiles[MAXFILE+1];			/* .f files		*/
char	buffer[SZ_BUFFER+1];
char	libbuf[SZ_LIBBUF+1];
char	*bp = buffer;
char	*libp = libbuf;
char	*pkgenv = NULL;
char	*pkglibs = NULL;
char	v_pkgenv[SZ_PKGENV+1];
int	nflags, nfiles, nhlibs, nxfiles, nffiles;
long	sig_int, sig_quit, sig_hup, sig_term;
char	*shellname = "/bin/sh";
int	foreigndefs = NO;
char	*foreign_defsfile = "";
char	*irafarch = "";				/* IRAFARCH string */
char	floatoption[32] = "";			/* f77 arch flag, if any */
int	pid;


/**
 *  External procedure declarations.
 */
extern 	void ZZSTRT (void);
extern 	void ZZSTOP (void);

/**
 *  Local procedure declarations.
 */
static char *mkfname (char *i_fname);
static int   addflags (char *flag, char *arglist[], int *p_nargs);
static char *iraflib (char *libref);
static void  printargs (char *cmd, char *arglist[], int nargs);
static void  xtof (char *file);
static int   getextn (char *fname);
static void  chdot (char *fname, char dotchar);

static int   run (char *task, char *argv[]);
static int   sys (char *cmd);

static void  done (int k);
static void  enbint (SIGFUNC handler);
static void  interrupt (void);
static int   await (int waitpid);
static void  rmfiles (void);

static void  fatalstr (char *s1, char *s2);
static void  fatal (char *s);

static char *findexe (char *prog, char *dir);




/**
 *  MAIN -- Execution begins here.  Interpret command line arguments and
 *  pass commands to UNIX to execute the various passes, i.e.:
 *
 *	xpp		SPP to modified-ratfor
 *	rpp		modified-ratfor to Fortran
 *	f77		UNIX fortran compiler
 *	cc		compile other sources, link if desired
 *
 *  The Fortran source is left behind if the -F flag is given.  The IRAF root
 *  directory must either be given on the command line as "-r pathname" or in
 *  the environment as the variable "irafdir".
 */
int
main (int argc, char *argv[])
{
	int	i, j, nargs, ncomp;
	char	*arglist[MAXFILE+MAXFLAG+10];
	char	*arg, *ip, *s;
	int	status, noperands;

	/* Initialization. */
	ZZSTRT();

	if (os_sysfile ("f77.sh", f77comp, SZ_FNAME) < 0) {
	    strcpy (f77comp, "f77");
	    usef2c = 0;
	} else
	    usef2c = 1;
	if (os_sysfile ("f2c.e", tempfile, SZ_FNAME) > 0)
	    strcpy (f2cpath, tempfile);

	nflags = nfiles = nhlibs = nxfiles = nffiles = 0;

	sig_int  = (long) signal (SIGINT,  SIG_IGN) & 01;
	sig_quit = (long) signal (SIGQUIT, SIG_IGN) & 01;
	sig_hup  = (long) signal (SIGHUP,  SIG_IGN) & 01;
	sig_term = (long) signal (SIGTERM, SIG_IGN) & 01;

	enbint ((SIGFUNC)interrupt);
	pid = getpid();

	/* Load any XC related environment definitions.
	 */
	if ((s = os_getenv ("XC-CC")) || (s = os_getenv ("XC_CC")))
	    strcpy (ccomp, s);
	if ((s = os_getenv ("XC-F77")) || (s = os_getenv ("XC_F77"))) {
	    strcpy (f77comp, s);
	    usef2c = (strncmp (f77comp, "f77", 3) == 0 ? 1 : 0);
	    useg95 = (strncmp (f77comp, "g95", 3) == 0 ? 1 : 0);
	}
	if ((s = os_getenv ("XC-LINKER")) || (s = os_getenv ("XC_LINKER")))
	    strcpy (linker, s);



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
		strcat (v_pkgenv, v_pkgenv[0] ? " -p " : "-p ");
		strcat (v_pkgenv, argv[i]);
		pkgenv = v_pkgenv;
	    }

	/* If no package environment was specified see if the user has
	 * specified a default package in their user environment.
	 */
	if (!pkgenv) {
	    char *s, u_pkgenv[SZ_PKGENV+1];
	    char *pkgname, *ip;

	    if ((s = os_getenv ("PKGENV"))) {
		strcpy (ip = u_pkgenv, s);
		while (*ip) {
		    while (isspace(*ip))
			ip++;
		    pkgname = ip;
		    while (*ip && !isspace(*ip))
			ip++;
		    if (*ip)
			*ip++ = EOS;

		    if (pkgname[0]) {
			loadpkgenv (pkgname);
			strcat (v_pkgenv, v_pkgenv[0] ? " -p " : "-p ");
			strcat (v_pkgenv, pkgname);
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
		switch (arg[1]) {
                case '/':
                    /* Pass flag on without further interpretation.
                     *     "-/foo"    ->  "-foo"
                     *     "-//foo"   ->  "foo"
                     */
                    lflags[nflags] = bp;
                    ip = &arg[2];
                    if (*ip == '/')
                        ip++;
                    else
                        *bp++ = '-';

                    while ((*bp++ = *ip++))
                        ;

                    if (nflags++ >= MAXFLAG)
                        fatal ("Too many compiler options");
                    break;

		case 'D':
		    /* Pass a -D<define> flag on to the host compiler.
		     */
		    lflags[nflags] = bp;
		    for (ip = &arg[0];  (*bp++ = *ip++);  )
			;
		    if (bp - buffer >= SZ_BUFFER)
			fatal ("Out of buffer space for options");
		    if (nflags++ >= MAXFLAG)
			fatal ("Too many compiler options");
		    break;

		case 'I':
		    /* Pass a -I<include-dir> flag on to the host compiler.
		     * A special case is "-Inolibc" which disables automatic
		     * inclusion of the IRAF LIBC includes (hlib$libc).
		     */
		    if (strcmp (&arg[2], "nolibc") == 0)
			nolibc++;
		    else {
			lflags[nflags] = bp;
			*bp++ = arg[0];
			*bp++ = arg[1];
			strcpy (bp, vfn2osfn (&arg[2], 0));
			bp += strlen (bp) + 1;

			if (bp - buffer >= SZ_BUFFER)
			    fatal ("Out of buffer space for options");
			if (nflags++ >= MAXFLAG)
			    fatal ("Too many compiler options");
		    }
		    break;

		case 'l':
		case 'L':
		    /* Library file (-llib) or library directory (-Ldir)
		     * reference.
		     */
		    if ((lfiles[nfiles] = iraflib (arg)) == NULL) {
			hlibs[nhlibs] = arg;
		        nhlibs++;
		    } else
			nfiles++;
		    if (nfiles > MAXFILE || nhlibs > MAXFILE)
			fatal ("Too many files");

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
			strcpy (outfile, arg);
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
		    if ((arg = argv[++i]) == NULL)
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
		     * we normally assume Fortran sources.
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
			if ((arg = argv[++i]) == NULL)
			    i--;
			else {
			    foreigndefs++;
			    foreign_defsfile = arg;
			    continue;
			}
		    }

		    lflags[nflags] = bp;
		    *bp++ = '-';

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
			    *bp++ = DEBUGFLAG;
			    if (bp - buffer >= SZ_BUFFER)
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
			    *bp++ = *ip;
			    if (bp - buffer >= SZ_BUFFER)
				fatal ("Out of buffer space for options");
		        }

		    if (bp - lflags[nflags] <= 1) {
			lflags[nflags] = NULL;
			bp--;
		    } else {
			*bp++ = EOS;
			if (nflags++ >= MAXFLAG)
			    fatal ("Too many compiler options");
		    }
		}

	    } else {
		char	*ip, *op, *last_dot;

		/* Get default name for output executable file, if not given
		 * as arg.  The default extension is ".e".
		 */
		if (outfile[0] == EOS) {
		    last_dot = NULL;
		    for (ip=arg, op=outfile;  (*op = *ip++) != EOS;  op++)
			if (*op == '.')
			    last_dot = op;
		    if (last_dot != NULL)
			*last_dot = EOS;
		    strcat (outfile, ".e");
		}

		/* Munge filename if file is a library. */
		if (isafile(arg) && (s = iraflib(arg)))
		    arg = s;

		if (access (arg,0) == -1) {
		    fprintf (stderr, "Warning: file `%s' not found\n", arg);
		    fflush (stderr);
		} else {
		    lfiles[nfiles++] = arg;
		    if (nfiles > MAXFILE)
			fatal ("Too many files");

		    if (isxfile (arg)) {
			xtof (arg);
			if (errflag & (XPP_BADXFILE | XPP_COMPERR)) {
			    nfiles--;
			    errflag &= ~(XPP_BADXFILE | XPP_COMPERR);
			}
		    } else if (isffile (arg)) {
			lffiles[nffiles++] = arg;
			if (nffiles > MAXFILE)
			    fatal ("too many files");
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

	/* Add -I<include-dir> to lflags for each directory in the pkglibs
	 * package library list.  pkglibs is a comma delimited list of VFN
	 * directory names formed by loading the core system and layered
	 * package environments.
	 */
	if ((pkglibs = os_getenv ("pkglibs"))) {
	    char *ip, *op, *vp, fname[SZ_FNAME];

	    for (ip=pkglibs;  *ip;  ) {
		while (*ip && (isspace(*ip) || *ip == ','))
		    ip++;
		for (op=fname;  *ip && !(isspace (*ip) || *ip == ',');  )
		    *op++ = *ip++;
		*op++ = EOS;
		if (*fname == EOS)
		    break;

		/* Omit the LIBC includes if -Inolibc was specified. */
		if (! (nolibc && strcmp (fname, LIBCINCLUDES) == 0)) {
		    lflags[nflags] = bp;
		    *bp++ = '-';
		    *bp++ = 'I';
		    for (vp=vfn2osfn(fname,0);  (*bp++ = *vp++);  )
			;
		    if (*(bp-2) == '/') {
			--bp;
			*(bp-1) = EOS;
		    }

		    if (bp - buffer >= SZ_BUFFER)
			fatal ("Out of buffer space for options");
		    if (nflags++ >= MAXFLAG)
			fatal ("Too many compiler options");
		}

		while (*ip && (isspace(*ip) || *ip == ','))
		    ip++;
	    }
	}

	/* Now check for any alternative compiler definitions or commandline
	 * flags which will affect out link line.  Some systems like LinuxPPC
	 * will require use of -lg2c even though we can continue to use the
	 * hlib$f77.sh the fortran compiler script on that system.
	 */
	if (useg2c || strncmp (f77comp, "g77", 3) == 0) {
	    fortlib[0] = fortlib[1] = "-lg2c";
	}


	/* Compile all F77 source files with F77 to produce object code.
	 * This compilation is separate from that used for the '.x' files,
	 * because we do not want to use the UNIX "-u" flag (requires that
	 * everything be declared) for raw Fortran files.
	 */
	nargs = 0;
	arglist[nargs++] = f77comp;
	arglist[nargs++] = "-c";
	if (usef2c == YES) {
	    arglist[nargs++] = "-f2c";
	    arglist[nargs++] = f2cpath;
	}

#ifdef __i386__
	arglist[nargs++] = "-m32";
#endif

        if (optimize) {
	    for (i=0;  i < nopt_flags;  i++)
	        arglist[nargs++] = opt_flags[i];
	}

	/* Add the user-defined flags last so they can override the 
	 * hardwired options.
	 */
	if ((s = os_getenv("XC-FFLAGS")) || (s = os_getenv("XC_FFLAGS")))
	    addflags (s, arglist, &nargs);

	for (i=0;  i < nflags;  i++)
	    arglist[nargs++] = lflags[i];
	
	for (i=0;  i < nffiles;  i++)
	    arglist[nargs++] = lffiles[i];
	arglist[nargs] = NULL;

	if (i > 0) {
	    if (debug)
		printargs (f77comp, arglist, nargs);
	    status = run (f77comp, arglist);
	    errflag += status;
	}


	/* Compile the remaining Fortran source files with F77 to produce
	 * object code.
	 */
	nargs = 0;
	arglist[nargs++] = f77comp;
	arglist[nargs++] = "-c";
	arglist[nargs++] = "-u";
	arglist[nargs++] = "-x";
	if (usef2c == YES) {
	    arglist[nargs++] = "-f2c";
	    arglist[nargs++] = f2cpath;
	}

#ifdef __i386__
	arglist[nargs++] = "-m32";
#endif

        if (optimize) {
	    for (i=0;  i < nopt_flags;  i++)
	       arglist[nargs++] = opt_flags[i];
	}

	/* Add the user-defined flags last so they can override the 
	 * hardwired options.
	 */
	if ((s = os_getenv("XC-FFLAGS")) || (s = os_getenv("XC_FFLAGS")))
	    addflags (s, arglist, &nargs);

	for (i=0;  i < nflags;  i++)
	    arglist[nargs++] = lflags[i];
	
	/* Make list of files to be compiled.  Do not include F77 files,
	 * as they were already compiled above.
	 */
	for (i=0, noperands=0;  i < nfiles;  i++) {
	    for (j=0;  j < nffiles && lffiles[j] != lfiles[i];  j++)
		;
	    if (j >= nffiles && isffile (lfiles[i])) {
		arglist[nargs++] = lfiles[i];
		noperands++;
	    }
	}
	arglist[nargs] = NULL;

	if (noperands > 0) {
	    if (debug)
		printargs (f77comp, arglist, nargs);
	    status = run (f77comp, arglist);
	    errflag += status;
	}


	/* Compile the remaining non-Fortran source files with CC to produce
	 * object code.
	 */
	nargs = 0;
	arglist[nargs++] = ccomp;
	arglist[nargs++] = "-c";

#ifdef __i386__
	arglist[nargs++] = "-m32";
#endif

	if (optimize) {
	    for (i=0;  i < nopt_flags;  i++)
	        arglist[nargs++] = opt_flags[i];
	}

	/* Add the user-defined flags last so they can override the 
	 * hardwired options.
	 */
	if ((s = os_getenv("XC-CFLAGS")) || (s = os_getenv("XC_CFLAGS")))
	    addflags (s, arglist, &nargs);

	for (i=0;  i < nflags;  i++)
	    arglist[nargs++] = lflags[i];
	
	/* Make list of files to be compiled.  Only C and assembler files
	 * are included.
	 */
	for (i=0, noperands=0;  i < nfiles;  i++) {
	    if (iscfile (lfiles[i]) || issfile (lfiles[i])) {
		arglist[nargs++] = lfiles[i];
		noperands++;
	    }
	}
	arglist[nargs] = NULL;

	if (noperands > 0) {
	    if (debug)
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
	arglist[nargs++] = linker;
	if ((s = os_getenv("XC-LFLAGS")) || (s = os_getenv("XC_LFLAGS")))
	    addflags (s, arglist, &nargs);

#ifdef __i386__
	arglist[nargs++] = "-m32";
#endif
	arglist[nargs++] = "-o";

	if (link_nfs) {
	    sprintf (tempfile, "/tmp/T_%s.XXXXXX", outfile);
	    mkstemp (tempfile);
	} else
	    sprintf (tempfile, "T_%s", outfile);
	arglist[nargs++] = tempfile;

	ncomp = 0;
	for (i=0;  i < nfiles;  i++)
	    if (*(ip = lfiles[i]) != '-') {
		while (*ip++ != EOS)
		    ;
		while (*--ip != '.' && ip >= lfiles[i])
		    ;
		if (*ip == '.')
		    switch (ip[1]) {
		    case 'f':
		    case 'r':
		    case 'c':
		    case 's':
		    case 'e':
			ip[1] = 'o';
			ncomp++;
		    }
	    }

	/* Link options. */
	link_static = 0;
	for (i=0;  i < nflags;  i++) {
	    arglist[nargs++] = lflags[i];
	    if (strcmp (lflags[i], F_STATIC) == 0)
		link_static = 1;
	    else if (strcmp (lflags[i], F_SHARED) == 0)
		link_static = 0;
	}

	/* File to link. */
	for (i=0;  i < nfiles;  i++)
	    arglist[nargs++] = lfiles[i];

	/* Libraries to link against.
	 */
	if (hostprog) {
	  arglist[nargs++] = mkfname (fortlib[0]);
	} else
	    arglist[nargs++] = mkfname (LIBMAIN);

	if (voslibs) {
	    if (usesharelib) {
		arglist[nargs++] = mkfname (SHARELIB);
		arglist[nargs++] = mkfname (IRAFLIB4);
		arglist[nargs++] = mkfname (IRAFLIB5);
		arglist[nargs++] = mkfname (IRAFLIB6);
	    } else {
		arglist[nargs++] = mkfname (IRAFLIB1);
		arglist[nargs++] = mkfname (IRAFLIB7);
		arglist[nargs++] = mkfname (IRAFLIB2);
		arglist[nargs++] = mkfname (IRAFLIB3);
		arglist[nargs++] = mkfname (IRAFLIB4);
		arglist[nargs++] = mkfname (IRAFLIB5);
		arglist[nargs++] = mkfname (IRAFLIB6);
	    }
	}

	/* Host libraries, searched after iraf libraries. */
	for (i=0;  i < nhlibs;  i++)
	    arglist[nargs++] = hlibs[i];

	/* The remaining system libraries depend upon which version of
	 * the SunOS compiler we are using. 
	 */
	addflags (fortlib[1], arglist, &nargs);
	addflags (fortlib[2], arglist, &nargs);
	addflags (fortlib[3], arglist, &nargs);
	addflags (fortlib[4], arglist, &nargs);
	addflags (fortlib[5], arglist, &nargs);
	addflags (fortlib[6], arglist, &nargs);
	addflags (fortlib[7], arglist, &nargs);
	addflags (fortlib[8], arglist, &nargs);
	addflags (fortlib[9], arglist, &nargs);
	arglist[nargs] = NULL;

	if (debug)
	    printargs (linker, arglist, nargs);

	/* If the link is successful, replace the old executable with the
	 * new one.  Do not delete the bad executable if the link fails,
	 * as we might want to examine its symbol table.
	 */
	if ((status = run (linker, arglist)) == 0) {
	    unlink (outfile);

	    if (link_nfs) {
		char command[1024];
		sprintf (command, "/bin/cp -f %s %s", tempfile, outfile);
		if (debug)
		    printargs (command, NULL, 0);
		status = sys (command);
	    } else
		link (tempfile, outfile);

	    /* Force the mode of the file. */
	    chmod (outfile, 0755);

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
	    if (os_sysfile (SHIMAGE, shlib, SZ_PATHNAME) > 0) {
		if (os_sysfile (EDSYM, edsym, SZ_PATHNAME) > 0) {
		    sprintf (command, "%s %s %s", edsym, outfile, shlib);
		    if (noshsym)
			strcat (command, " -T");
		    else if (notvsym)
			strcat (command, " -t");
		    status = sys (command);
		}
	    }
	}
	errflag += status;
	done (errflag);

	return (0);
}


/* MKFNAME -- Make the UNIX pathname of an IRAF library file.  Use os_sysfile
 * the get the vfn of the library file, so that we do not have to know what
 * system directory the library file is in.
 */
static char *
mkfname (char *i_fname)
{
	char fname[SZ_PATHNAME+1];
	char *oname;

	/* Library referenced as -lXXX */
	if (strncmp (i_fname, "-l", 2) == 0) {
	    sprintf (fname, "lib%s.a", &i_fname[2]);
	    if ((oname = iraflib (fname)))
		return (oname);
	    else
		return (i_fname);
	}

	/* Must be a library filename or pathname */
	strcpy (fname, i_fname);
	if ((oname = iraflib (fname)))
	    strcpy (libp, oname);
	else
	    strcpy (libp, fname);

	oname = libp;
	libp += strlen (libp) + 1;

	return (oname);
}


/* ADDFLAGS -- Add one or more flags to an argument list.  Ignore null flags,
 * separate multiple flags on whitespace.
 */
static int
addflags (char *flag, char *arglist[], int *p_nargs)
{
	register int i, len, nargs = *p_nargs;
	char *fp, *fs, lflag[SZ_FNAME];

	if (flag && *flag) {

	    for (fp = flag; *fp; ) {
		while (*fp && isspace(*fp)) 		/* skip leading space */
		    fp++;
		for (i=0; *fp && !isspace(*fp); ) 	/* collect flag       */
		    lflag[i++] = *fp++;
		lflag[i] = '\0';
		len = strlen (lflag);
		strcpy ((fs = malloc(len+1)), lflag);

	        if (strcmp (lflag, F_STATIC) == 0) {
		    link_static = 1;
	        } else if (strcmp (lflag, F_SHARED) == 0) {
		    link_static = 0;
	        } else if ((strcmp (lflag, "-lf2c") == 0) || 
	    	    (strcmp (lflag, "-lcompat") == 0)) {
		        /* Use the IRAF version of libf2c.a or libcompat.a,
		         * not the host version which may or may not be present.
		         */
		        arglist[nargs++] = mkfname (lflag);
		        *p_nargs = nargs;
		        return (1);
	        }
	        arglist[nargs++] = fs;
	    }

	    *p_nargs = nargs;
	    return (1);
	}

	return (0);
}


/* IRAFLIB -- Determine if "libname" is an IRAF library.  If so return
 * the pathname of the library, else return NULL.
 */
static char *
iraflib (char *libref)
{
	register char *ip, *op;
	char savename[SZ_PATHNAME+1];
	char libname[SZ_PATHNAME+1];
	char fname[SZ_PATHNAME+1];
	char path[SZ_PATHNAME+1];
	int foundit, dbg = dbgout;
	char *absname;

	strcpy (savename, libref);

	if (strncmp (libref, "-l", 2) == 0) { 
	    sprintf (libname, "lib%s.a", libref+2);
	    libref = libname;
	}

	/* If dbgout is enabled try the debug library first, but fall back
	 * to the normal library if the debug library is not found.
	 */
again:
	/* Position IP to EOS. */
	for (ip=libref;  *ip;  ip++)
	    ;

	if (!(*(ip-2) == '.' && *(ip-1) == 'a')) {
	    /* Not a library file, leave it alone.
	     */
	    strcpy (fname, libref);

	} else {
	    /* Normalize the library file name, "libXXX[_p].a".
	     */
	    for (ip=libref, op=fname;  (*op = *ip);  op++, ip++)
		;
	    if ((*(op-2) == '.' && *(op-1) == 'a')) {
		*(op-2) = '\0';
		op -= 2;
	    } else
		op -= 1;

	    if (dbg && !(*(op-2) == '_' && *(op-1) == 'p')) {
		*op++ = '_';
		*op++ = 'p';
	    }
	    *op++ = '.';
	    *op++ = 'a';
	    *op++ = '\0';
	}

	foundit = 0;
	if (access (fname, 0) == 0) {
	    strcpy (path, fname);
	    foundit++;
	} else {
	    if (os_sysfile (fname, path, SZ_PATHNAME) > 0)
		foundit++;
	}

	if (foundit) {
	    strcpy (absname=bp, vfn2osfn (path, 0));
	    bp += strlen (absname) + 1;
	    if (bp - buffer >= SZ_BUFFER)
		fatal ("Out of space for library names");
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
static void
printargs (char *cmd, char *arglist[], int nargs)
{
	int	i;

	fputs (cmd, stderr);
	for (i=1;  i < nargs;  i++)
	    fprintf (stderr, " %s", arglist[i]);
	putc ('\n', stderr);
	fflush (stderr);
}


/* XTOF -- Convert a ".x" file into a ".f" file, i.e., call up the preprocessor
 * to translate an SPP file into Fortran.
 */
static void
xtof (char *file)
{
	static  char xpp_path[SZ_PATHNAME+1], rpp_path[SZ_PATHNAME+1];
	char	cmdbuf[SZ_CMDBUF], fname[SZ_FNAME];


	lxfiles[nxfiles++] = file;
	if (nxfiles > MAXFILE)
	    fatal ("too many files");

	if (!xpp_path[0])
	    if (os_sysfile (XPP, xpp_path, SZ_PATHNAME) <= 0)
		strcpy (xpp_path, XPP);

	if (userincs) {
	    if (pkgenv)
	        sprintf (cmdbuf, "%s %s -A -R %s", xpp_path, pkgenv, file);
	    else
	        sprintf (cmdbuf, "%s -A -R %s", xpp_path, file);
	} else {
	    if (pkgenv)
	        sprintf (cmdbuf, "%s %s -R %s", xpp_path, pkgenv, file);
	    else
	        sprintf (cmdbuf, "%s -R %s", xpp_path, file);
	}


	if (foreigndefs) {
	    strcat (cmdbuf, " -h ");
	    strcat (cmdbuf, foreign_defsfile);
	}

	errflag |= sys (cmdbuf);
	chdot (file, 'r');

	strcpy (fname, file);
	chdot (fname, 'f');

	if (!rpp_path[0])
	    if (os_sysfile (RPP, rpp_path, SZ_PATHNAME) <= 0)
		strcpy (rpp_path, RPP);
	sprintf (cmdbuf, "%s %s%s >%s",
	    rpp_path, dbgout ? "-g " : "", file, fname);
	if (!(errflag & XPP_BADXFILE))
	    errflag |= sys (cmdbuf);

	unlink (file);			/* remove ".r" file */
	chdot (file, 'f');		/* change name to ".f" */
}


/* GETEXTN -- Get a one letter extension from a file name (BPS 07.23.96)
 */
static int
getextn (char *fname)
{
	register char *ip, *dot;
        int ch;

	for (ip=fname, dot=NULL;  *ip != EOS;  ip++)
	    if (*ip == '.')
		dot = ip;

	if (dot == NULL || *(dot+2) != EOS) {
	    ch = EOS;
	} else {
	    ch = *(dot+1);
	}

	return (ch);
}
	

/* CHDOT -- Change the filename extension, i.e., the single character
 * following the "." at the end of the filename, to the indicated character.
 */
static void
chdot (char *fname, char dotchar)
{
	char	*p;

	p = fname;
	while (*p++ != EOS)
	    ;
	while (*--p != '.' && p >= fname)
	    ;
	*(p+1) = dotchar;
}


/* RUN -- Send a command to UNIX and return the execution status to our
 * caller at the completion of the command.
 */
static int
run (char *task, char *argv[])
{
	int	waitpid;
	char	path[SZ_PATHNAME];

	if ((waitpid = fork()) == 0) {
	    enbint (SIG_DFL);

	    execvp (task, argv);	/* use user PATH for search */
	    strcpy (path, SYSBINDIR);
	    strcat (path, task);
	    execv  (path, argv);	/* look in SYSBINDIR */
	    strcpy (path, LOCALBINDIR);
	    strcat (path, task);
	    execv  (path, argv);	/* look in LOCALBINDIR */

	    fatalstr ("Cannot execute %s", task);
	}

	return (await (waitpid));
}


/*
 * Task execution and interrupt handling routines,
 * taken with minor modifications the F77 driver.
 */


/* SYS -- Execute a general UNIX command passed as a string.  The command may
 * contain i/o redirection metacharacters.  The full path of the command to
 * be executed should be given (and always is in the case of XC).
 */
static int
sys (char *cmd)
{
	register char *ip;
	char	*argv[256];
	char	*inname, *outname;
	int	append;
	int	waitpid;
	int	argc;

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
	    } else
		argv[argc++] = ip;
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
static void
done (int k)
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
static void
enbint (SIGFUNC handler)
{
	if (sig_int == 0)
	    signal (SIGINT, handler);
	if (sig_quit == 0)
	    signal (SIGQUIT, handler);
	if (sig_hup == 0)
	    signal (SIGHUP, handler);
	if (sig_term == 0)
	    signal (SIGTERM, handler);
}


/* INTERRUPT -- Exception handler, called if an interrupt is received
 * during compilation.
 */
static void
interrupt (void)
{
	done (2);
}


/* AWAIT -- Wait for an asynchronous child process to terminate.
 */
static int
await (int waitpid)
{
	int	w, status;

	enbint (SIG_IGN);
	while ((w = wait (&status)) != waitpid)
	    if (w == -1)
		fatal ("bad wait code");
	enbint ((SIGFUNC)interrupt);
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
static void
rmfiles (void)
{
	int	i;

	for (i=0;  i < nxfiles;  i++) {
	    chdot (lxfiles[i], 'f');
	    unlink (lxfiles[i]);
	}
}


/* FATALSTR -- Fatal error with an sprintf format and one string argument.
 */
static void
fatalstr (char *s1, char *s2)
{
	char	out[SZ_CMDBUF];

	sprintf (out, s1, s2);
	fatal (out);
}


/* FATAL -- A fatal error has occurred.  Print error message and terminate
 * process execution.
 */
static void
fatal (char *s)
{
	fprintf (stderr, "Fatal compiler error: %s\n", s);
	fflush (stderr);
	done (1);
}


/* FINDEXE -- Search for the named file and return the path if found, else
 * NULL.  If "dir" is non-NULL the directory in which the file resides is
 * returned in the string buffer pointed to.  The user's PATH is searched,
 * followed by SYSBINDIR, then LOCALBINDIR.
 */
static char *
findexe (
    char  *prog,		/* file to search for */
    char  *dir			/* pointer to output string buf, or NULL */
)
{
	register char *ip, *op;
	static	char path[SZ_PATHNAME];
	char	dirpath[SZ_PATHNAME];
	char	*dp = dir ? dir : dirpath;
	char	*pathp;

	/* Look for the program in the directories in the user's path.
	 */
	ip = pathp = os_getenv ("PATH");
	while (*ip) {
	    for (op=dp;  *ip && (*op = *ip++) != ':';  op++)
		;
	    *op++ = '/';
	    *op++ = EOS;
	    strcpy (path, dp);
	    strcat (path, prog);
	    if (access (path, 0) != -1)
		return (path);
	}

	/* Look in SYSBINDIR. */
	strcpy (dp, SYSBINDIR);
	strcpy (path, dp);
	strcat (path, prog);

	if (access (path, 0) != -1) {
	    static  char envpath[8192];
	    char    *oldpath;

	    /* Add SYSBINDIR to the user's path.  This is required to
	     * use the V1.3 compiler.  Note that this code should only be
	     * executed once, since the next time findexe is called the
	     * SYSBINDIR directory will be in the default path, above.
	     */
	    if ((oldpath = pathp)) {
		sprintf (envpath, "PATH=%s:%s", SYSBINDIR, oldpath);
		putenv (envpath);
	    }

	    return (path);
	}

	/* Look in LOCALBINDIR. */
	strcpy (dp, LOCALBINDIR);
	strcpy (path, dp);
	strcat (path, prog);
	if (access (path, 0) != -1)
	    return (path);

	/* Not found. */
	return (NULL);
}
