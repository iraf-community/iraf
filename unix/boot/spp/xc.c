/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include "xpp.h"

#define	NOKNET
#define	import_knames
#include <iraf.h>

/*
 * XC -- Main entry point of the SPP compiler for the IRAF subset preprocessor
 * language).
 */

#define	ERR		(-1)
#define	EOS		'\0'
#define	YES		1
#define	NO		0
#define	MAXFLAG		64		/* maximum option flags */
#define MAXFILE		1024		/* maximum files on command line */
#define	SZ_BUFFER	1024		/* library names, flags */
#define	SZ_LIBBUF	1024		/* full library names */
#define	SZ_FNAME	127
#define	SZ_PATHNAME	127

#define	DEBUGFLAG	'g'		/* what to use when -x is seen */
#define	XPP		"xpp"
#define	RPP		"rpp"
#define LIBMAIN		"libmain.o"
#define SHARELIB	"libshare.a"
#define IRAFLIB1	"libex.a"
#define IRAFLIB2	"libsys.a"
#define IRAFLIB3	"libvops.a"
#define IRAFLIB4	"libos.a"
#define	FORTLIB0	"-lU77"
#define FORTLIB1	"-lm"
#define FORTLIB2	"-lF77"
#define FORTLIB3	"-lI77"
#define FORTLIB4	"-lm"
#define	CC_COMMAND	"cc"
#define	F77_COMMAND	"f77"
#define	LD_COMMAND	"f77"

#ifdef SUNOS4
int	usesharelib = YES;
#else
int	usesharelib = NO;
#endif

#ifndef apollo
#define	U_USED				/* whether to use -u compilation flag */
#endif

int	noedsym = NO;
int	stripexe = NO;
int	notvsym = NO;
int	noshsym = NO;
int	errflag = NO;
int	objflags = NO;
int	keepfort = NO;
int	mkobject = YES;
int	mktask = YES;
int	optimize = YES;
int	cflagseen = NO;
int	nfileargs = 0;
char	outfile[SZ_FNAME] = "";
char	tempfile[SZ_FNAME] = "";
char	*lflags[MAXFLAG+1];
char	*lfiles[MAXFILE+1];			/* all files		*/
char	*lxfiles[MAXFILE+1];			/* .x files		*/
char	*lffiles[MAXFILE+1];			/* .f files		*/
char	buffer[SZ_BUFFER+1];
char	libbuf[SZ_LIBBUF+1];
char	*bp = buffer;
char	*libp = libbuf;
char	*pkgenv = NULL;
char	v_pkgenv[SZ_FNAME+1];
int	nflags, nfiles, nxfiles, nffiles;
int	sig_int, sig_quit, sig_hup, sig_term;
char	*shellname = "/bin/sh";
int	pid;
int	debug = NO;
int	hostprog;
int	foreigndefs;
char	*foreign_defsfile = "";

char	*vfn2osfn();
char	*os_getenv();


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
main (argc, argv)
int	argc;
char	*argv[];
{
	int	i, j, interrupt(), nargs, ncomp;
	char	cmdbuf[100];
	char	*arglist[MAXFILE+MAXFLAG+10];
	char	*arg, *ip, *iraflib(), *mkfname();
	int	status, noperands;

	ZZSTRT();

	nflags = nfiles = nxfiles = nffiles = 0;

	sig_int  = (int) signal (SIGINT, SIG_IGN) & 01;
	sig_quit = (int) signal (SIGQUIT,SIG_IGN) & 01;
	sig_hup  = (int) signal (SIGHUP, SIG_IGN) & 01;
	sig_term = (int) signal (SIGTERM,SIG_IGN) & 01;

	enbint (interrupt);
	pid = getpid();

	/* Count the number of file arguments.  Load the environment for
	 * any packages names on the command line.
	 */
	for (i=1, nfileargs=0;  argv[i] != NULL;  i++)
	    if (argv[i][0] != '-')
		nfileargs++;
	    else if (strcmp (argv[i], "-p") == 0 && argv[i+1])
		loadpkgenv (pkgenv = argv[++i]);

	/* If no package environment was specified see if the user has
	 * specified a default package in their user environment.
	 */
	if (!pkgenv)
	    if (pkgenv = os_getenv ("PKGENV")) {
		strcpy (v_pkgenv, pkgenv);
		loadpkgenv (pkgenv = v_pkgenv);
	    }

	/* Process command line options, make file lists.
	 * Convert ".x" files to ".f".
	 */
	for (i=1;  (arg = argv[i]) != NULL;  i++)
	    if (arg[0] == '-') {
		switch (arg[1]) {
		case '/':
		    /* Pass flag on without further interpretation.
		     *     -/hostflag
		     */
		    lflags[nflags] = bp;
		    *bp++ = '-';
		    for (ip = &arg[2];  (*bp++ = *ip++);  )
			;
		    if (nflags++ >= MAXFLAG)
			fatal ("Too many compiler options");
		    break;

		case 'l':
		    if ((lfiles[nfiles] = iraflib (&arg[2])) == NULL)
		        lfiles[nfiles] = arg;
		    nfiles++;
		    if (nxfiles > MAXFILE)
			fatal ("Too many files");
		    objflags = YES;
		    mkobject = YES;
		    mktask = YES;
		    break;

		case 'o':
		    /* Set output file name. */
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
		    if ((arg = argv[++i]) == EOS)
			i--;
		    break;

		case 'h':
		    /* Host program: do not link in IRAF main or search
		     * standard IRAF libraries unless explicitly referenced
		     * on command line.
		     */
		    hostprog++;
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

		    lflags[nflags] = bp;
		    *bp++ = '-';

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
	
	if (!mkobject) {
	    if (debug) {
		fprintf (stderr, "quit, fortran only\n");
		fflush (stderr);
	    }
	    ZZSTOP();
	    exit (errflag);
	}


	/* Compile all F77 source files with F77 to produce object code.
	 * This compilation is separate from that used for the '.x' files,
	 * because we do not want to use the UNIX "-u" flag (requires that
	 * everything be declared) for raw Fortran files.
	 */
	nargs = 0;
	arglist[nargs++] = F77_COMMAND;
	arglist[nargs++] = "-c";
	if (optimize)
	    arglist[nargs++] = "-O";

	for (i=0;  i < nflags;  i++)
	    arglist[nargs++] = lflags[i];
	
	for (i=0;  i < nffiles;  i++)
	    arglist[nargs++] = lffiles[i];
	arglist[nargs] = NULL;

	if (i > 0) {
	    if (debug)
		printargs (F77_COMMAND, arglist, nargs);
	    errflag += run (F77_COMMAND, arglist);
	}


	/* Compile the remaining Fortran source files with F77 to produce
	 * object code.
	 */
	nargs = 0;
	arglist[nargs++] = F77_COMMAND;
	arglist[nargs++] = "-c";
#ifdef	U_USED
	arglist[nargs++] = "-u";
#endif
	if (optimize)
	    arglist[nargs++] = "-O";

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
		printargs (F77_COMMAND, arglist, nargs);
	    errflag += run (F77_COMMAND, arglist);
	}


	/* Compile the remaining non-Fortran source files with CC to produce
	 * object code.
	 */
	nargs = 0;
	arglist[nargs++] = CC_COMMAND;
	arglist[nargs++] = "-c";
	if (optimize)
	    arglist[nargs++] = "-O";

	for (i=0;  i < nflags;  i++)
	    arglist[nargs++] = lflags[i];
	
	/* Make list of files to be compiled.  Do not include any Fortran
	 * files, as they were already compiled above.
	 */
	for (i=0, noperands=0;  i < nfiles;  i++) {
	    if (!isffile (lfiles[i])) {
		arglist[nargs++] = lfiles[i];
		noperands++;
	    }
	}
	arglist[nargs] = NULL;

	if (noperands > 0) {
	    if (debug)
		printargs (CC_COMMAND, arglist, nargs);
	    errflag += run (CC_COMMAND, arglist);
	}


	/* If "-c" (compile only), or there was a compiler error, do not
	 * proceed with the link.
	 */
	if (!mktask || cflagseen || errflag)
	    done (errflag);


	/* Link the object files and libraries to produce the "-o" task.
	 */
	nargs = 0;
#ifdef apollo
	arglist[nargs++] = LD_COMMAND;
#else
	arglist[nargs++] = CC_COMMAND;
#endif
	arglist[nargs++] = "-o";

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

	for (i=0;  i < nflags;  i++)
	    arglist[nargs++] = lflags[i];
	
	for (i=0;  i < nfiles;  i++)
	    arglist[nargs++] = lfiles[i];

	if (hostprog) {
	    arglist[nargs++] = mkfname (FORTLIB0);
	} else {
	    arglist[nargs++] = mkfname (LIBMAIN);
	    if (usesharelib) {
		arglist[nargs++] = mkfname (SHARELIB);
		arglist[nargs++] = mkfname (IRAFLIB4);
	    } else {
		arglist[nargs++] = mkfname (IRAFLIB1);
		arglist[nargs++] = mkfname (IRAFLIB2);
		arglist[nargs++] = mkfname (IRAFLIB3);
		arglist[nargs++] = mkfname (IRAFLIB4);
	    }
	}

	arglist[nargs++] = mkfname (FORTLIB1);
	arglist[nargs++] = mkfname (FORTLIB2);
	arglist[nargs++] = mkfname (FORTLIB3);
	arglist[nargs++] = mkfname (FORTLIB4);
	arglist[nargs] = NULL;

	if (ncomp) {
	    fprintf (stderr, "link:\n");
	    fflush (stderr);
	}
	if (debug)
#ifdef apollo
	    printargs (LD_COMMAND, arglist, nargs);
#else
	    printargs (CC_COMMAND, arglist, nargs);
#endif

	/* If the link is successful, replace the old executable with the
	 * new one.  Do not delete the bad executable if the link fails,
	 * as we might want to examine its symbol table.
	 */
#ifdef apollo
	if ((status = run (LD_COMMAND, arglist)) == 0) {
#else
	if ((status = run (CC_COMMAND, arglist)) == 0) {
#endif
	    unlink (outfile);
	    link   (tempfile, outfile);
	    unlink (tempfile);
	}
	errflag += status;

	/* If we are linking against the iraf shared library and symbol editing
	 * has not been disabled, edit the symbol table of the new executable
	 * to provide symbols within the shared image.
	 */
	if (usesharelib && !noedsym && !stripexe) {
	    char    shlib[SZ_PATHNAME+1];
	    char    command[256];

	    if (os_sysfile ("S.e", shlib, SZ_PATHNAME) > 0) {
		sprintf (command, "edsym %s %s", outfile, shlib);
		if (noshsym)
		    strcat (command, " -T");
		else if (notvsym)
		    strcat (command, " -t");
		status = sys (command);
	    }
	}
	errflag += status;

	done (errflag);
}


/* MKFNAME -- Make the UNIX pathname of an IRAF library file.  Use os_sysfile
 * the get the vfn of the library file, so that we do not have to know what
 * system directory the library file is in.
 */
char *
mkfname (i_fname)
char	*i_fname;
{
	char	path[SZ_PATHNAME+1];
	char	fname[SZ_PATHNAME+1];
	char	*oname;

	if (strncmp (i_fname, "-l", 2) == 0) {
	    sprintf (fname, "lib%s.a", &i_fname[2]);
	    if (os_sysfile (fname, path, SZ_PATHNAME) <= 0)
		return (i_fname);
	} else {
	    strcpy (fname, i_fname);
	    if (os_sysfile (fname, path, SZ_PATHNAME) <= 0)
		sprintf (path, "iraf$lib/%s", fname);
	}

	strcpy (libp, vfn2osfn (path, 0));
	libp += strlen (oname = libp) + 1;

	return (oname);
}


/* IRAFLIB -- Determine if "libname" is an IRAF library.  If so return
 * the pathname of the library, else return NULL.
 */
char *
iraflib (libname)
char	*libname;
{
	char	fname[SZ_FNAME+1];
	char	path[SZ_PATHNAME+1];
	char	*absname;

	sprintf (fname, "lib%s.a", libname);
	if (os_sysfile (fname, path, SZ_PATHNAME) > 0) {
	    absname = bp;
	    strcpy (absname, vfn2osfn (path, 0));
	    bp += strlen (absname) + 1;
	    if (bp - buffer >= SZ_BUFFER)
		fatal ("Out of space for library names");
	    return (absname);
	} else
	    return (NULL);
}


/* PRINTARGS -- Echo a UNIX command on the standard error output.
 */
printargs (cmd, arglist, nargs)
char	*cmd;
char	*arglist[];
int	nargs;
{
	int	i;

	fputs (cmd, stderr);
	for (i=1;  i <= nargs;  i++)
	    fprintf (stderr, " %s", arglist[i]);
	putc ('\n', stderr);
	fflush (stderr);
}


/* XTOF -- Convert a ".x" file into a ".f" file, i.e., call up the preprocessor
 * to translate an SPP file into Fortran.
 */
xtof (file)
char	*file;
{
	static  char xpp_path[SZ_PATHNAME+1], rpp_path[SZ_PATHNAME+1];
	char	cmdbuf[100], fname[100];
	char	*mkfname();

	lxfiles[nxfiles++] = file;
	if (nxfiles > MAXFILE)
	    fatal ("too many files");

	if (nfileargs > 1 || mkobject) {
	    fprintf (stderr, "%s:\n", file);
	    fflush (stderr);
	}

	if (!xpp_path[0])
	    if (os_sysfile (XPP, xpp_path, SZ_PATHNAME) <= 0)
		strcpy (xpp_path, XPP);

	if (pkgenv)
	    sprintf (cmdbuf, "%s -p %s -R %s", xpp_path, pkgenv, file);
	else
	    sprintf (cmdbuf, "%s -R %s", xpp_path, file);

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
	sprintf (cmdbuf, "%s %s >%s", rpp_path, file, fname);
	if (!(errflag & XPP_BADXFILE))
	    errflag |= sys (cmdbuf);

	unlink (file);			/* remove ".r" file */
	chdot (file, 'f');		/* change name to ".f" */
}


/* ISXFILE -- Determine if the named file has a ".x" extension.
 */
isxfile (fname)
char	*fname;
{
	char	*p, *dot;

	for (p=fname, dot=NULL;  *p != EOS;  p++)
	    if (*p == '.')
		dot = p;
	if (dot != NULL && *(dot+1) == 'x')
	    return (YES);
	else
	    return (NO);
}


/* ISFFILE -- Determine if the named file has a ".f" extension.
 */
isffile (fname)
char	*fname;
{
	char	*p, *dot;

	for (p=fname, dot=NULL;  *p != EOS;  p++)
	    if (*p == '.')
		dot = p;
	if (dot != NULL && *(dot+1) == 'f')
	    return (YES);
	else
	    return (NO);
}


/* ISEFILE -- Determine if the named file has a ".e" extension.
 */
isefile (fname)
char	*fname;
{
	char	*p, *dot;

	for (p=fname, dot=NULL;  *p != EOS;  p++)
	    if (*p == '.')
		dot = p;
	if (dot != NULL && *(dot+1) == 'e')
	    return (YES);
	else
	    return (NO);
}
	

/* CHDOT -- Change the filename extension, i.e., the single character
 * following the "." at the end of the filename, to the indicated character.
 */
chdot (fname, dotchar)
char	*fname;
char	dotchar;
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
run (task, argv)
char	*task;
char	*argv[];
{
	int	waitpid, fork();
	char	*s, *t, path[50];

	s = path;
	t = "/usr/bin/";
	while (*t != EOS)
	    *s++ = *t++;
	for (t = task;  (*s++ = *t++) != EOS;  )
	    ;

	if ((waitpid = fork()) == 0) {
	    enbint (SIG_DFL);

	    execv (path+9, argv);	/* command */
	    execv (path+4, argv);	/* /bin/command */
	    execv (path  , argv);	/* /usr/bin/command */

	    fatalstr ("Cannot execute %s", path+9);
	}

	return (await (waitpid));
}


/*
 * Task execution and interrupt handling routines,
 * taken with minor modifications the F77 driver.
 */


/* SYS -- Execute a general UNIX command passed as a string.  The command may
 * contain i/o redirection metacharacters.
 */
sys (str)
char	*str;
{
	register char *s, *t;
	char	*argv[100], path[100];
	char	*inname, *outname;
	int	append;
	int	waitpid;
	int	argc;

	if (debug) {
	    fprintf (stderr, "debug: %s\n", str);
	    fflush (stderr);
	}

	inname  = NULL;
	outname = NULL;
	argv[0] = shellname;
	argc = 1;

	t = str;
	while (isspace (*t))
	    ++t;
	while (*t) {
	    if (*t == '<')
		inname = t+1;
	    else if (*t == '>') {
		if (t[1] == '>') {
		    append = YES;
		    outname = t+2;
		} else {
		    append = NO;
		    outname = t+1;
		}
	    } else
		argv[argc++] = t;
	    while ( !isspace (*t) && *t != '\0' )
		++t;
	    if (*t) {
		*t++ = '\0';
		while (isspace (*t))
		    ++t;
	    }
	}

	if (argc == 1)				/* no command */
	    return (-1);
	argv[argc] = 0;

	s = path;
	t = "/usr/bin/";
	while (*t)
	    *s++ = *t++;
	for (t = argv[1] ; *s++ = *t++ ; )
	    ;
	if ((waitpid = fork()) == 0) {
	    if (inname)
		freopen (inname, "r", stdin);
	    if (outname)
		freopen (outname, (append ? "a" : "w"), stdout);
	    enbint (SIG_DFL);

	    texec (path+9, argv);	/* command */
	    texec (path+4, argv);	/* /bin/command */
	    texec (path  , argv);	/* /usr/bin/command */

	    s = path;
	    t = "/usr/local/bin/";
	    while (*t)
		*s++ = *t++;
	    for (t = argv[1] ; *s++ = *t++ ; )
		;

	    texec (path  , argv);	/* /usr/local/bin/command */

	    fatalstr ("Cannot execute %s", path);
	}

	return (await (waitpid));
}


/* TEXEC -- Spawn a shell to execute with the given argument list.
 * Taken from the shell with minor modifications.
 */
texec (f, av)
char	*f;
char	**av;
{
	extern int errno;

	execv (f, av+1);

	if (errno == ENOEXEC) {
	    av[1] = f;
	    execv (shellname, av);
	    fatal ("No shell!");
	}
	if (errno == ENOMEM)
	    fatalstr ("%s: too large", f);
}


/* DONE -- Called at process shutdown to cleanup.  Primary action is to delete
 * the intermediate Fortran files, unless the -F flag was given on the command
 * line.
 */
done (k)
int	k;
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
enbint (handler)
int	(*handler)();
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
interrupt()
{
	done (2);
}


/* AWAIT -- Wait for an asynchronous child process to terminate.
 */
await (waitpid)
int	waitpid;
{
	int	w, status;
	extern	interrupt();

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
rmfiles()
{
	int	i;

	for (i=0;  i < nxfiles;  i++) {
	    chdot (lxfiles[i], 'f');
	    unlink (lxfiles[i]);
	}
}


/* FATALSTR -- Fatal error with an sprintf format and one string argument.
 */
fatalstr (s1, s2)
char	*s1, *s2;
{
	char	out[100];

	sprintf (out, s1, s2);
	fatal (out);
}


/* FATAL -- A fatal error has occurred.  Print error message and terminate
 * process execution.
 */
fatal (s)
char	*s;
{
	fprintf (stderr, "Fatal compiler error: %s\n", s);
	fflush (stderr);
	done (1);
}


/* ERROR -- Print a warning message but do not terminate the process.
 */
error (s)
char	*s;
{
	fprintf (stderr, "Error: %s\n", s);
	fflush (stderr);
}
