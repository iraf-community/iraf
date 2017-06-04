/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>

#define import_spp
#define import_error
#include <iraf.h>
#include "mkpkg.h"
#include "extern.h"
#include "../bootProto.h"

/*
 * HOST.C -- [MACHDEP] Special host interface routines required by the MKPKG
 * utility.
 */

#define	SZ_COPYBUF	4096
#ifndef SZ_CMD
#define	SZ_CMD		2048		/* max size OS command, see mkpkg.h */
#endif
#define	SZ_LIBPATH	512		/* path to library 		    */
#define	LIBRARIAN	"ar"
#define	LIBTOOL		"libtool"
#define	LIBFLAGS	"r"
#define	REBUILD		"ranlib"
#define	XC		"xc"
#define	INTERRUPT	SYS_XINT

extern	char *makeobj();
extern	char *vfn2osfn();
extern	char *getenv();

extern  void fatals (char *fmt, char *arg);

char	*resolvefname();
char	*mkpath();

int   h_updatelibrary (char *library, char *flist[], int totfiles, 
                char *xflags, char *irafdir);
int   h_rebuildlibrary (char *library);
int   h_incheck (char *file, char *dir);
int   h_outcheck (char *file, char *dir, int clobber);
void  h_getlibname (char *file, char *fname);
int   h_xc (char *cmd);
int   h_purge (char *dir);
int   h_copyfile (char *oldfile, char *newfile);
 
int   u_fcopy (char *old, char *new);
int   h_movefile (char *old, char *new);
int   u_fmove (char *old, char *new );
int   add_sources (char *cmd, int maxch, char *flist[], 
                int totfiles, int hostnames, int  *nsources);
int   add_objects (char *cmd, int maxch, char *flist[], 
                int totfiles, int hostnames);

char *makeobj (char *fname);
char *mkpath (char *module, char *directory, char *outstr);
char *resolvefname (char *fname);
int   h_direq (char *dir1, char *dir2);



/* H_UPDATELIBRARY -- Compile a list of source files and replace them in the
 * host library.  This is done by formatting a command for the XC compiler
 * and passing it to the host system.  Since XC is pretty much the same on
 * all systems, this should be close to portable.  Note that when we are
 * called we are not necessarily in the same directory as the library, but
 * we are always in the same directory as the files in the file list.
 * Note also that the file list may contain object files which cannot be
 * compiled, but which must be replaced in the library.
 */
int
h_updatelibrary (
    char   *library,		/* pathname of library		*/
    char   *flist[],		/* pointers to filename strings	*/
    int	    totfiles,		/* number of files in list	*/
    char   *xflags,		/* XC compiler flags		*/
    char   *irafdir 		/* iraf root directory		*/
)
{
	char	cmd[SZ_CMD+1], *args;
	int	exit_status, baderr, npass;
	int	nsources, nfiles, ndone, nleft;
	int	hostnames, status;
	char	libfname[SZ_PATHNAME+1];
	char   *lname = NULL;

	/* Get the library file name. */
	h_getlibname (library, libfname);
	lname = resolvefname(libfname);

	/*
	 * Compile the files.
	 * -------------------
	 */
	if (irafdir[0])
	    sprintf (cmd, "%s -r %s %s", XC, irafdir, xflags);
	else
	    sprintf (cmd, "%s %s", XC, xflags);

	if (debug)
	    strcat (cmd, " -d");
	if (dbgout)
	    strcat (cmd, " -x");

	/* Compute offset to the file list and initialize loop variables.
	 * Since the maximum command length is limited, only a few files
	 * are typically processed in each iteration.
	 */
	exit_status = OK;
	baderr = NO;
	args  = &cmd[strlen(cmd)];
	nleft = totfiles;
	ndone = 0;

	while (nleft > 0) {
	    /* Add as many filenames as will fit on the command line.
	     */
	    nfiles = add_sources (cmd, SZ_CMD, &flist[ndone], nleft,
		hostnames=NO, &nsources);

	    /* This should not happen.
	     */
	    if (nfiles <= 0) {
		printf ("OS command overflow; cannot compile files\n");
		fflush (stdout);
		exit_status = ERR;
		return 0;
	    }

	    if (verbose) {
		if (nsources > 0)
		    printf ("%s\n", cmd);
		else
		    printf ("file list contains only object files\n");
		fflush (stdout);
	    }

	    if (execute && nsources > 0)
		if ((status = os_cmd (cmd)) != OK) {
		    if (status == INTERRUPT)
			fatals ("<ctrl/c> interrupt %s", library);
		    if (!ignore)
			baderr++;
		    exit_status += status;
		}

	    /* Truncate command and repeat with the next few files.
	     */
	    (*args) = EOS;

	    ndone += nfiles;
	    nleft -= nfiles;
	}

	/* Do not update object modules in library if a compilation error
	 * occurred.  The object files will be left on disk and the user
	 * will rerun us after fixing the problem; the next time around we
	 * will see that the objects exist and are up to date, hence will
	 * not recompile them.  When all have been successfully compiled
	 * the library will be updated.
	 */
	if (baderr)
	    return 0;

	/*
	 * Update the library.
	 * ---------------------
	 */
	sprintf (cmd, "%s %s %s", LIBRARIAN, LIBFLAGS, resolvefname(libfname));

	/* Compute offset to the file list and initialize loop variables.
	 * Since the maximum command length is limited, only a few files
	 * are typically processed in each iteration.
	 */
	args  = &cmd[strlen(cmd)];
	nleft = totfiles;
	ndone = 0;

	for (npass=0; nleft > 0; npass++) {

	    /* Add as many filenames as will fit on the command line.  */
	    nfiles = add_objects (cmd, SZ_CMD, &flist[ndone], nleft,
		hostnames=NO);

	    /* This should not happen.  */
	    if (nfiles <= 0) {
		printf ("OS command overflow; cannot update library `%s'\n",
		    libfname);
		fflush (stdout);
		exit_status = ERR;
		return 0;
	    }

	    if (verbose) {
		printf ("%s\n", cmd);
		fflush (stdout);
	    }

	    if (execute) {
		if ((exit_status = os_cmd (cmd)) == OK) {
		    /* Delete the object files.
		     */
		    int	i;

		    for (i=0;  i < nfiles;  i++)
			os_delete (makeobj (flist[ndone+i]));
		} else if (exit_status == INTERRUPT)
		    fatals ("<ctrl/c> interrupt %s", library);
	    }

	    /* Truncate command and repeat with the next few files.
	     */
	    (*args) = EOS;

	    ndone += nfiles;
	    nleft -= nfiles;

	}

	return (exit_status);
}


/* H_REBUILDLIBRARY -- Called after all recently recompiled modules have been
 * replaced in the library.  When we are called we are in the same directory
 * as the library.
 */
int
h_rebuildlibrary (
    char  *library 		/* filename of library	*/
)
{
	char cmd[SZ_LINE+1];
	char libfname[SZ_PATHNAME+1];
	char *libpath;

	/* Get the library file name. */
	h_getlibname (library, libfname);
	libpath = resolvefname (vfn2osfn(libfname,0));

	sprintf (cmd, "%s %s", REBUILD, libpath);
	if (verbose) {
	    printf ("%s\n", cmd);
	    fflush (stdout);
	}

	if (execute)
	    return (os_cmd (cmd));
	else
	    return (OK);
}


/* H_INCHECK -- Check a file, e.g., a library, back into the directory it
 * was originally checked out from.  If the directory name pointer is NULL
 * merely delete the checked out copy of the file.  On a UNIX system the
 * checked out file is a symbolic link, so all we do is delete the link.
 * On a VMS system the checked out file is a copy, and we have to physically
 * copy the new file back, creating a new version of the original file.
 */
int
h_incheck (
    char  *file,		/* file to be checked in	*/
    char  *dir 			/* where to put the file	*/
)
{
	char    backup[SZ_PATHNAME+1];
	char    path[SZ_PATHNAME+1];
	char	fname[SZ_PATHNAME+1];
	char	*osfn, *ip;
	struct	stat fi;
	int	status;

	/* Get the library file name. */
	h_getlibname (file, fname);
	osfn = vfn2osfn (fname, 0);

	if (verbose) {
	    printf ("check file `%s' into `%s'\n", fname, dir ? dir : "");
	    fflush (stdout);
	}

	if (stat (osfn, &fi) == ERR) {
	    printf ("$checkin: file `%s' not found\n", osfn);
	    fflush (stdout);
	    return (ERR);
	}

	/* If the file is not a symbolic link to an existing remote file it
	 * is probably a new library, so move it to the destination directory,
	 * otherwise just delete the link.  If the named file exists in
	 * IRAFULIB update that version of the file instead of the standard one.
	 */
	if (dir != NULL && !(fi.st_mode & S_IFLNK)) {
	    path[0] = EOS;
	    if ((ip = getenv("IRAFULIB")))
		if (access (mkpath(fname,ip,path), 0) < 0)
		    path[0] = EOS;

	    if (path[0] == EOS)
		status = h_movefile (osfn, mkpath(fname,dir,path));
	    else
		status = h_movefile (osfn, path);

	} else
	    status = unlink (osfn);

	/* If there was a local copy of the file it will have been renamed
	 * with a .cko extension when the file was checked out, and should be
	 * restored.
	 */
	sprintf (backup, "%s.cko", fname);
	if (access (backup, 0) == 0) {
	    if (debug) {
		printf ("h_incheck: rename %s -> %s\n", backup, fname);
		fflush (stdout);
	    }
	    if (rename (backup, fname) == -1)
		printf ("cannot rename %s -> %s\n", backup, fname);
	}

	return (status);
}


/* H_OUTCHECK -- Check out a file, e.g., gain access to a library in the
 * current directory so that it can be updated.  If the file has already
 * been checked out do not check it out again.  In principle we should also
 * place some sort of a lock on the file while it is checked out, but...
 */
int
h_outcheck (
    char  *file,		/* file to be checked out		*/
    char  *dir,			/* where to get the file		*/
    int	   clobber 		/* clobber existing copy of file?	*/
)
{
	register char	*ip, *op;
	char path[SZ_PATHNAME+1];
	char fname[SZ_PATHNAME+1];

	/* Get the library file name. */
	h_getlibname (file, fname);

	/* Make the UNIX pathname of the destination file.	[MACHDEP]
	 * Use the IRAFULIB version of the file if there is one.
	 */
	path[0] = EOS;
	if ((ip = getenv("IRAFULIB")))
	    if (access (mkpath(fname,ip,path), 0) < 0)
		path[0] = EOS;

	if (path[0] == EOS) {
	    for (ip=vfn2osfn(dir,0), op=path;  (*op = *ip++);  op++)
		;
	    if (*(op-1) != '/')
		*op++ = '/';
	    for (ip=vfn2osfn(fname,0);  (*op = *ip++);  op++)
		;
	    *op = EOS;
	}

	if (verbose) {
	    printf ("check out file `%s = %s'\n", fname, path);
	    fflush (stdout);
	}

	/* If the file already exists and clobber is enabled, delete it.
	 * If the file is a symbolic link (a pathname), and IRAF has been
	 * moved since the link was created, then the symlink will be
	 * pointing off into never never land and must be redone.  If clobber
	 * is NOT enabled, then probably the remote copy of the file is an
	 * alternate source for the local file, which must be preserved.
	 */
	if (access (fname, 0) != -1) {
	    char    backup[SZ_PATHNAME+1];

	    if (clobber) {
		if (debug) {
		    printf ("h_outcheck: deleting %s\n", fname);
		    fflush (stdout);
		}
		unlink (fname);
	    } else {
		/* Do not rename the file twice; if the .cko file already
		 * exists, the second time would clobber it.  Note that if a
		 * mkpkg run is aborted, the checked out file and renamed
		 * local file will remain, but a subsequent successful mkpkg
		 * will restore everything.
		 */
		sprintf (backup, "%s.cko", fname);
		if (access (backup, 0) == -1) {
		    if (debug) {
			printf ("h_outcheck: rename %s -> %s\n", fname, backup);
			fflush (stdout);
		    }
		    if (rename (fname, backup) == -1)
			printf ("cannot rename %s -> %s\n", fname, backup);
		}
	    }
	}

	return (symlink (path, fname));
}


/* H_GETLIBNAME -- Get a library filename.  If debug output is enabled (-g
 * or -x), and we are checking out a library file (.a), update the debug
 * version of the library (XX_p.a).
 */
void
h_getlibname (
    char  *file,
    char  *fname
)
{
	register char *ip;

	strcpy (fname, file);
	if (dbgout) {
	    for (ip=fname;  *ip;  ip++)
		;
	    if (*(ip-2) == '.' && *(ip-1) == 'a' &&
		    !(*(ip-4) == '_' && *(ip-3) == 'p')) {
		*(ip-2) = '_';
		*(ip-1) = 'p';
		*(ip-0) = '.';
		*(ip+1) = 'a';
		*(ip+2) = '\0';
	    }
	}
}


/* H_XC -- Host interface to the XC compiler.  On UNIX all we do is use the
 * oscmd facility to pass the XC command line on to UNIX.
 */
int
h_xc (char *cmd)
{
	return (os_cmd (cmd));
}


/* H_PURGE -- Purge all old versions of all files in the named directory.
 * This is a no-op on UNIX since multiple file versions are not supported.
 */
int
h_purge (
    char  *dir		/* LOGICAL directory name */
)
{
	if (verbose) {
	    printf ("purge directory `%s'\n", dir);
	    fflush (stdout);
	}

	/*
	 * format command "purge [dir]*.*;*"
	 * if (verbose)
	 *     echo command to stdout
	 * if (execute)
	 *     call os_cmd to execute purge command
	 */

	 return (OK);
}


/* H_COPYFILE -- Copy a file.  If the new file already exists it is
 * clobbered (updated).
 */
int
h_copyfile (
    char  *oldfile,		/* existing file to be copied	*/
    char  *newfile 		/* new file, not a directory name */
)
{
	char	old[SZ_PATHNAME+1];
	char	new[SZ_PATHNAME+1];

	strcpy (old, vfn2osfn (oldfile, 0));
	strcpy (new, vfn2osfn (newfile, 1));

	if (verbose) {
	    printf ("copy %s to %s\n", old, new);
	    fflush (stdout);
	}

	if (execute) {
	    if (os_access (old, 0,0) == NO) {
		printf ("$copy: file `%s' not found\n", oldfile);
		fflush (stdout);
		return (ERR);
	    } else
		return (u_fcopy (old, new));
	}

	return (OK);
}


/* U_FCOPY -- Copy a file, UNIX.
 */
int
u_fcopy (
    char  *old,
    char  *new
)
{
	char	buf[SZ_COPYBUF], *ip;
	int	in, out, nbytes;
	struct	stat fi;
	long	totbytes;

	/* Open the old file and create the new one with the same mode bits
	 * as the original.
	 */
	if ((in  = open(old,0)) == ERR || fstat(in,&fi) == ERR) {
	    printf ("$copy: cannot open input file `%s'\n", old);
	    fflush (stdout);
	    return (ERR);
	} if ((out = creat(new,0644)) == ERR || fchmod(out,fi.st_mode) == ERR) {
	    printf ("$copy: cannot create output file `%s'\n", new);
	    fflush (stdout);
	    close (in);
	    return (ERR);
	}

	/* Copy the file.
	 */
	totbytes = 0;
	while ((nbytes = read (in, buf, SZ_COPYBUF)) > 0)
	    if (write (out, buf, nbytes) == ERR) {
		close (in); close (out);
		printf ("$copy: file write error on `%s'\n", new);
		fflush (stdout);
		return (ERR);
	    } else
		totbytes += nbytes;

	close (in);
	close (out);

	/* Check for premature termination of the copy.
	 */
	if (totbytes != fi.st_size) {
	    printf ("$copy: file changed size `%s' oldsize=%d, newsize=%d\n",
		old, (int)fi.st_size, (int)totbytes);
	    fflush (stdout);
	    return (ERR);
	}

	/* If file is a library (".a" extension in UNIX), preserve the
	 * modify date else UNIX will think the library symbol table is
	 * out of date.
	 */
	for (ip=old;  *ip;  ip++)
	    ;
	ip -= 2;
	if (ip > old && strcmp (ip, ".a") == 0) {
	    struct  timeval tv[2];

	    tv[0].tv_sec = fi.st_atime;
	    tv[1].tv_sec = fi.st_mtime;
	    utimes (new, tv);
	}

	return (OK);
}


/* H_MOVEFILE -- Move a file from the current directory to another directory,
 * or rename the file within the current directory.  If the destination file
 * already exists it is clobbered.
 */
int
h_movefile (
    char  *old,		/* file to be moved		*/
    char  *new 		/* new pathname of file		*/
)
{
	char	old_osfn[SZ_PATHNAME+1];
	char	new_osfn[SZ_PATHNAME+1];

	strcpy (old_osfn, vfn2osfn (old, 0));
	strcpy (new_osfn, vfn2osfn (new, 0));

	if (debug) {
	    printf ("move %s to %s\n", old_osfn, new_osfn);
	    fflush (stdout);
	}

	if (execute) {
	    if (os_access (old_osfn, 0,0) == NO) {
		printf ("$move: file `%s' not found\n", old);
		fflush (stdout);
		return (ERR);
	    } else
		return (u_fmove (old_osfn, new_osfn));
	}

	return (OK);
}


/* U_FMOVE -- Unix procedure to move or rename a file.  Will move file to a
 * different device (via a file copy) if necessary.
 */
int
u_fmove (
    char  *old,
    char  *new 
)
{
	unlink (new);
	if (link (old, new) == ERR)
	    if (u_fcopy (old, new) == ERR) {
		printf ("$move: cannot create `%s'\n", new);
		fflush (stdout);
		return (ERR);
	    }

	if (unlink (old) == ERR) {
	    printf ("$move: cannot unlink `%s'\n", old);
	    fflush (stdout);
	    return (ERR);
	}

	return (OK);
}


/* ADD_SOURCES -- Append source files from the file list to the command
 * buffer.  Omit object files.  Return a count of the number of files to
 * be compiled.  This code is machine dependent since Unix permits arbitrarily
 * long command lines, but most systems do not, in which case something
 * else must be done (e.g., write a command file and have the host system
 * process that).
 */
int
add_sources (
    char *cmd,			/* concatenate to this		*/
    int	  maxch,			/* max chars out		*/
    char *flist[],		/* pointers to filename strings	*/
    int	  totfiles,		/* number of files in list	*/
    int	  hostnames,		/* return host filenames?	*/
    int	 *nsources 		/* receives number of src files */
)
{
	register char	*ip, *op, *otop;
	register int	i;
	int	nfiles;

	*nsources = 0;
	nfiles    = 0;

	otop = &cmd[maxch];
	for (op=cmd;  *op;  op++)
	    ;

	for (i=0;  i < totfiles;  i++) {
	    /* Skip over object files.
	     */
	    for (ip=flist[i];  *ip;  ip++)
		;
	    if (strcmp (ip-2, ".o") == 0) {
		nfiles++;
		continue;
	    }

	    if (op + strlen (flist[i]) + 1 >= otop)
		break;

	    nfiles++;
	    (*nsources)++;
	    *op++ = ' ';

	    if (hostnames)
		ip = vfn2osfn (flist[i], 0);
	    else
		ip = flist[i];

	    for (;  (*op = *ip++);  op++)
		;
	}

	return (nfiles);
}


/* ADD_OBJECTS -- Append the ".o" equivalent of each file name to the
 * output command buffer.  Return the number of file names appended.
 */
int
add_objects (
    char *cmd,			/* concatenate to this		*/
    int	  maxch,		/* max chars out		*/
    char *flist[],		/* pointers to filename strings	*/
    int	  totfiles,		/* number of files in list	*/
    int	  hostnames 		/* return host filenames?	*/
)
{
	register char	*ip, *op, *otop;
	register int	i;
	int	nfiles;

	otop = &cmd[maxch];
	for (op=cmd;  *op;  op++)
	    ;

	for (i=0, nfiles=0;  i < totfiles;  i++) {
	    if (op + strlen (flist[i]) + 1 >= otop)
		break;

	    nfiles++;
	    *op++ = ' ';

	    ip = makeobj (flist[i]);
	    if (hostnames)
		ip = vfn2osfn (ip,0);

	    for (;  (*op = *ip++);  op++)
		;
	}

	return (nfiles);
}


/* MAKEOBJ -- Return a pointer to the ".o" equivalent of the input file
 * name.  The last period in the input filename is assumed to delimit the
 * filename extension.
 */
char *
makeobj (char *fname)
{
	register char	*ip, *op;
	static	char objfile[SZ_FNAME+1];
	char	*lastdot;

	for (ip=fname, op=objfile, lastdot=NULL;  (*op = *ip++);  op++)
	    if (*op == '.')
		lastdot = op;

	if (lastdot != NULL)
	    op = lastdot;
	strcpy (op, ".o");

	return (objfile);
}


/* MKPATH -- Given a module name and a directory name, return the pathname of
 * the module in the output string.  Do not use the directory pathname if the
 * module name is already a pathname.
 */
char *
mkpath (
    char  *module,
    char  *directory,
    char  *outstr
)
{
	register char	*ip, *op;

	if (directory && module[0] != '/') {
	    for (ip=directory, op=outstr;  (*op = *ip++);  op++)
		;
	    if (op > outstr && *(op-1) != '/') {
		*op++ = '/';
		*op = EOS;
	    }
	    for (ip=module;  (*op = *ip++);  op++)
		;
	} else
	    strcpy (outstr, module);

	return (outstr);
}


/* RESOLVEFNAME -- If a filename reference is a symbolic link resolve it to
 * the pathname of an actual file by tracing back through all symbolic links
 * to the fully resolved file or path.
 * 
 * Example:
 *
 *     ./libsys.a -> /iraf/iraf/lib/libsys.a
 *        /iraf/iraf/lib/libsys.a -> ../bin/libsys.a
 *	    -> /iraf/iraf/bin/libsys.a
 *
 * Note that the "fully resolved" filename may still contain unresolved links
 * for directory elements - it is only the filename which is fully resolved
 * in the output pathname.
 */
char *
resolvefname (char *fname)
{
	static char pathname[SZ_LIBPATH];
	char relpath[SZ_LIBPATH];
	extern char *strrchr();

	strcpy (pathname, fname);
	while (os_symlink (pathname, relpath, SZ_LIBPATH)) {
	    if (relpath[0] == '/') {
		/* Link to an absolute pathname, just use new path. */
		strcpy (pathname, relpath);
	    } else {
		/* Relative path.  This includes upwards references such
		 * as ../foo.  Replace the filename by the relative path.
		 * Let unix resolve any upwards references later, when the
		 * file is accessed.
		 */
                char *str = strrchr(pathname,'/');
                strcpy ((str ? (str+1) : pathname), relpath);
	    }
	}

	return (pathname);
}


/* H_DIREQ -- Compare two directory pathnames for equality.  This is easy
 * in most cases, but the comparison can fail when it shouldn't due to aliases
 * for directory names, e.g., a directory may be referred to by a symbolic
 * name, but get-cwd will return a different path, causing the comparison to
 * fail.
 */
int
h_direq (char *dir1, char *dir2)
{
	register char	*ip1, *ip2;

	/* If the pathname contains a directory named "irafXXX" (where the
	 * XXX are optional characters in the directory name) everything to
	 * the left for the purposes of this comparision.  This allows the
	 * iraf root directory to be specified with a path such as
	 *
	 *		/<whatever>/iraf/iraf.version/
	 *
	 * and the directory name comparision will take place using only
	 * the portion of the path following this prefix.
	 */
	for (ip1=dir1;  *ip1;  ip1++)
	    if (*ip1 == '/' && *(ip1+1) == 'i')
		if (strncmp (ip1+1, "iraf", 4) == 0) {
		    for (ip1++;  *ip1 && *ip1 != '/';  ip1++)
			;
		    if (*ip1 == '/')
			dir1 = ip1 + 1;
		    --ip1;
		}
	for (ip2=dir2;  *ip2;  ip2++)
	    if (*ip2 == '/' && *(ip2+1) == 'i')
		if (strncmp (ip2+1, "iraf", 4) == 0) {
		    for (ip2++;  *ip2 && *ip2 != '/';  ip2++)
			;
		    if (*ip2 == '/')
			dir2 = ip2 + 1;
		    --ip2;
		}

	return (strcmp (dir1, dir2) == 0);
}
