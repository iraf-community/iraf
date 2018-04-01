/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#define	NOKNET
#define	import_spp
#define	import_finfo
#define	import_knames
#include <iraf.h>

#include "../bootProto.h"


/*
 * WTAR -- Write a UNIX tar format file (on disk, tape, or to stdout)
 *
 * Switches:
 *		f	write to named file, otherwise write to stdout
 *		t	print name of each file written
 *		v	verbose; print full description of each file
 *		d	print debug messages
 *		o	omit binary files (e.g. when foreign host has
 *			  incompatible binary file format)
 */

#define TBLOCK		512
#define NBLOCK		20
#define NAMSIZ		100
#define	MAXERR		20
#define	MAXTRYS		100
#define	SZ_TAPEBUFFER	(TBLOCK * NBLOCK)
#define	RWXR_XR_X	0755

#define	LF_LINK		1
#define	LF_SYMLINK	2
#define	LF_DIR		5

/* File header structure.  One of these precedes each file on the tape.
 * Each file occupies an integral number of TBLOCK size logical blocks
 * on the tape.  The number of logical blocks per physical block is variable,
 * with at most NBLOCK logical blocks per physical tape block.  Two zero
 * blocks mark the end of the tar file.
 */
union hblock {
	char dummy[TBLOCK];
	struct header {
		char name[NAMSIZ];	/* NULL delimited		*/
		char mode[8];		/* octal, ascii			*/
		char uid[8];
		char gid[8];
		char size[12];
		char mtime[12];
		char chksum[8];
		char linkflag;
		char linkname[NAMSIZ];
	} dbuf;
};

/* Decoded file header.
 */
struct fheader {
	char	name[NAMSIZ];
	int	mode;
	int	uid;
	int	gid;
	int	isdir;
	long	size;
	long	mtime;
	long	chksum;
	int	linkflag;
	char	linkname[NAMSIZ];
};

/* Map TAR file mode bits into characters for printed output.
 */
struct _modebits {
    int	  code;
    char  ch;
} modebits[] = {
    { 040000,	'd' },
    { 0400,	'r' },
    { 0200,	'w' },
    { 0100,	'x' },
    { 040,	'r' },
    { 020,	'w' },
    { 010,	'x' },
    { 04,	'r' },
    { 02,	'w' },
    { 01,	'x' },
    { 0,	0   }
};

int	debug=NO;		/* Print debugging messages		*/
int	omitbinary;		/* omit binary files 			*/
int	printfnames;		/* Print file names			*/
int	verbose;		/* Print everything			*/

struct	fheader *curfil;
int	nerrs;
char	*first_file;
char	tapeblock[SZ_TAPEBUFFER];
char	*nextblock = NULL;
int	nblocks;
int	in;
int	out = EOF;


extern  int ZZSTRT (void);
extern  int ZZSTOP (void);
extern  int ZFINFO (PKCHAR *fname, XLONG *finfo_struct, XINT *status);

extern  int tape_open (char *fname, int mode);
extern  int tape_close (int fd);
extern  int tape_write (int fd, char *buf, int nbytes);


static void  putfiles (char *dir, int   out, char *path);
static void  tarfileout (char *fname, int out, int ftype, char *path);
static int   putheader (register struct fheader *fh, int out);
static int   cchksum (register char *p, register int nbytes);
static void  printheader (FILE *fp, register struct fheader *fh, int verbose);
static void  copyfile (char *fname, struct fheader *fh, int ftype, int out);
static int   putblock (int out, char *buf);
static void  endtar (int out);
static int   u_fmode (int iraf_fmode, int ftype);
static char *dname (char *dir);




/* MAIN -- "wtar [-tvdo] [-f tarfile] [files]".  If no files are listed the
 * current directory tree is used as input.  If no output file is specified
 * output is to the standard output.
 */
int main (int argc, char *argv[])
{
	static	char	*def_flist[2] = { ".", NULL };
	char	*argp, **flist;
	int	argno, ftype, i;

	ZZSTRT();

	flist       = def_flist;
	omitbinary  = NO;
	printfnames = debug;
	verbose     = debug;

	if (debug) {
	    printf ("wtar called with %d arguments:", argc);
	    for (argno=1;  (argp = argv[argno]) != NULL;  argno++)
		printf (" %s", argp);
	    printf ("\n");
	}

	/* Process the argument list.
	 */
	for (argno=1;  (argp = argv[argno]) != NULL;  argno++) {
	    if (*argp != '-') {
		flist = &argv[argno];
		break;

	    } else {
		for (argp++;  *argp;  argp++) {
		    switch (*argp) {
		    case 'd':
			debug++;
			printfnames++;
			verbose++;
			break;
		    case 't':
			printfnames++;
			break;
		    case 'v':
			printfnames++;
			verbose++;
			break;
		    case 'o':
			omitbinary++;
			break;

		    case 'f':
			if (argv[argno+1]) {
			    argno++;
			    if (debug)
				printf ("open output file `%s'\n", argv[argno]);
			    out = tape_open (argv[argno], 1);
			    if (out == ERR) {
				fflush (stdout);
				fprintf (stderr,
				    "cannot open `%s'\n", argv[argno]);
				ZZSTOP();
				exit (OSOK+1);
			    }
			}
			break;

		    default:
			fflush (stdout);
			fprintf (stderr,
			    "Warning: unknown switch -%c\n", *argp);
			fflush (stderr);
		    }
		}
	    }
	}

	/* Write to the standard output if no output file specified.
	 * The filename "stdin" is reserved.
	 */
	if (out == ERR) {
	    if (debug)
		printf ("output defaults to stdout\n");
	    out = tape_open ("stdout", 1);
	}

	nextblock = tapeblock;
	nblocks = 0;

	/* Put each directory and file listed on the command line to 
	 * the tarfile.
	 */
	for (i=0;  (argp = flist[i]) != NULL;  i++)
	    if ((ftype = os_filetype (argp)) == DIRECTORY_FILE)
		putfiles (argp, out, "");
	    else
		tarfileout (argp, out, ftype, "");

	/* Close the tarfile.
	 */
	endtar (out);
	tape_close (out);

	ZZSTOP();
	exit (OSOK);

	return (0);
}


/* PUTFILES -- Put the named directory tree to the output tarfile.  We chdir
 * to each subdirectory to minimize path searches and speed up execution.
 */
static void
putfiles (
    char *dir,			/* directory name		*/
    int	  out,			/* output file			*/
    char *path 			/* pathname of curr. directory	*/
)
{
	char	newpath[SZ_PATHNAME+1];
	char	oldpath[SZ_PATHNAME+1];
	char	fname[SZ_PATHNAME+1];
	int	ftype, dp;

	if (debug)
	    printf ("putfiles (%s, %d, %s)\n", dir, out, path);

	/* Put the directory file itself to the output as a file.
	 */
	tarfileout (dir, out, DIRECTORY_FILE, path);

	if ((dp = os_diropen (dir)) == ERR) {
	    fflush (stdout);
	    fprintf (stderr, "cannot open subdirectory `%s%s'\n", path, dir);
	    fflush (stderr);
	    return;
	}

	os_fpathname (".", oldpath, SZ_PATHNAME);
	sprintf (newpath, "%s%s", dname(path), dir);
	strcpy (newpath, dname(newpath));

	if (debug)
	    printf ("change directory to %s\n", newpath);
	if (os_chdir (dir) == ERR) {
	    os_dirclose (dp);
	    fflush (stdout);
	    fprintf (stderr, "cannot change directory to `%s'\n", newpath);
	    fflush (stderr);
	    return;
	}

	/* Put each file in the directory to the output file.  Recursively
	 * read any directories encountered.
	 */
	while (os_gfdir (dp, fname, SZ_PATHNAME) > 0)
	    if (os_symlink (fname, 0, 0))
		tarfileout (fname, out, LF_SYMLINK, newpath);
	    else if ((ftype = os_filetype (fname)) == DIRECTORY_FILE)
		putfiles (fname, out, newpath);
	    else
		tarfileout (fname, out, ftype, newpath);

	if (debug)
	    printf ("return from subdirectory %s\n", newpath);
	if (os_chdir (oldpath) == ERR) {
	    fflush (stdout);
	    fprintf (stderr, "cannot return from subdirectory `%s'\n", newpath);
	    fflush (stderr);
	}

	os_dirclose (dp);
}


/* TARFILEOUT -- Write the named file to the output in tar format.
 */
static void
tarfileout (
    char *fname,			/* file to be output	*/
    int	  out,				/* output stream	*/
    int	  ftype,			/* file type		*/
    char *path				/* current path		*/
)
{
	struct	_finfo fi;
	struct	fheader fh;
	int	status;

	if (debug)
	    printf ("put file `%s', type %d\n", fname, ftype);

	if (ftype == BINARY_FILE && omitbinary) {
	    if (printfnames) {
		fflush (stdout);
		fprintf (stderr, "omit binary file `%s'\n", fname);
		fflush (stderr);
	    }
	    return;
	}

	/* Get info on file to make file header.
	 */
	ZFINFO ((PKCHAR *)vfn2osfn(fname,0), (XLONG *) &fi, (XINT *) &status);
	if (status == XERR) {
	    fflush (stdout);
	    fprintf (stderr, "Warning: can't get info on file `%s'\n", fname);
	    fflush (stderr);
	    return;
	}

	/* Format and output the file header.
	 */
	memset (&fh, 0, sizeof(fh));
	strcpy (fh.name, path);
	strcat (fh.name, fname);
	strcpy (fh.linkname, "");
	fh.linkflag = 0;

	if (ftype == DIRECTORY_FILE) {
	    strcpy (fh.name, dname(fh.name));
	    fh.size  = 0;
	    fh.isdir = 1;
	    fh.linkflag = LF_DIR;
	} else {
	    fh.size  = fi.fi_size;
	    fh.isdir = 0;
	}

	os_getowner (fname, &fh.uid, &fh.gid);
	fh.mode  = u_fmode  (fi.fi_perm, fi.fi_type);
	fh.mtime = os_utime (fi.fi_mtime);

	if (ftype == LF_SYMLINK) {
	    struct  stat fi;
	    lstat (fname, &fi);

	    /* Set attributes of symbolic link, not file pointed to. */
	    fh.uid   = fi.st_uid;
	    fh.gid   = fi.st_gid;
	    fh.mode  = fi.st_mode;
	    fh.mtime = fi.st_mtime;
	    fh.size  = 0;

	    fh.linkflag = LF_SYMLINK;
	    os_symlink (fname, fh.linkname, NAMSIZ);
	}

	if (putheader (&fh, out) == EOF)  {
	    fflush (stdout);
	    fprintf (stderr, 
		"Warning: could not write file header for `%s'\n", fname);
	    fflush (stderr);
	    return;
	}

	/* Copy the file data.
	 */
	if (fh.size > 0 && !fh.isdir && !fh.linkflag)
	    copyfile (fname, &fh, ftype, out);

	if (printfnames) {
	    printheader (stdout, &fh, verbose);
	    fflush (stdout);
	}
}


/* PUTHEADER -- Encode and write the file header to the output tarfile.
 */
static int
putheader (
    register struct fheader *fh,	/* (input) file header		*/
    int	 out 				/* output file descriptor	*/
)
{
	register char	*ip;
	register int	n;
	union	hblock	hb;
	char	chksum[10];


	/* Clear the header block. */
	for (n=0;  n < TBLOCK;  n++)
	    hb.dummy[n] = '\0';

	/* Encode the file header.
	 */
	strncpy  (hb.dbuf.name, fh->name, NAMSIZ-1);
	snprintf (hb.dbuf.mode,  8,  "%6o ",   fh->mode);
	snprintf (hb.dbuf.uid,   8,  "%6o ",   fh->uid);
	snprintf (hb.dbuf.gid,   8,  "%6o ",   fh->gid);
	snprintf (hb.dbuf.size,  12, "%11lo ", fh->size);
	snprintf (hb.dbuf.mtime, 12, "%11lo ", fh->mtime);

	switch (fh->linkflag) {
	case LF_SYMLINK:
	    hb.dbuf.linkflag = '2';
	    break;
	case LF_DIR:
	    hb.dbuf.linkflag = '5';
	    break;
	default:
	    hb.dbuf.linkflag = '0';
	    break;
	}
	strncpy (hb.dbuf.linkname, fh->linkname, NAMSIZ-1);

	/* Encode the checksum value for the file header and then
	 * write the field.  Calculate the checksum with the checksum
	 * field blanked out. Compute the actual checksum as the sum of 
	 * all bytes in the header block.  A sum of zero indicates the 
	 * end of the tar file.
	 */
	for (n=0;  n < 8;  n++)
	    hb.dbuf.chksum[n] = ' ';

	snprintf (chksum, 8, "%6o", cchksum (hb.dummy, TBLOCK));
	for (n=0, ip=chksum;  n < 8;  n++)
	    hb.dbuf.chksum[n] = *ip++;

	if (debug) {
	    printf ("File header:\n");
	    printf ("      name = %s\n", hb.dbuf.name);
	    printf ("      mode = %s\n", hb.dbuf.mode);
	    printf ("       uid = %s\n", hb.dbuf.uid);
	    printf ("       gid = %s\n", hb.dbuf.gid);
	    printf ("      size = %-12.12s\n", hb.dbuf.size);
	    printf ("     mtime = %-12.12s\n", hb.dbuf.mtime);
	    printf ("    chksum = %s\n", hb.dbuf.chksum);
	    printf ("  linkflag = %c\n", hb.dbuf.linkflag);
	    printf ("  linkname = %s\n", hb.dbuf.linkname);
	    fflush (stdout);
	}

	/* Write the header to the tarfile.
	 */
	return (putblock (out, hb.dummy));
}


/* CCHKSUM -- Compute the checksum of a byte array.
 */
static int
cchksum (
    register char	*p,
    register int	nbytes
)
{
	register int	sum;

	for (sum=0;  --nbytes >= 0;  )
	    sum += *p++;

	return (sum);
}


/* PRINTHEADER -- Print the file header in either short or long (verbose)
 * format, e.g.:
 *		drwxr-xr-x  9 tody         1024 Nov  3 17:53 .
 */
static void
printheader (
    FILE  *fp,				/* output file			*/
    register struct fheader *fh,	/* file header struct		*/
    int	verbose 			/* long format output		*/
)
{
	register struct	_modebits *mp;
	char	*tp;

	if (!verbose) {
	    fprintf (fp, "%s\n", fh->name);
	    return;
	}

	for (mp=modebits;  mp->code;  mp++)
	    fprintf (fp, "%c", mp->code & fh->mode ? mp->ch : '-');

	tp = ctime (&fh->mtime);
	fprintf (fp, "%3d %4d %2d %8ld %-12.12s %-4.4s %s",
	    fh->linkflag,
	    fh->uid,
	    fh->gid,
	    fh->size,
	    tp + 4, tp + 20,
	    fh->name);

	if (fh->linkflag && *fh->linkname)
	    fprintf (fp, " -> %s\n", fh->linkname);
	else
	    fprintf (fp, "\n");
}


/* COPYFILE -- Copy bytes from the input file to the output file.  Each file
 * consists of a integral number of TBLOCK size blocks on the output file.
 */
static void
copyfile (
    char  *fname,		/* file being read from		*/
    struct fheader *fh,		/* file header structure	*/
    int	   ftype,		/* file type, text or binary	*/
    int	   out 			/* output file			*/
)
{
	register char	*bp;
	register int	i;
	int	nbytes, nleft, blocks, fd, count, total, ch;
	char	buf[TBLOCK*2];

	bp = buf;
	total = nbytes = 0;
	blocks = (fh->size + TBLOCK - 1 ) / TBLOCK;

	if ((fd = os_open (fname, 0, ftype)) == ERR) {
	    fflush (stdout);
	    fprintf (stderr, "Warning: cannot open file `%s'\n", fname);
	    fflush (stderr);
	    goto pad_;
	}

	while (blocks > 0) {
	    if ((count = os_read (fd, bp, TBLOCK)) == ERR || count > TBLOCK) {
		fflush (stdout);
		fprintf (stderr, "Warning: file read error on `%s'\n", fname);
		fflush (stderr);
		if (nerrs++ > MAXERR) {
		    fprintf (stderr, "Too many errors\n");
		    exit (OSOK+1);
		}
	    } else {
		/* Buffer input to TBLOCK blocks.
		 */
		if (count == 0)		/* EOF */
		    break;
		else if ((nbytes += count) < TBLOCK)
		    bp += count;
		else {	
		    putblock (out, buf);
		    blocks--;

		    /* Copy overflow back to beginning... */
		    if (nbytes > TBLOCK) {
			nleft = nbytes - TBLOCK;
			os_amovb (&buf[TBLOCK], buf, nbytes - TBLOCK);
		    } else
			nleft = 0;

		    bp = (char *) ((long)buf + nleft);
		    total += nbytes;
		    nbytes = nleft;
		}
	    }
	}

	os_close (fd);

	/* Fill current block and subsequent full blocks until the number of
	 * bytes specified in the file header have been output.  All files
	 * occupy an integral number of 512 byte blocks on tape.  For text
	 * files, pad with spaces, otherwise pad with nulls.  Also, for text
	 * files, add newlines to avoid excessively long lines.
	 */
pad_:
	ch = (ftype == TEXT_FILE) ? ' ' : '\0';
	while (blocks > 0) {
	    for (i=nbytes;  i < TBLOCK;  i++)
		if (ftype == TEXT_FILE && i % 64 == 0)
		    buf[i] = '\n';
		else
		    buf[i] = ch;

	    if (ftype == TEXT_FILE)
		buf[TBLOCK-1] = '\n';

	    putblock (out, buf);
	    blocks--;
	    nbytes = 0;
	}
}


/* PUTBLOCK -- Write a block to tape (buffered).
 */
static int
putblock (int out, char *buf)
{
	int	nbytes = 0;

	if (buf) {
	    os_amovb (buf, nextblock, TBLOCK);
	    nextblock += TBLOCK;
	    if (++nblocks == NBLOCK)
		nbytes = SZ_TAPEBUFFER;
	} else if (nblocks > 0)
	    nbytes = SZ_TAPEBUFFER;

	if (nbytes > 0) {
	    if (tape_write (out, tapeblock, nbytes) < nbytes) {
		fflush (stdout);
		fprintf (stderr, "Warning: write error on tarfile\n");
		fflush (stderr);
	    }

	    nextblock = tapeblock;
	    nblocks = 0;
	}

	return (TBLOCK);
}


/* ENDTAR -- Write the end of the tar file, i.e., two zero blocks.
 */
static void
endtar (int out)
{
	register int  i;
	union    hblock hb;

	if (debug)
	    printf ("write end of tar file\n");

	for (i=0;  i < TBLOCK;  i++)
	    hb.dummy[i] = '\0';

	putblock (out, hb.dummy);	/* write 2 null blocks */
	putblock (out, hb.dummy);
	putblock (out, 0);		/* flush tape buffer */
}


/* U_FMODE -- Convert the IRAF file mode bits to the corresponding UNIX bits
 * for the tar file header.
 */
static int
u_fmode (int iraf_fmode, int ftype)
{
	register int	in = iraf_fmode;
	register int	m = 0;
	int	exec;

	exec = (ftype == FI_DIRECTORY || ftype == FI_EXECUTABLE);

	if (in & 001)	m |= 0400;	/* Owner READ  */
	if (in & 002)  	m |= 0200;	/*       WRITE */
	if (exec)	m |= 0100;	/*       EXECUTE */

	if (in & 004)	m |= 040;	/* Group READ  */
	if (in & 010)	m |= 020;	/*       WRITE */
	if (exec)	m |= 010;	/*       EXECUTE */

	if (in & 020)	m |= 004;	/* World READ  */
	if (in & 040)	m |= 002;	/*       WRITE */
	if (exec)	m |= 001;	/*       EXECUTE */

	return (m);
}


/* DNAME -- Normalize a directory pathname.   For unix, this means convert
 * an // sequences into a single /, and make sure the directory pathname ends
 * in a single /.
 */
static char *
dname (char *dir)
{
	register char	*ip, *op;
	static	char path[SZ_PATHNAME+1];

	for (ip=dir, op=path;  *ip;  *op++ = *ip++)
	    while (*ip == '/' && *(ip+1) == '/')
		ip++;

	if (op > path && *(op-1) != '/')
	    *op++ = '/';
	*op = EOS;

	return (path);
}
