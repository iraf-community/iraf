/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#define NOKNET
#define	import_spp
#define	import_knames
#include <iraf.h>

#include "../bootProto.h"


/*
 * RTAR -- Read a UNIX tar format tape containing files with legal IRAF
 * virtual filenames.  Map tape filenames to host system filenames using
 * IRAF filename mapping if the tape does not contain legal host system
 * filenames.
 *
 * Switches:
 *		a	advance to first file in filelist before doing
 *			  anything.  useful for restarting an aborted
 *			  operation.  first file is not otherwise used.
 *		b	generate only C style binary byte stream output
 *			  files (default is to write a text file when
 *			  the input stream is text).
 *		d	print debug messages
 *		e	exclude, rather than include, listed files
 *		f	read from named file rather than stdin
 *		l	do not try to resolve links by a file copy
 *		m	do not restore file modify times
 *		n	do not strip tailing blank lines from text files
 *		o	omit binary files (e.g. when foreign host has
 *			  incompatible binary file format)
 *		p	omit the given pathname prefix when creating files
 *		r	replace existing file at extraction
 *		t	print name of each file matched
 *		u	do not attempt to restore user id
 *		v	verbose; print full description of each file
 *		x	extract files (extract everything if no files
 *			  listed or if -e is set)
 *
 * Switches must be given in a group, in any order, e.g.:
 *
 *	rtar -xetvf tarfile sys/osb sys/os lib/config.h$
 *
 * would extract all files from tarfile with names not beginning with sys/os
 * or sys/osb or with names not equal to lib/config.h, printing a verbose
 * description of each file extracted.  If an exclude filename does not end
 * with a $ all files with the given string as a prefix are excluded.
 */

#define TBLOCK		512
#define NBLOCK		20
#define NAMSIZ		100
#define	MAXERR		20
#define	MAXTRYS		100
#define	MAXLINELEN	256
#define	SZ_TAPEBUFFER	(TBLOCK * NBLOCK)
#define	EOS		'\0'
#define	ERR		(-1)
#define	OK		0
#define	RWXR_XR_X	0755
#define	SZ_PADBUF	8196
#define ctrlcode(c)	((c) >= '\007' && (c) <= '\017')

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


static int advance;		/* Advance to named file		*/
static int stripblanks;		/* strip blank padding at end of file	*/
static int debug;		/* Print debugging messages		*/
static int binaryout;		/* make only binary byte stream files	*/
static int omitbinary;		/* omit binary files (do not write)	*/
static int extract;		/* Extract files from the tape		*/
static int replace;		/* Replace existing files		*/
static int exclude;		/* Excluded named files			*/
static int printfnames;		/* Print file names			*/
static int verbose;		/* Print everything			*/
static int links;		/* Defeat copy to resolve link		*/
static int setmtime;		/* Restore file modify times		*/
static int rsetuid;		/* Restore file user id			*/

static char *pathprefix = NULL;
static int len_pathprefix = 0;
static struct fheader *curfil;
static int eof;
static int nerrs;
static char *first_file;
static char tapeblock[SZ_TAPEBUFFER];
static char *nextblock;
static int nblocks;

extern int  ZZSTRT (void);
extern int  ZZSTOP (void);

extern  int tape_open (char *fname, int mode);
extern  int tape_close (int fd);
extern  int tape_read (int fd, char *buf, int nbytes);

static int   matchfile (char *fname, register char **files);
static int   getheader (int in, register struct fheader *fh);
static int   cchksum (register char *p, register int nbyte);
static void  printheader (FILE *out, register struct fheader *fh, int verbose);
static int   filetype (int in, struct fheader *fh);
static int   newfile (char *fname, int mode, int uid, int gid, int type);
static int   checkdir (register char *path, int mode, int uid, int gid);
static void  copyfile (int in, int out, struct fheader *fh, int ftype);
static void  strip_blanks (int in, int out, long nbytes);
static void  skipfile (int in, struct fheader *fh);
static char *getblock (int in);






/* MAIN -- "rtar [xtvlef] [names]".  The default operation is to extract all
 * files from the tar format standard input in quiet mode.
 */
int main (int argc, char *argv[])
{
	struct	fheader fh;
	char	**argp;
	char	*ip;
	int	in = 0, out;
	int	ftype;
	int	ch;

	ZZSTRT();		/* initialize the IRAF kernel	*/

	advance		= 0;
	debug		= 0;
	binaryout	= 0;
	omitbinary	= 0;
	extract		= 0;
	replace		= 0;
	exclude		= 0;
	printfnames	= 0;
	verbose		= 0;
	links		= 0;
	setmtime	= 1;
	rsetuid		= 1;
	stripblanks	= 1;	/* strip blanks at end of file by default */

	/* Get parameters.  Argp is left pointing at the list of files to be
	 * extracted (default all if no files named).
	 */
	argp = &argv[1];
	if (argc <= 1)
	    extract++;
	else {
	    while (*argp && **argp == '-') {
		ip = *argp++ + 1;
		while ((ch = *ip++) != EOS) {
		    switch (ch) {
		    case 'a':
			advance++;
			break;
		    case 'n':
			stripblanks = 0;
			break;
		    case 'x':
			extract++;
			break;
		    case 'b':
			binaryout++;
			break;
		    case 'd':
			debug++;
			break;
		    case 'e':
			exclude++;
			break;
		    case 'r':
			replace++;
			break;
		    case 't':
			printfnames++;
			break;
		    case 'v':
			printfnames++;
			verbose++;
			break;
		    case 'l':
			links++;
			break;
		    case 'm':
			setmtime = 0;
			break;
		    case 'u':
			rsetuid = 0;
			break;
		    case 'o':
			omitbinary++;
			break;
		    case 'p':
			if (*argp != NULL) {
			    pathprefix = *argp++;
			    len_pathprefix = strlen (pathprefix);
			}
			break;
		    case 'f':
			if (*argp == NULL) {
			    fprintf (stderr, "missing filename argument\n");
			    exit (OSOK+1);
			}
			in = tape_open (*argp, 0);
			if (in == ERR) {
			    fprintf (stderr, "cannot open `%s'\n", *argp);
			    ZZSTOP();
			    exit (OSOK+1);
			}
			argp++;
			break;
		    default:
			fprintf (stderr, "Warning: unknown switch `%c'\n", ch);
			fflush (stderr);
			break;
		    }
		}
	    }
	}

	/* If advancing to a file get the name of the file.  This file name
	 * occurs at the beginning of the file list but is not part of the list.
	 * Only full filenames are permitted here.
	 */
	if (advance)
	    first_file = *argp++;

	/* Step along through the tar format file.  Read file header and if
	 * file is in list and extraction is enabled, extract file.
	 */
	while (getheader (in, &fh) != EOF) {
	    curfil = &fh;
	    if (advance) {
		if (strcmp (fh.name, first_file) == 0) {
		    if (debug)
			fprintf (stderr, "match\n");
		    advance = 0;
		} else {
		    if (debug)
			printheader (stderr, &fh, verbose);
		    skipfile (in, &fh);
		    continue;
		}
	    }

	    if (matchfile (fh.name, argp) == exclude) {
		if (debug)
		    fprintf (stderr, "skip file `%s'\n", fh.name);
		skipfile (in, &fh);
		continue;
	    }

	    if (printfnames) {
		printheader (stdout, &fh, verbose);
		fflush (stdout);
	    }

	    if (fh.linkflag == LF_SYMLINK || fh.linkflag == LF_LINK) {
		/* No file follows header if file is a link.  Try to resolve
		 * the link by copying the original file, assuming it has been
		 * read from the tape.
		 */
		if (extract) {
		    if (fh.linkflag == LF_SYMLINK) {
			if (replace)
			    os_delete (fh.name);
			if (symlink (fh.linkname, fh.name) != 0) {
			    fprintf (stderr,
				"Cannot make symbolic link %s -> %s\n",
				fh.name, fh.linkname);
			}
		    } else if (fh.linkflag == LF_LINK && !links) {
			if (replace)
			    os_delete (fh.name);
			if (os_fcopy (fh.linkname, fh.name) == ERR) {
			    fprintf (stderr, "Copy `%s' to `%s' fails\n",
				fh.linkname, fh.name);
			} else {
			    os_setfmode (fh.name, fh.mode);
			    if (rsetuid)
				os_setowner (fh.name, fh.uid, fh.gid);
			    if (setmtime)
				os_setmtime (fh.name, fh.mtime);
			}
		    } else {
			fprintf (stderr,
			    "Warning: cannot make link `%s' to `%s'\n",
			    fh.name, fh.linkname);
		    }
		}
		continue;
	    }

	    if (extract) {
		ftype = filetype (in, &fh);
		if (fh.size > 0 && ftype == BINARY_FILE && omitbinary) {
		    if (printfnames)
			fprintf (stderr, "omit binary file `%s'\n", fh.name);
		    skipfile (in, &fh);
		    continue;
		}
		out = newfile (fh.name, fh.mode, fh.uid, fh.gid, ftype);
		if (out == ERR) {
		    fprintf (stderr, "cannot create file `%s'\n", fh.name);
		    skipfile (in, &fh);
		    continue;
		}
		if (!fh.isdir) {
		    copyfile (in, out, &fh, ftype);
		    os_close (out);
		}
		os_setfmode (fh.name, fh.mode);
		if (rsetuid)
		    os_setowner (fh.name, fh.uid, fh.gid);
		if (setmtime)
		    os_setmtime (fh.name, fh.mtime);
	    } else
		skipfile (in, &fh);
	}

	/* End of TAR file normally occurs when a zero tape block is read;
	 * this is not the same as the physical end of file, leading to
	 * problems when reading from sequential devices (e.g. pipes and
	 * magtape).  Advance to the physical end of file before exiting.
	 */
	if (!eof)
	    while (tape_read (in, tapeblock, SZ_TAPEBUFFER) > 0)
		;
	if (in)
	    tape_close (in);

	ZZSTOP();
	exit (OSOK);

	return (0);
}


/* MATCHFILE -- Search the filelist for the named file.  If the file list
 * is empty anything is a match.  If the list element ends with a $ an
 * exact match is required (excluding the $), otherwise we have a match if
 * the list element is a prefix of the filename.
 */
static int
matchfile (
    char *fname,		/* filename to be compared to list	*/
    register char **files	/* pointer to array of fname pointers	*/
)
{
	register char *fn, *ln;
	register int firstchar;

	if (*files == NULL)
	    return (1);

	firstchar = *fname;
	do {
	    if (**files++ == firstchar) {
		for (fn=fname, ln = *(files-1);  *ln && *ln == *fn++;  )
		    ln++;
		if (*ln == EOS)
		    return (1);
		else if (*ln == '$' && *(fn-1) == EOS)
		    return (1);
	    }
	} while (*files);

	return (0);
}


/* GETHEADER -- Read the next file block and attempt to interpret it as a
 * file header.  A checksum error on the file header is fatal and usually
 * indicates that the tape is not positioned to the beginning of a file.
 * If we have a legal header, decode the character valued fields into binary.
 */
static int
getheader (
    int	in,				/* input file			*/
    register struct fheader *fh		/* decoded file header (output)	*/
)
{
	register char *ip, *op;
	register int n;
	union	hblock *hb;
	int	tape_checksum, ntrys;

	for (ntrys=0;  ;  ntrys++) {
	    if ((hb = (union hblock *)getblock (in)) == NULL)
		return (EOF);

	    /* Decode the checksum value saved in the file header and then
	     * overwrite the field with blanks, as the field was blank when
	     * the checksum was originally computed.  Compute the actual
	     * checksum as the sum of all bytes in the header block.  If the
	     * sum is zero this indicates the end of the tar file, otherwise
	     * the checksums must match.
	     */
	    if (*hb->dbuf.chksum == '\0' && cchksum ((char *)hb, TBLOCK) == 0)
		return (EOF);
	    else
		sscanf (hb->dbuf.chksum, "%o", &tape_checksum);

	    for (ip=hb->dbuf.chksum, n=8;  --n >= 0;  )
		*ip++ = ' ';
	    if (cchksum ((char *)hb, TBLOCK) != tape_checksum) {
		/* If a checksum error occurs try to advance to the next
		 * header block.
		 */
		if (ntrys == 0) {
		    fprintf (stderr,
			"rtar: file header checksum error %o != %o\n",
	    		cchksum ((char *)hb, TBLOCK), tape_checksum);
		} else if (ntrys >= MAXTRYS) {
		    fprintf (stderr, "cannot recover from checksum error\n");
		    exit (OSOK+1);
		}
	    } else
		break;
	}

	if (ntrys > 1)
	    fprintf (stderr, "found next file following checksum error\n");

	/* Decode the ascii header fields into the output file header
	 * structure.
	 */
	for (ip=hb->dbuf.name, op=fh->name;  (*op++ = *ip++);  )
	    ;
	fh->isdir = (*(op-2) == '/');

	sscanf (hb->dbuf.mode,     "%o",  &fh->mode);
	sscanf (hb->dbuf.uid,      "%o",  &fh->uid);
	sscanf (hb->dbuf.gid,      "%o",  &fh->gid);
	sscanf (hb->dbuf.size,     "%lo", &fh->size);
	sscanf (hb->dbuf.mtime,    "%lo", &fh->mtime);

	n = hb->dbuf.linkflag;
	if (n >= '0' && n <= '9')
	    fh->linkflag = n - '0';
	else
	    fh->linkflag = 0;

	if (fh->linkflag)
	    strcpy (fh->linkname, hb->dbuf.linkname);
	
	return (TBLOCK);
}


/* CCHKSUM -- Compute the checksum of a byte array.
 */
static int
cchksum (
    register char *p,
    register int   nbytes
)
{
	register int	sum;

	for (sum=0;  --nbytes >= 0;  )
	    sum += *p++;

	return (sum);
}


struct _modebits {
	int	code;
	char	ch;
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


/* PRINTHEADER -- Print the file header in either short or long (verbose)
 * format, e.g.:
 *		drwxr-xr-x  9 tody         1024 Nov  3 17:53 .
 */
static void
printheader (
    FILE  *out,				/* output file			*/
    register struct fheader *fh,	/* file header struct		*/
    int	  verbose			/* long format output		*/
)
{
	register struct	_modebits *mp;
	char	*tp;

	if (!verbose) {
	    fprintf (out, "%s\n", fh->name);
	    return;
	}

	for (mp=modebits;  mp->code;  mp++)
	    fprintf (out, "%c", mp->code & fh->mode ? mp->ch : '-');

	tp = ctime (&fh->mtime);
	fprintf (out, "%3d %4d %2d %8ld %-12.12s %-4.4s %s",
	    fh->linkflag,
	    fh->uid,
	    fh->gid,
	    fh->size,
	    tp + 4, tp + 20,
	    fh->name);

	if (fh->linkflag && *fh->linkname)
	    fprintf (out, " -> %s\n", fh->linkname);
	else
	    fprintf (out, "\n");
}


/* FILETYPE -- Determine the file type (text, binary, or directory) of the
 * next file on the input stream.  Directory files are easy; the tar format
 * identifies directories unambiguously.  Discriminating between text and
 * binary files is not possible in general because UNIX does not make such
 * a distinction, but in practice we can apply a heuristic which will work
 * in nearly all cases.  This can be overriden, producing only binary byte
 * stream files as output, by a command line switch.
 */
static int
filetype (
    int	   in,			/* input file			*/
    struct fheader *fh		/* decoded file header		*/
)
{
	register char	*cp;
	register int	n, ch;
	int	newline_seen, nchars;

	/* Easy cases first.
	 */
	if (fh->isdir)
	    return (DIRECTORY_FILE);
	else if (fh->size == 0 || binaryout)
	    return (BINARY_FILE);

	/* Get a pointer to the first block of the input file and set the
	 * input pointers back so that the block is returned by the next
	 * call to getblock.
	 */
	if ((cp = getblock (in)) == NULL)
	    return (BINARY_FILE);
	nextblock -= TBLOCK;
	nblocks++;

	/* Examine the data to see if it is text.  The simple heuristic
	 * used requires that all characters be either printable ascii
	 * or common control codes.
	 */
	n = nchars = (fh->size < TBLOCK) ? fh->size : TBLOCK;
	for (newline_seen=0;  --n >= 0;  ) {
	    ch = *cp++;
	    if (ch == '\n')
		newline_seen++;
	    else if (!isprint(ch) && !isspace(ch) && !ctrlcode(ch))
		break;
	}

	if (n >= 0 || (nchars > MAXLINELEN && !newline_seen))
	    return (BINARY_FILE);
	else
	    return (TEXT_FILE);
}


/* NEWFILE -- Try to open a new file for writing, creating the new file
 * with the mode bits given.  Create all directories leading to the file if
 * necessary (and possible).
 */
static int
newfile (
    char *fname,		/* pathname of file		*/
    int	  mode,			/* file mode bits		*/
    int	  uid, int gid,		/* file owner, group codes	*/
    int	  type 			/* text, binary, directory	*/
)
{
	int	fd;
	char	*cp;

	if (len_pathprefix && strncmp(fname,pathprefix,len_pathprefix) == 0)
	    fname += len_pathprefix;

	if (debug)
	    fprintf (stderr, "newfile `%s':\n", fname);

	if (checkdir (fname, mode, uid, gid) == ERR)
	    return (ERR);

	if (type == DIRECTORY_FILE) {
	    cp = strrchr (fname, '/');
	    if (cp && *(cp+1) == EOS)
		*cp = EOS;
	    fd = os_createdir (fname, mode);

	    /* Ignore any error creating directory, as this may just mean
	     * that the directory already exists.  If the directory does
	     * not exist and cannot be created, there will be plenty of
	     * other errors when we try to write files into it.
	     */
	    fd = OK;

	} else {
	    if (replace)
		os_delete (fname);
	    fd = os_createfile (fname, mode, type);
	}

	return (fd);
}


/* CHECKDIR -- Verify that all the directories in the pathname of a file
 * exist.  If they do not exist, try to create them.
 */
static int
checkdir (
    register char *path,
    int	mode,
    int	uid, int gid
)
{
	register char	*cp;

	/* Quick check to see if the directory exists.
	 */
	if ((cp = strrchr (path, '/')) == NULL)
	    return (OK);

	*cp = EOS;
	if (os_access (path, 0, DIRECTORY_FILE) == YES) {
	    *cp = '/';
	    return (OK);
	}
	*cp = '/';

	/* The directory cannot be accessed.  Try to make all directories
	 * in the pathname.  If the file is itself a directory leave its
	 * creation until later.
	 */
	for (cp=path;  *cp;  cp++) {
	    if (*cp != '/')
		continue;
	    if (*(cp+1) == EOS)
		return (OK);

	    *cp = EOS;
	    if (os_access (path, 0, DIRECTORY_FILE) == NO) {
		if (os_createdir (path, RWXR_XR_X) == ERR) {
		    fprintf (stderr, "cannot create directory `%s'\n", path);
		    *cp = '/';
		    return (ERR);
		} else
		    os_setowner (path, uid, gid);
	    }
	    *cp = '/';
	}

	return (OK);
}


/* COPYFILE -- Copy bytes from the input (tar) file to the output file.
 * Each file consists of a integral number of TBLOCK size blocks on the
 * input file.
 */
static void
copyfile (
    int	   in,			/* input file			*/
    int	   out,			/* output file			*/
    struct fheader *fh,		/* file header structure	*/
    int	   ftype		/* text or binary file		*/
)
{
	long	nbytes = fh->size;
	int	nblocks = 0, maxpad;
	char	*bp;


	/* Link files are zero length on the tape. */
	if (fh->linkflag)
	    return;

	if (ftype == BINARY_FILE || !stripblanks)
	    maxpad = 0;
	else
	    maxpad = SZ_PADBUF;

	/* Copy all but the last MAXPAD characters if the file is a text file
	 * and stripping is enabled.
	 */
	while (nbytes > maxpad && (bp = getblock (in)) != NULL)
	    if (os_write (out, bp, nbytes<TBLOCK ? (int)nbytes:TBLOCK) == ERR) {
		fprintf (stderr, "Warning: file write error on `%s'\n",
		    curfil->name);
		if (nerrs++ > MAXERR) {
		    fprintf (stderr, "Too many errors\n");
		    exit (OSOK+1);
		}
	    } else {
		nbytes -= TBLOCK;
		nblocks++;
	    }

	/* Strip whitespace at end of file added by WTAR when the archive was
	 * created.
	 */
	if (nbytes > 0)
	    strip_blanks (in, out, nbytes);

	if (debug)
	    fprintf (stderr, "%d blocks written\n", nblocks);
}


/* STRIP_BLANKS -- Read the remaining file data into the pad buffer.
 * Write out the remaining data, minus any extra blanks or empty blank lines
 * at the end of the file.  Some versions of WTAR (e.g., VMS) do not know
 * the actual size of a text file and have to pad with blanks at the end to
 * make the file the size noted in the file header.
 */
static void
strip_blanks (int in, int out, long nbytes)
{
	register char	*ip, *op;
	char	padbuf[SZ_PADBUF+10];
	char	*lastnl;
	int	n;

	/* Fill buffer.
	 */
	op = padbuf;
	while (nbytes > 0 && (ip = getblock (in)) != NULL) {
	    n = nbytes < TBLOCK ? (int)nbytes : TBLOCK;
	    os_amovb (ip, op, n + sizeof(XCHAR)-1);
	    nbytes -= n;
	    op += n;
	}

	/* Backspace from the end of the buffer until the last nonblank line
	 * is found.
	 */
	lastnl = op - 1;
	for (ip=lastnl;  ip > padbuf;  --ip)
	    if (*ip == '\n')
		lastnl = ip;
	    else if (*ip != ' ')
		break;

	/* Write out everything up to and including the newline at the end of
	 * the last line containing anything but blanks.
	 */
	os_write (out, padbuf, lastnl - padbuf + 1);
}


/* SKIPFILE -- Skip the indicated number of bytes on the input (tar) file.
 */
static void
skipfile (
    int	   in,			/* input file			*/
    struct fheader *fh 		/* file header			*/
)
{
	register long nbytes = fh->size;

	/* Link files are zero length on the tape. */
	if (fh->linkflag)
	    return;

	while (nbytes > 0 && getblock (in) != NULL)
	    nbytes -= TBLOCK;
}


/* GETBLOCK -- Return a pointer to the next file block of size TBLOCK bytes
 * in the input file.
 */
static char *
getblock (int in)
{
	char	*bp;
	int	nbytes;

	for (;;) {
	    if (eof)
		return (NULL);
	    else if (--nblocks >= 0) {
		bp = nextblock;
		nextblock += TBLOCK;
		return (bp);
	    }

	    if ((nbytes = tape_read (in, tapeblock, SZ_TAPEBUFFER)) < TBLOCK)
		eof++;
	    else {
		nblocks = (nbytes + TBLOCK-1) / TBLOCK;
		nextblock = tapeblock;
	    }
	}
}
