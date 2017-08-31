/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

#define	import_kernel
#define	import_knames
#define	import_spp
#include <iraf.h>


/*
 * KUTIL -- Miscellaneous utilities required by the network interface.
 * Most of these are either portable (no i/o) or are built upon the kernel
 * i/o routines.
 */

/* KU_FOPEN -- Open a text file.
 */
ku_fopen (fname, mode)
char	*fname;
char	*mode;
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	XINT	fmode, chan;

	strcpy ((char *)osfn, fname);

	if (mode[0] == 'r')
	    fmode = READ_ONLY;
	else
	    return (ERR);

	ZOPNTX (osfn, &fmode, &chan);

	return (chan);
}


/* KU_FCLOSE -- Close a text file.
 */
int 
ku_fclose (int fd)
{
	XINT	chan=fd, status;

	ZCLSTX (&chan, &status);
	return (status);
}


/* KU_FGETS -- Get a newline delimited line from a text file.  The semantics of
 * this procedure are like the unix FGETS.
 */
char *
ku_fgets (char *obuf, int maxch, int fd)
{
	register XCHAR	*ip;
	register char	*op;
	register int	n;
	XCHAR	lbuf[SZ_LINE+1];
	XINT	maxchars, status, chan;

	maxchars = (maxch > SZ_LINE) ? SZ_LINE : maxch;
	chan = fd;

	ZGETTX (&chan, lbuf, &maxchars, &status);
	if (status <= 0)
	    return (NULL);
	
	for (ip=lbuf, op=obuf, n=status;  --n >= 0;  )
	    *op++ = *ip++;
	*op++ = EOS;

	return (obuf);
}


/* KU_GPASSWD -- Read a line from the terminal in raw mode (no echo), e.g.,
 * when reading a password.
 */
int 
ku_gpasswd (
    char *prompt,		/* user prompt string		*/
    char *passwd,		/* receives password		*/
    int maxch
)
{
	XCHAR	text[SZ_LINE+1], ch;
	XINT	mode=READ_WRITE, chan, status, nchars;
	register char	*ip;
	register XCHAR	*op;
	register int	n;

	/* Open terminal. */
	strcpy ((char *)text, TTYNAME);
	ZOPNTY (text, &mode, &chan);
	if (chan < 0) {
	    passwd[0] = EOS;
	    return (ERR);
	}

	/* Write prompt string. */
	for (ip=prompt, op=text, nchars=0;  (*op++ = *ip++) != EOS;  )
	    nchars++;
	ZPUTTY (&chan, text, &nchars, &status);
	ZFLSTY (&chan, &status);

	/* Read line in raw mode. */
	nchars = 1;
	for (n=0;  n < maxch;  n++) {
	    ZGETTY (&chan, &text, &nchars, &status);
	    ch = text[0];
	    if (status <= 0 || ch == '\n' || ch == '\r')
		break;
	    passwd[n] = ch;
	}
	passwd[n] = EOS;

	/* Echo the newline. */
	ch = '\n';
	ZPUTTY (&chan, &ch, &nchars, &status);

	/* Disable raw mode. */
	nchars = LEN_RAWCMD;
	for (ip=RAWOFF, op=text, n=LEN_RAWCMD;  --n > 0 && (*op++ = *ip++);  )
	    ;
	ZPUTTY (&chan, text, &nchars, &status);
	ZCLSTY (&chan, &status);

	return (n);
}


/* KU_MKFNAME -- Make an OSFN, given a logical directory name (either "iraf"
 * or "home"), a subdirectory name, and a filename.
 */
int 
ku_mkfname (
    char *ldir,			/* logical directory name	*/
    char *subdir,		/* subdirectory			*/
    char *fname,			/* filename			*/
    char *osfn,			/* receives pathname		*/
    int maxch
)
{
	PKCHAR	pkname[SZ_PATHNAME+1];
	PKCHAR	temp[SZ_FNAME+1];
	XINT	maxchars=SZ_PATHNAME, nchars;

	if (ku_mapdir (ldir, (char *)pkname, SZ_PATHNAME) == ERR)
	    return (ERR);

	strcpy ((char *)temp, subdir);
	ku_strupk (pkname, pkname, &maxchars);
	ku_strupk (temp, temp, &maxchars);
	ZFSUBD (pkname, &maxchars, temp, &nchars);
	ku_strpak (pkname, pkname, &maxchars);

	strcat ((char *)pkname, fname);
	strncpy (osfn, (char *)pkname, maxch);
	osfn[maxch-1] = EOS;

	return (OK);
}


/* KU_ITOC -- Encode a simple positive integer in a decimal radix, returning
 * a pointer to the encoded numeric string.
 */
char *
ku_itoc (int num)
{
	register int	dig, n;
	register char	*op;
	static	char buf[15];

	op = &buf[15];
	*--op = '\0';

	for (n=num;  dig = n % 10;  n /= 10)
	    *--op = dig + '0';

	return (op);
}


/* KU_BCOPY -- Copy a byte array.
 */
int 
ku_bcopy (
    char *a,			/* input byte array			*/
    char *b,			/* output byte array			*/
    int nbytes			/* number of bytes to move		*/
)
{
	register char	*ip, *op;
	register int	n = nbytes;

	/* If the two arrays are the same return immediately.  If the move is
	 * to the left then copy left to right, else copy right to left.
	 */
	if (a == b) {
	    return;
	} else if (b < a) {
	    for (ip=a, op=b;  --n >= 0;  )
		*op++ = *ip++;
	} else {
	    for (ip = &a[n], op = &b[n];  --n >=  0;  )
		*--op = *--ip;
	}
}


/* KU_SLEEP -- Suspend process execution.
 */
int 
ku_sleep (int nseconds)
{
	int	mseconds = nseconds*1000;

	ZWMSEC (&mseconds);
}


/* KU_ERROR -- [MACHDEP] Print an error message somewhere where the user can
 * see it (but do not abort or interrupt execution).
 */
int 
ku_error (char *message)
{
	write (2, message, strlen(message));
	write (2, "\n", 1);
}


/* KU_MAPDIR -- Return the OSFN of the named logical directory, which can
 * be either "iraf" or "home".  The IRAF root directory is either given in
 * the host system environment and returned by ZGTENV, or is defined as
 * IRAF in <iraf.h>.  The user's login directory "home" is the host
 * system login directory, not the IRAF login directory.  On a UNIX system
 * the pathname of this directory is given in the UNIX file /etc/passwd.
 * On other systems, e.g., VMS, the ZGTENV mechanism can be used to define
 * the user's home directory.
 */
int 
ku_mapdir (
    char *ldir,			/* logical directory name	*/
    char *osfn,			/* receives filename		*/
    int maxch
)
{
	PKCHAR	pkname[SZ_FNAME+1];
	PKCHAR	valstr[SZ_PATHNAME+1];
	XINT	maxchars=SZ_PATHNAME, status;

	/* Look in the host environment first.
	 */
	strcpy ((char *)pkname, ldir);
	ZGTENV (pkname, valstr, &maxchars, &status);

	if (status > 0) {
	    strncpy (osfn, (char *)valstr, maxch);
	    osfn[maxch-1] = EOS;
	    return (OK);
	} else if (strncmp (ldir, "iraf", 4) == 0) {
	    strncpy (osfn, IRAF, maxch);
	    osfn[maxch-1] = EOS;
	    return (OK);
	} else if (strncmp (ldir, "home", 4) != 0) {
	    osfn[0] = EOS;
	    return (ERR);
	}

	/* If we get here the ldir is "home" and no definition was found in the
	 * host environment.  Determine host login directory by some system
	 * dependent means.  [MACHDEP].
	 */
	strcpy ((char *)pkname, "LOGNAME");
	ZGTENV (pkname, valstr, &maxchars, &status);
	if (status <= 0) {
	    osfn[0] = EOS;
	    return (ERR);
	} else {
	    strcpy (osfn, ":udd:");
	    strcat (osfn, (char *)valstr);
	    strcat (osfn, ":");
	    return (OK);
	}
}


/* STRPAK -- Pack an SPP character string into a C string, i.e., a sequence
 * of characters stored one per byte, delimited by EOS='\0'.  The operation
 * may be performed in place.  This version assumes that the host character
 * set is ASCII and hence no lookup table reference to map character sets is
 * needed.  If this is not the case, code must be added to convert to the host
 * character set.
 *
 * N.B.: If sizeof(XCHAR)=1, XEOS=EOS, and the host character set is ASCII,
 * and the operation is being performed in place, then this procedure should
 * do nothing.
 */
ku_strpak (instr, outstr, maxch)
XCHAR	*instr;
PKCHAR	*outstr;
XINT	*maxch;
{
	register XCHAR	*ip = instr;
	register char	*op = (char *)outstr;
	register int	  n = *maxch;

	while ((*op++ = *ip++) != XEOS && --n >= 0)
	    ;
	*--op = EOS;
}

/* STRUPK -- Unpack a kernel (C style) string into an SPP string.  The unpacking * operation can be performed in place.  A kernel string consists of a sequence
 * of host characters stored one character per byte, delimited by EOS='\0'.
 * We assume here that the host character set is ASCII.  If this is not the
 * case code must be added to convert from the host character set to ASCII in
 * the unpacked string.
 *
 * N.B.: If sizeof(XCHAR)=1, XEOS=EOS, and the host character set is ASCII,
 * and the operation is being performed in place, then this procedure should
 * do nothing.
 */
ku_strupk (instr, outstr, maxch)
PKCHAR	*instr;
XCHAR	*outstr;
XINT	*maxch;
{
	register char	*ip = (char *)instr;
	register XCHAR	*op = outstr;
	register int	  n;

	/* Is is necessary to determine the length of the string in order to
	 * be able to unpack the string in place, i.e., from right to left.
	 */
	n = strlen (ip);
	n = (n < *maxch) ? n : *maxch;
	op[n] = XEOS;

	while (--n >= 0)
	    op[n] = ip[n];
}
