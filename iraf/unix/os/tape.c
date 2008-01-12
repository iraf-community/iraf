#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

#ifdef sun
#include <sundev/tmreg.h>
#include <sundev/xtreg.h>
#include <sundev/arreg.h>
#endif

/*
 * TAPE.C -- Magtape test program (for most UNIX systems).
 *
 * Commands:
 *
 *	open [device [r|w]]		open device with given mode
 *	close				close device
 *	rew[ind]			rewind
 *
 *	fsf [n]				forward space N filemarks
 *	bsf [n]				backspace N filemarks
 *	fsr [n]				forward space N records
 *	bsr [n]				backspace N records
 *	read [n [bufsize]]		read N records
 *	write [n [bufsize]]		write N records
 *	seek [offset[bkm]]              seek (b=block#, k=x1024, m=x1024*1024)
 *	weof				write end of file
 *
 *	s[tatus]			print tape status (device dependent)
 *	verbose				toggle verbose status mode
 *
 *	log [file]			toggle logging to file (def:tape.out)
 *	run file			execute commands in "file"
 *	? or help			print commands
 *	q[uit]				exit command loop
 *
 * The drive to be opened is by default the most recently referenced drive,
 * or the value of the variable TAPE, if defined in the user environment.
 *
 * Records are read into a default buffer size of 65535 bytes on reads if
 * no buffer size is specified.  The number of bytes read will be printed,
 * along with the first 76 or so printable ascii characters at the head of
 * the record, omitting all control codes.  This is usually enough to identify
 * the type of record.
 *
 * On writes, the data written is an ascii representation of the file and
 * record number, i.e., "file M record N".  If the buffer size is not given
 * the last buffer size specified is used, initially 1024.
 */

/* #define SUNOS41 */

#define SZ_COMMAND	512
#define SZ_FNAME	256
#define SZ_IOBUF	262144
#define NREAD		64512
#define NWRITE		1024
#define EOS		'\0'

static	char mtdev[SZ_FNAME];
static	char o_mtdev[SZ_FNAME];
static	char iobuf[SZ_IOBUF];
static	char cmdbuf[SZ_COMMAND];
static	char tokbuf[SZ_COMMAND];
static	char logfile[SZ_FNAME];
static	int rbufsz, wbufsz;
static	int t_fileno;
static	int t_blkno;
static	int t_acmode;
static	int verbose;
static	int status;
static	FILE *stack[20];
static	FILE *logfp;
static	const char *tp;
static	int tape;
static	int sp;

static char *nextcmd ( FILE * );
static char *prompt ( void );
static char *gettok ( void );
static int output ( const char * );
static int phelp ( void );
static int pstatus ( void );
static int mtop ( int , int );

/* TAPE program main.
 */
int main ( int argc, char *argv[] )
{
	char lbuf[256];
	const char *token;
	int nrec, nbytes;
	FILE *in;
	FILE *fp;

	errno = 0;
	t_blkno = 0;
	t_fileno = 0;
	rbufsz = NREAD;
	wbufsz = NWRITE;
	strcpy (logfile, "tape.out");

	if (argc > 1) {
	    strncpy (o_mtdev, argv[1], SZ_FNAME);
	    o_mtdev[SZ_FNAME-1]='\0';
	}

	for ( in=stdin, sp=0 ; ; ) {
	    /* Prompt if interactive. */
	    if (in == stdin) {
		fputs (prompt(), stdout);
		fflush (stdout);
	    }

	    /* Get command from current input stream. */
	    if (nextcmd (in) == NULL) {
quit:
		if (in != stdin)
		    fclose (in);
		if ( sp <= 0 ) break;
		else {
		    sp--;
		    in = stack[sp];
		    continue;
		}
	    } else if (*tp == '!') {
		system (tp+1);
		continue;
	    } else if ((token = gettok()) == NULL)
		continue;

	    if (in == stdin) {
		/* Log command if entered interactively. */
		if (logfp)
		    fputs (cmdbuf, logfp);
	    } else {
		/* Echo command if noninteractive. */
		output (cmdbuf);
		fflush (stdout);
	    }

	    /* Check for program control commands. */
	    if (!strncmp (token, "quit", 1)) {
		goto quit;
	    } else if (!strcmp (token, "?") || !strcmp (token, "help")) {
		phelp();
		continue;
	    } else if (!strncmp (token, "status", 2)) {
		pstatus();
		continue;
	    } else if (!strncmp (token, "verbose", 3)) {
		verbose = !verbose;
		continue;

	    } else if (!strncmp (token, "log", 3)) {
		/* Ignore log commands not entered interactively. */
		if (in != stdin)
		    continue;

		/* Toggle logging. */
		if (logfp) {
		    printf ("logging disabled\n");
		    fclose (logfp);
		    logfp = NULL;
		} else {
		    if (token = gettok()) {
			strncpy (logfile, token, SZ_FNAME);
			logfile[SZ_FNAME-1]='\0';
		    }
		    if ((logfp = fopen (logfile, "a")) == NULL)
			printf ("cannot open logfile %s\n", logfile);
		    else {
			printf ("logging output to %s\n", logfile);
			fprintf (logfp, "# --- BEGIN ---\n");
		    }
		}
		continue;

	    } else if (!strcmp (token, "run")) {
		if (!(token = gettok()) || (fp=fopen(token,"r")) == NULL)
		    printf ("cannot run %s\n", token ? token : "?");
		else {
		    if ( 20 <= sp ) {
			fprintf(stderr,"[ERROR] sp exceeds stack size\n");
			sp--;
		    }
		    stack[sp++] = in;
		    in = fp;
		}
		continue;
	    }

	    /*
	     * TAPE CONTROL commands.
	     */

	    if (!strncmp (token, "open", 1)) {
		/* Get device name. */
		if (!(token=gettok()) || !strcmp (token, ".")) {
		    if (!o_mtdev[0] && (token = getenv ("TAPE"))) {
			strncpy (mtdev, token, SZ_FNAME);
			mtdev[SZ_FNAME-1]='\0';
		    }
		    else {
			strcpy (mtdev, o_mtdev);
		    }
		} else if (token[0]) {
		    strncpy (mtdev, token, SZ_FNAME);
		    mtdev[SZ_FNAME-1]='\0';
		}

		if (!mtdev[0]) {
		    output ("no tape device specified\n");
		    continue;
		}

		/* Open device. */
		if ((tape = open (mtdev, t_acmode =
		    ((token=gettok()) && *token == 'w') ? 2 : 0)) == -1) {
		    snprintf (lbuf, 256, "cannot open device %s\n", mtdev);
		    output (lbuf);
		    mtdev[0] = EOS;
		    continue;
		}
		snprintf (lbuf, 256,
		    "device %s open on descriptor %d\n", mtdev, tape);
		output (lbuf);
		strcpy (o_mtdev, mtdev);
	    } else if (!strncmp (token, "close", 1)) {
		close (tape);
		if (t_acmode) {
		    t_fileno++;
		    t_blkno = 0;
		}
		mtdev[0] = EOS;
		errno = 0;
	    } else if (!strncmp (token, "rew", 3)) {
		mtop (MTREW, 1);
		t_fileno = 0;
		t_blkno = 0;
		errno = 0;
	    } else if (!strcmp (token, "weof")) {
		mtop (MTWEOF, 1);
		t_fileno++;
		t_blkno = 0;

	    } else if (!strcmp (token, "fsf")) {
		mtop (MTFSF, (token = gettok()) ? atoi(token) : 1);
	    } else if (!strcmp (token, "fsr")) {
		mtop (MTFSR, (token = gettok()) ? atoi(token) : 1);
	    } else if (!strcmp (token, "bsf")) {
		mtop (MTBSF, (token = gettok()) ? atoi(token) : 1);
	    } else if (!strcmp (token, "bsr")) {
		mtop (MTBSR, (token = gettok()) ? atoi(token) : 1);

	    } else if (!strncmp (token, "read", 1)) {
		int i, j;

		nrec = (token = gettok()) ? atoi(token) : 1;
		nbytes = rbufsz = (token = gettok()) ? atoi(token) : rbufsz;
		if (nbytes > SZ_IOBUF)
		    nbytes = SZ_IOBUF;

		for (j=0;  j < nrec;  j++) {
		    for (i=0;  i < nbytes;  i++)
			iobuf[i] = 0;
		    status = read (tape, iobuf, nbytes);
		    pstatus();

		    if (status < 0) {
			output ("  ERR\n");
		    } else if (status == 0) {
			output ("  EOF\n");
		    } else if (status > 0) {
			char obuf[512];
			const char *ip;
			char *op, ch;

			op = obuf;  *op++ = ' ';  *op++ = ' ';
			for (i=0, ip=iobuf;  i < status && op-obuf < 78;  i++)
			    if ((ch = ip[i]) > 040 && ch < 0177)
				*op++ = ip[i];
			*op++ = '\n';
			*op++ = EOS;
			output (obuf);
		    }
		}

		continue;

	    } else if (!strncmp (token, "write", 1)) {
		int i;

		nrec = (token = gettok()) ? atoi(token) : 1;
		nbytes = wbufsz = (token = gettok()) ? atoi(token) : wbufsz;
		if (nbytes > SZ_IOBUF)
		    nbytes = SZ_IOBUF;

		for (i=0;  i < nbytes;  i++)
		    iobuf[i] = 0;

		for (i=0;  i < nrec;  i++) {
		    snprintf (iobuf, SZ_IOBUF, "file %d, record %d\n",
			t_fileno, t_blkno);
		    status = write (tape, iobuf, nbytes);
		    t_blkno++;
		    pstatus();
		}

		continue;

	    } else if (!strncmp (token, "seek", 2)) {
		const char *ip;
		int fwd, bak, i;

		if (token = gettok()) {
		    ip = token;
		    fwd = bak = 0;
		    if (*ip == '-') {
			bak++;
			ip++;
		    } else if (*ip == '+') {
			fwd++;
			ip++;
		    }

		    for (i=0;  isdigit(*ip);  ip++)
			i = i * 10 + (*ip - '0');

		    switch (*ip) {
		    case 'b':
			i *= rbufsz;
			break;
		    case 'k':
			i *= 1024;
			break;
		    case 'm':
			i *= (1024*1024);
			break;
		    }

		    if (fwd)
			status = lseek (tape, (off_t)i, 1);
		    else if (bak)
			status = lseek (tape, -(off_t)i, 1);
		    else
			status = lseek (tape, (off_t)i, 0);
		    pstatus();

		} else {
		    status = lseek (tape, 0, 1);
		    pstatus();
		}

	    } else
		output ("unrecognized command\n");

	    if (verbose)
		pstatus();
	    fflush (stdout);
	}

	return 0;
}


/* MTOP -- Execute a magtape operation.
 */
/* op    : operation code */
/* count : count argument */
static int mtop ( int op, int count )
{
	struct mtop mt;

	mt.mt_op = op;
	mt.mt_count = count;
	status = ioctl (tape, MTIOCTOP, &mt);
	if (!verbose && status < 0)
	    pstatus();

	return 0;
}


/* NEXTCMD -- Get next command.
 */
static char *nextcmd ( FILE *in )
{
	fflush (stdout);
	if (fgets (cmdbuf, SZ_COMMAND, in) == NULL)
	    return (NULL);
	else
	    return (tp = cmdbuf);
}


/* GETTOK -- Get next token from the input stream.
 */
static char *gettok( void )
{
	char *op, *maxop;

	while (*tp && isspace(*tp))
	    tp++;
	if (*tp == EOS || *tp == '#')
	    return (NULL);

	maxop = tokbuf + SZ_COMMAND -1;
	for ( op=tokbuf ; op < maxop && (*tp) && !isspace(*tp) ; tp++, op++ )
	    *op = *tp;

	*op = EOS;
	return (tokbuf);
}


/* PROMPT -- Return a pointer to the prompt string.
 */
static char *prompt( void )
{
	static char prompt[32];
	static char defp[] = "% ";
	const char *ip, *dev;

	for (ip=dev=mtdev;  *ip;  ip++)
	    if (*ip == '/')
		dev = ip + 1;

	if (*dev) {
	    snprintf (prompt, 32, "(%s) ", dev);
	    return (prompt);
	} else
	    return (defp);
}


/* PSTATUS -- Print status of tape and last operation.
 */
static int pstatus( void )
{
	char	obuf[512];
	/* int	stat; */

#ifdef sun
	static	struct mt_tape_info info[] = MT_TAPE_INFO;
	struct	mt_tape_info *tp;
	struct	mtget mt;
	const char *tn;

	if (verbose) {
	    if (ioctl (tape, MTIOCGET, &mt) != 0)
		sprintf (obuf, "MTIOCGET ioctl fails\n");
	    else {
		for (tn="unknown", tp=info;  tp->t_type;  tp++)
		    if (tp->t_type == mt.mt_type) {
			tn = tp->t_name;
			break;
		    }

		snprintf (obuf, 512,
	    "status %d (%d) file=%d block=%d resid=%d [ds=0x%x er=0x%x] %s\n",
		    status, errno, mt.mt_fileno, mt.mt_blkno,
		    mt.mt_resid, mt.mt_dsreg, mt.mt_erreg, tn);
	    }
	} else
	    snprintf (obuf, 512, "status %d (%d)\n", status, errno);
#else
	snprintf (obuf, 512, "status %d (%d)\n", status, errno);
#endif

	output (obuf);
	fflush (stdout);

	return 0;
}


/* OUTPUT -- Write text to the standard output, and to the logfile output
 * if enabled.
 */
static int output ( const char *text )
{
	fputs (text, stdout);
	if (logfp) {
	    fputs ("# ", logfp);
	    fputs (text, logfp);
	}

	return 0;
}


static const char *helptxt[] = {
	"Usage: tape [device].   The following commands are provided:\n",
	"\n",
	"    open [device [r|w]]    rewind                   fsf [n]\n",
	"    close                  read [nrec [bufsz]]      fsr [n]\n",
	"    log [file]             write [nrec [bufsz]]     bsf [n]\n",
	"    run <file>             weof                     bsr [n]\n",
	"    verbose                status                   quit\n",
	0 };

/* PHELP -- Print list of commands.
 */
static int phelp( void )
{
	int i;

	for (i=0;  helptxt[i];  i++)
	    output (helptxt[i]);

	return 0;
}
