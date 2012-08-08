#include <stdio.h>

/* MEMLOG -- SPP callable routines for logging MEMIO debug messages and
 * user application messages in sequence to the mem.log file.
 *
 *		 memlog (message)
 *		memlog1 (message, arg1)
 *		memlog2 (message, arg1, arg2)
 *		memlogs (message, strarg)
 *		 memlev (loglevel)
 *
 * Memlog logs a simple message string.  Memlog[12] allow one or two integer
 * arguments.  Memlogs allows one string argument.
 */

#define	FNAME	"mem.log"
#define	XCHAR	short

static	FILE *fp = NULL;
static	int loglevel = 3;
static 	int number = 0;

#define	LOG_MALLOC	0001
#define	LOG_SALLOC	0002

static void memput();


/* MEMLOG -- User routine to log a message in sequence to the memio debug
 * log file.
 */
memlog_ (message)
XCHAR	*message;
{
	register XCHAR *ip;
	register char *op;
	char	p_message[1024];

	for (ip=message, op=p_message;  *op++ = *ip++;  )
	    ;
	memput (p_message);
}


/* MEMLEV -- Set the logging level.
 */
memlev_ (level)
int	*level;
{
	loglevel = *level;
}


/* MEMLOG1 -- User routine to log a message in sequence to the memio debug
 * log file.
 */
memlo1_ (format, arg1)
XCHAR	*format;
int	*arg1;
{
	register XCHAR *ip;
	register char *op;
	char	p_format[1024];
	char	message[1024];

	/* Output user message. */
	for (ip=format, op=p_format;  *op++ = *ip++;  )
	    ;
	sprintf (message, p_format, *arg1);
	memput (message);
}


/* MEMLOG2 -- User routine to log a message in sequence to the memio debug
 * log file.
 */
memlo2_ (format, arg1, arg2)
XCHAR	*format;
int	*arg1;
int	*arg2;
{
	register XCHAR *ip;
	register char *op;
	char	p_format[1024];
	char	message[1024];

	/* Output user message. */
	for (ip=format, op=p_format;  *op++ = *ip++;  )
	    ;
	sprintf (message, p_format, *arg1, *arg2);
	memput (message);
}


/* MEMLOGS -- User routine to log a message in sequence to the memio debug
 * log file.
 */
memlos_ (format, strarg)
XCHAR	*format;
XCHAR	*strarg;
{
	register XCHAR *ip;
	register char *op;
	char	p_format[1024];
	char	p_strarg[1024];
	char	message[1024];

	/* Output user message. */
	for (ip=format, op=p_format;  *op++ = *ip++;  )
	    ;
	for (ip=strarg, op=p_strarg;  *op++ = *ip++;  )
	    ;
	sprintf (message, p_format, p_strarg);
	memput (message);
}


/* MEMPUT -- Log a message in sequence to the memio debug log file.
 */
static void
memput (message)
char	*message;
{
	/* Open logfile. */
	if (fp == NULL) {
	    unlink (FNAME);
	    if ((fp = fopen (FNAME, "a")) == NULL)
		return;
	}

	/* Output sequence number. */
	fprintf (fp, "%10s  %08d %8s  - -  ",
	    "------", number++, "------");

	/* Output message. */
	fprintf (fp, message);
	fprintf (fp, "\n");

	fflush (fp);
}


/* ZMEMLG -- Used internally by the MEMIO routines.
 */
zmemlg_ (addr, retaddr, action, class, format, arg1, arg2)
int	*addr, *retaddr;
int	*action, *class;
XCHAR	*format;
int	*arg1, *arg2;
{
	register XCHAR *ip;
	register char *op;
	char	p_format[1024];
	char	s_action[2];

	if (!(loglevel & *class))
	    return;

	for (ip=format, op=p_format;  *op++ = *ip++;  )
	    ;
	s_action[0] = *action;
	s_action[1] = '\0';

	if (fp == NULL) {
	    unlink (FNAME);
	    if ((fp = fopen (FNAME, "a")) == NULL)
		return;
	}

	fprintf (fp, "%10x  %08d %8x  %s %d  ",
	    *addr, number++, *retaddr, s_action, *class);
	fprintf (fp, p_format, *arg1, *arg2);
	fprintf (fp, "\n");
	fflush (fp);
}
