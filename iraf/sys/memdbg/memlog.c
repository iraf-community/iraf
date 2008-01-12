#include <stdio.h>
#include <unistd.h>

#define import_libc
#include <iraf.h>

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

#define FNAME	"mem.log"

static	FILE *fp = NULL;
static	int loglevel = 3;
static	int number = 0;

#define LOG_MALLOC	0001
#define LOG_SALLOC	0002

static void memput( const char *message );


/* MEMLOG -- User routine to log a message in sequence to the memio debug
 * log file.
 */
int memlog_ ( XCHAR *message )
{
	XCHAR *ip;
	char *op, *maxop;
	char p_message[1024];

	maxop = p_message + 1024 -1;
	for ( ip=message, op=p_message ; op < maxop && (*ip) ; op++, ip++ )
	    *op = *ip;
	*op = EOS;
	memput (p_message);

	return 0;
}


/* MEMLEV -- Set the logging level.
 */
int memlev_ ( XINT *level )
{
	loglevel = *level;

	return 0;
}


/* MEMLOG1 -- User routine to log a message in sequence to the memio debug
 * log file.
 */
int memlo1_ ( XCHAR *format, XINT *arg1 )
{
	XCHAR *ip;
	char *op, *maxop;
	char p_format[1024];
	char message[1024];

	/* Output user message. */
	maxop = p_format + 1024 -1;
	for ( ip=format, op=p_format ; op < maxop && (*ip) ; op++, ip++ )
	    *op = *ip;
	*op = EOS;
	snprintf (message, 1024, p_format, *arg1);
	memput (message);

	return 0;
}


/* MEMLOG2 -- User routine to log a message in sequence to the memio debug
 * log file.
 */
int memlo2_ ( XCHAR *format, XINT *arg1, XINT *arg2 )
{
	XCHAR *ip;
	char *op, *maxop;
	char p_format[1024];
	char message[1024];

	/* Output user message. */
	maxop = p_format + 1024 -1;
	for ( ip=format, op=p_format ; op < maxop && (*ip) ; op++, ip++ )
	    *op = *ip;
	*op = EOS;
	snprintf (message, 1024, p_format, *arg1, *arg2);
	memput (message);

	return 0;
}


/* MEMLOGS -- User routine to log a message in sequence to the memio debug
 * log file.
 */
int memlos_ ( XCHAR *format, XCHAR *strarg )
{
	XCHAR *ip;
	char *op, *maxop;
	char p_format[1024];
	char p_strarg[1024];
	char message[1024];

	/* Output user message. */
	maxop = p_format + 1024 -1;
	for ( ip=format, op=p_format ; op < maxop && (*ip) ; op++, ip++ )
	    *op = *ip;
	*op = EOS;
	maxop = p_strarg + 1024 -1;
	for ( ip=strarg, op=p_strarg ; op < maxop && (*ip) ; op++, ip++ )
	    *op = *ip;
	*op = EOS;
	snprintf (message, 1024, p_format, p_strarg);
	memput (message);

	return 0;
}


/* MEMPUT -- Log a message in sequence to the memio debug log file.
 */
static void memput ( const char *message )
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
	fprintf (fp, "%s", message);
	fprintf (fp, "\n");

	fflush (fp);
}


/* ZMEMLG -- Used internally by the MEMIO routines.
 */
int zmemlg_ ( XINT *addr, XINT *retaddr, XINT *action, XINT *class, 
	      XCHAR *format, XINT *arg1, XINT *arg2 )
{
	XCHAR *ip;
	char *op, *maxop;
	char p_format[1024];
	char s_action[2];

	if (!(loglevel & *class))
	    return 0;

	maxop = p_format + 1024 -1;
	for ( ip=format, op=p_format ; op < maxop && (*ip) ; op++, ip++ )
	    *op = *ip;
	*op = EOS;
	s_action[0] = *action;
	s_action[1] = '\0';

	if (fp == NULL) {
	    unlink (FNAME);
	    if ((fp = fopen (FNAME, "a")) == NULL)
		return 0;
	}

	fprintf (fp, "%10lx  %08d %8lx  %s %ld  ",
		 (long)(*addr), number++, (long)(*retaddr), 
		 s_action, (long)(*class));
	fprintf (fp, p_format, *arg1, *arg2);
	fprintf (fp, "\n");
	fflush (fp);

	return 0;
}
