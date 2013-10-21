#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <fcntl.h>

#define	import_spp
#include <iraf.h>

/*
 * ZZPSTR.C -- Support for debugging SPP programs.
 *
 *	   zzpstr (s1, s2)	# Write a debug message to the process stderr
 *	   zzlstr (s1, s2)	# Write a debug message to /tmp/k.log
 *   spp_printstr (s)		# GDB support function
 *  spp_printmemc (memc_p)	# GDB support function
 *
 * The procedures zzpstr and zzlstr are meant to be called from within
 * compiled SPP code to write debug messages to either the process stderr
 * or to a log file.  This is different than writing to SPP STDERR since
 * the latter is a pseudofile (it gets sent to the CL before being written
 * out).  In other words zzpstr/zzlstr are low level debug functions, 
 * comparable to a host fprintf.
 *
 * spp_printstr and spp_printmemc are called from a debugger (GDB) to
 * print char strings.  spp_printstr prints a char variable as an EOS
 * terminated string.  spp_printmemc does the same thing, but takes a Memc
 * pointer variable as input.
 *
 * The following commands can be added to your .gdbinit file to make it
 * easier to use these functions:
 *
 *	define ps
 *	call spp_printstr ($arg0)
 *	end
 *
 *	define pc
 *	call spp_printmemc ($arg0)
 *	end
 *
 * Then you can type e.g., "ps fname" to print char variable fname,
 * or "pc ip" to print the string pointed to by SPP Memc pointer "ip".
 * Both of these functions will print tabs and newlines as \t and \n,
 * and other control codes as \ooo where ooo is the octal value of the
 * character.
 */

#define	LOGFILE "/tmp/k.log"

void 	spp_printmemc (long memc_ptr);
void 	spp_printstr (XCHAR *s);



/* SPP_DEBUG -- Dummy function called to link the SPP debug functions into
 * a program.
 */
int spp_debug (void) { return (0); }


/* ZZPSTR -- Write SPP text data directly to the host stderr.  Up to two
 * strings may be ouptut.  Either may be the null pointer to disable.
 * A newline is added at the end if not present in the last string.
 */
int
zzpstr_ (XCHAR *s1, XCHAR *s2)
{
	register XCHAR *s, *ip;
	register char *op;
	char buf[4096];
	int lastch = 0;


	if ( (s = s1) ) {
	    for (ip=s, op=buf; (*op = *ip++);  op++)
		;
	    lastch = *(op-1);
	    write (2, buf, op-buf);
	}

	if ( (s = s2) ) {
	    for (ip=s, op=buf; (*op = *ip++);  op++)
		;
	    lastch = *(op-1);
	    write (2, buf, op-buf);
	}

	if (lastch != '\n')
	    write (2, "\n", 1);

	return (XOK);
}


/* ZZLSTR -- Write SPP text data to a log file.
 */
int
zzlstr_ (XCHAR *s1, XCHAR *s2)
{
	register XCHAR *s, *ip;
	register char *op;
	char buf[4096];
	int lastch = 0;
	int status = 0, fd;

	if ((fd = open (LOGFILE, O_CREAT|O_WRONLY|O_APPEND, 0644)) < 0)
	    return (fd);

	if ( (s = s1) ) {
	    for (ip=s, op=buf;  (*op = *ip++);  op++)
		;
	    lastch = *(op-1);
	    status = write (fd, buf, op-buf);
	}

	if ( (s = s2) ) {
	    for (ip=s, op=buf;  (*op = *ip++);  op++)
		;
	    lastch = *(op-1);
	    status = write (fd, buf, op-buf);
	}

	if (lastch != '\n')
	    status = write (fd, "\n", 1);

	status = close (fd);
	return (status);
}


/* SPP_PRINTSTR -- GDB callable debug function to print an EOS terminated SPP
 * string passed as a char array.
 */
void
spp_printstr (XCHAR *s)
{
	register XCHAR *ip;
	register char *op, *otop;
	static char obuf[1024];
	int ch;

	for (ip=s+1, op=obuf, otop=obuf+1020;  (ch = *ip);  ip++) {
	    if (!isprint (ch)) {
		if (ch == '\t') {
		    *op++ = '\\';
		    *op++ = 't';
		} else if (ch == '\n') {
		    *op++ = '\\';
		    *op++ = 'n';
		} else {
		    *op++ = '\\';
		    *op++ = ((ch >> 6) & 07) + '0';
		    *op++ = ((ch >> 3) & 07) + '0';
		    *op++ = ( ch       & 07) + '0';
		}
	    } else
		*op++ = ch;

	    if (op >= otop)
		break;
	}

	*op++ = '\0';
	printf ("%s\n", obuf);
	fflush (stdout);
}


/* SPP_PRINTMEMC -- GDB callable debug function to print an EOS terminated SPP
 * string passed as a pointer to char.
 */
void
spp_printmemc (long memc_ptr)
{
	XCHAR *str = (XCHAR *) ((memc_ptr - 1) * 2 - 2);
	spp_printstr (str);
}
