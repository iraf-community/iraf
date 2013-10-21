/**
 *  @file  	voLog.c
 *  @author  	Mike Fitzpatrick, NOAO
 *  @date  	6/10/09
 *
 *  @brief  VOApps logging interface.  
 */
/*****************************************************************************/


#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <pthread.h>
#include <errno.h>

#include "voApps.h"
#include "voAppsP.h"


#define	SZ_FMTSPEC	25		/* max size single format spec	*/
#define	SZ_ARGVAL	128
#define EOS		0

#define TY_INT		0		/* the only types we support	*/
#define TY_DOUBLE	1
#define TY_CHAR		2


/* Public methods.
*/
void  vo_encodeString (char *buf, char *format, va_list *argp);
char *vo_doarg (va_list **argp, int dtype);
char *vo_logtime ();

static char  argVal[SZ_ARGVAL];



/**
 *  VO_APPLOG -- VOApp message logger.
 *  
 *  @fn	     vo_appLog (char *format, ...)
 *
 *  @brief  	      VOApps message logger.
 *  @param   fd       logging file descriptor
 *  @param   format   message format string  
 *  @return  nothing
 */
void
vo_appLog (FILE *fd, char *format, ...)
{
    int     len;
    char    *buf;
    va_list argp;


    va_start (argp, format);		/* encode as a single string	*/
    buf = calloc (1, (4 * SZ_LINE) );
    vo_encodeString (buf, format, &argp);
    va_end (argp);

    len = strlen (buf);			/* ensure a newline		*/
    if (strcmp ("\n", &buf[len-1]))
	strcat (buf, "\n");
    
    if (fd)
        fprintf (fd, "%s %s", vo_logtime(), buf);

    free ((void *) buf);
}


/**
 *  VO_ENCODESTRING -- Process the format to the output file, taking arguments
 *  from the list pointed to by argp as % format specs are encountered in 
 *  the input.
 *
 *  @fn	     vo_encodeString (char *buf, char *format, va_list *argp)
 *
 *  @param   buf     formatted output buffer
 *  @param   format  format string
 *  @param   argp    variable-length arguments
 *  @return
 */
void
vo_encodeString (char *buf, char *format, va_list *argp)
{
    register int ch;			/* next format char reference	*/
    char	formspec[SZ_FMTSPEC];	/* copy of single format spec	*/
    char	*fsp;			/* pointer into formspec	*/
    char  	cbuf[10];
    int	        done;			/* one when at end of a format	*/
    int	        nch = SZ_LINE;		/* one when at end of a format	*/


    while ((ch = *format++) && nch > 0) {
      if (ch == '%') {
    	fsp = formspec;
    	*fsp++ = ch;
    	done = 0;

    	while (!done) {
    	    ch = *fsp++ = *format++;

    	    switch (ch) {
    	    case EOS:
    		--format;
    		done++;
    		break;

    	    case 'l':
    		fsp--; 			/* arg size modifier; ignored for now */
    		break;

    	    case 'b':			/* nonstandard UNIX	*/
    	    case 'c':
    	    case 'd':
    	    case 'o':
    	    case 'x':
    	    case 'u':
    		*fsp = EOS;
    		strcat (buf, vo_doarg (&argp, TY_INT));
    		done++;
    		break;

    	    case 'E':			/* ANSI emulation	*/
    		*(fsp-1) = 'e';
    		goto rval;
    	    case 'G':			/* ANSI emulation	*/
    		*(fsp-1) = 'g';
    		goto rval;

    	    case 'e':
    	    case 'f':
    	    case 'g':
rval:
    		*fsp = EOS;
    		strcat (buf, vo_doarg (&argp, TY_DOUBLE));
    		done++;
    		break;

    	    case 's':
    		*fsp = EOS;
    		strcat (buf, vo_doarg (&argp, TY_CHAR));
    		done++;
    		break;
    	    }
    	}

      } else {
	  memset (cbuf, 0, 10);
	  sprintf (cbuf, "%c", ch);
    	  strcat (buf, cbuf);
      }

      nch = SZ_LINE - strlen (buf);	/* prevent overflow		*/
    }
}


/**
 *  VO_DOARG -- Encode a single argument acording to the data type.
 *
 *  @fn     static char *vo_doarg (va_list **argp, int dtype)
 *
 *  @param  argp	argument list
 *  @param  dtype	data type
 *
 */
char * 
vo_doarg (va_list **argp, int dtype)
{
    int		ival;
    double	dval;
    char	*cptr;


    memset (argVal, 0, SZ_ARGVAL);

    /* Pass the data value to be encoded, bump argument pointer by the
    ** size of the data object.  If there is no data value the case
    ** is a no-op.
    */
    switch (dtype) {
    case TY_INT:
        ival = va_arg ((**argp), int);
        sprintf (argVal, "%d", ival);
        break;
    case TY_DOUBLE:
        dval = va_arg ((**argp), double);
        sprintf (argVal, "%g", dval);
        break;
    case TY_CHAR:
        cptr = va_arg ((**argp), char *);
        sprintf (argVal, "%s", cptr);
        break;
    }

    return (argVal);
}



/**
 *  LOGTIME - Generate a time string for the log.
 *
 *  @fn	static char *vo_logtime()
 */
char * 
vo_logtime ()
{
    time_t      t  = time (NULL);
    struct tm  *tm = gmtime (&t);
    static char tstr[128];

    memset (tstr, 0, 128);
    strftime (tstr, 128, "%m%d %T", tm);

    return (tstr);
}
