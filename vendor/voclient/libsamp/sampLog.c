/**
 *  SAMPLOG.C -- SAMP trace and logging interface.
 *
 *  @file  	sampLog.c
 *  @author  	Mike Fitzpatrick
 *  @date  	6/10/09
 *
 *  @brief  SAMP trace and logging interface.  
 *
 */
/*****************************************************************************/


#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "samp.h"


#define	SZ_FMTSPEC	25		/* max size single format spec	*/
#define EOS		0

#define T_INT		0		/* the only types we support	*/
#define T_DOUBLE	1
#define T_CHAR		2



/* Private methods.
*/
static void    samp_encodeString (char *buf, char *format, va_list *argp);
static char   *samp_doarg (va_list **argp, int dtype);
static char   *logtime ();



/**
 *  SAMPLOG -- SAMP message logger.
 *  
 *  @brief   SAMP message logger.
 *  @fn	     sampLog (handle_t handle, char *format, ...)
 *
 *  @param   handle     SAMP handle
 *  @param   format   	message format string  
 *  @return  nothing
 */
void
sampLog (handle_t handle, char *format, ...)
{
    Samp    *sampP = samp_H2P (handle);     /* get struct pointer   */
    char    *buf;
    int      len;
    va_list  argp;


    va_start (argp, format);		/* encode as a single string	*/
    buf = calloc (1, (4 * SZ_LINE) );
    (void) samp_encodeString (buf, format, &argp);
    va_end (argp);

    len = strlen (buf);			/* ensure a newline		*/
    if (strcmp ("\n", &buf[len-1]))
	strcat (buf, "\n");
    
    if (sampP->logfd)
        fprintf (sampP->logfd, "[%s] %s", logtime(), buf);

    if (sampP->debug)
        fprintf (stderr, "[%s] %s", logtime(), buf);

    if (buf) free ((void *) buf);
}


/**
 *  SAMPTRACE -- SAMP tracer.
 *  
 *  @brief   SAMP tracer.
 *  @fn	     sampTrace (handle_t handle, char *format, ...)
 *
 *  @param   handle     SAMP handle
 *  @param   format   	message format string  
 *  @return  nothing
 */
void
sampTrace (handle_t handle, char *format, ...)
{
    Samp    *sampP = samp_H2P (handle);     /* get struct pointer   */
    char    *buf;
    int      len;
    va_list  argp;


    if (!sampP->trace)
	return;

    va_start (argp, format);		/* encode as a single string	*/
    buf = calloc (1, (4 * SZ_LINE) );
    (void) samp_encodeString (buf, format, &argp);
    va_end (argp);

    len = strlen (buf);			/* ensure a newline		*/
    if (strcmp ("\n", &buf[len-1]))
	strcat (buf, "\n");
    
    fprintf (stderr, "[%s] %s", logtime(), buf);

    if (buf) free ((void *) buf);
}



/**************************************************************************
 *  Private Methods
 *************************************************************************/

/**
 *  SAMP_ENCODESTRING -- Process the format to the output file, taking arguments
 *  from the list pointed to by argp as % format specs are encountered in 
 *  the input.
 *
 *  @fn	     samp_encodeString (char *buf, char *format, va_list *argp)
 *
 *  @param   buf     formatted output buffer
 *  @param   format  format string
 *  @param   argp    variable-length arguments
 *  @return
 */
static void
samp_encodeString (char *buf, char *format, va_list *argp)
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
    		strcat (buf, samp_doarg (&argp, T_INT));
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
    		strcat (buf, samp_doarg (&argp, T_DOUBLE));
    		done++;
    		break;

    	    case 's':
    		*fsp = EOS;
    		strcat (buf, samp_doarg (&argp, T_CHAR));
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
 *  SAMP_DOARG -- Encode a single argument acording to the data type.
 *
 *  @fn     static char *samp_doarg (va_list **argp, int dtype)
 *
 *  @param  argp	argument list
 *  @param  dtype	data type
 *
 */
static char * 
samp_doarg (va_list **argp, int dtype)
{
    int		ival;
    double	dval;
    char	*cptr;
    static char val[128];


    /* Pass the data value to be encoded, bump argument pointer by the
    ** size of the data object.  If there is no data value the case
    ** is a no-op.
    */
    memset (val, 0, 128);

    switch (dtype) {
    case T_INT:
        ival = va_arg ((**argp), int);
        sprintf (val, "%d", ival);
        break;
    case T_DOUBLE:
        dval = va_arg ((**argp), double);
        sprintf (val, "%g", dval);
        break;
    case T_CHAR:
        cptr = va_arg ((**argp), char *);
        sprintf (val, "%s", cptr);
        break;
    }

    return (val);
}



/**
 *  LOGTIME - Generate a time string for the log.
 *
 *  @fn	static char *logtime()
 */
static char * 
logtime ()
{
    time_t      t  = time (NULL);
    struct tm  *tm = gmtime (&t);
    static char tstr[128];

    memset (tstr, 0, 128);
    strftime (tstr, 128, "%m%d %T", tm);

    return (tstr);
}
