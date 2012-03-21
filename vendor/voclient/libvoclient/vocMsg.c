/******************************************************************************
 *
 *  VOClient Messaging Interface
 *
 *            msg = newCallMsg (objid, method, nparams)
 *          msg = newResultMsg (status, type, nitems)
 *                msg = newMsg (msgclass, str)
 *
 *              msgAddIntParam (msg, ival)
 *            msgAddFloatParam (msg, dval)
 *           msgAddStringParam (msg, str)
 *
 *             msgAddIntResult (msg, ival)
 *           msgAddFloatResult (msg, dval)
 *          msgAddStringResult (msg, str)
 *
 *                     sendMsg (fd, msg)
 *                     freeMsg (msg)
 *
 *             res = getResult (fd)                    # for reading RESULT msgs
 *                  freeResult (res)
 *
 *         stat = resultStatus (res)
 *           type = resultType (res)
 *       nitems = resultLength (res)
 *
 *         ival = getIntResult (res, index)
 *       dval = getFloatResult (res, index)
 *       str = getStringResult (res, index)
 *
 *  M. Fitzpatrick, NOAO, June 2006
 */

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <errno.h>
#include <signal.h>
#include <netdb.h>
#include <fcntl.h>
#include <sys/uio.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


extern	VOClient *vo;

#define SELWIDTH	32


/*  Private interface procedure
*/
static int       msg_write (int fd, char *buf, int nbytes);
static int       msg_read (int fd, char *buf, int maxbytes, int *nbytes);
static void      msg_addParam (vocMsg_t *msg, int type, char *str);
static vocRes_t *msg_scanResult (char *result);
static int       msg_scanInt (char **ip);
static char *    msg_scanString (char **ip, char *val, int maxchar);
static void * 	 msg_readBulk (int fd, int *len, int *status);
static int   	 msg_readBulkToFile (int fd, char *fname, int nexpect,
			int overwrite, int *len);

static int	 msg_onsig(int sig, int *arg1, int *arg2);



/*****************************************************************************
 *  NEWCALLMSG --  Create a CALL message structure and initialize with
 *  the requested parameters.  Structures are allocated dynamically,
 *  the caller is responsible for freeing the struct when finished.
 */
vocMsg_t *
msg_newCallMsg (ObjectID objid, char *method, int nparams)
{
    vocMsg_t *msg = (vocMsg_t *) calloc (1, sizeof (vocMsg_t));

    msg->type	 = MSG_CALL;
    msg->objId	 = objid;
    strncpy (msg->method, method, strlen (method));
    msg->nparams = nparams;

    sprintf (msg->message, "CALL { %ld %s %d }\n", 
	(long) objid, method, nparams);

    return (msg);
}


/*****************************************************************************
 *  NEWRESULTMSG -- Create a new RESULT message context and initialize
 *  with the requested parameters.  The caller is responsible for freeing
 *  the struct when complete.
 */
vocMsg_t *
msg_newResultMsg (int status, int type, int nitems)
{
    vocMsg_t *msg = (vocMsg_t *) calloc (1, sizeof (vocMsg_t));

    msg->type	 = MSG_RESULT;
    msg->status	 = status;
    msg->restype = type;
    msg->nitems  = nitems;

    sprintf (msg->message, "RESULT { %d %d %d }\n", status, type, nitems);

    return (msg);
}


/*****************************************************************************
 *  NEWMSG -- Create a new MSG message context and initialize with the 
 *  requested parameters.  The caller is responsible for freeing the struct 
 *  when complete.
 */
vocMsg_t *
msg_newMsg (char *msgclass, char *str)
{
    vocMsg_t *msg = (vocMsg_t *) calloc (1, sizeof (vocMsg_t));

    msg->type	 = MSG_MESSAGE;
    strncpy (msg->msgclass, msgclass, strlen (msgclass));
    strncpy (msg->msgstr, str, strlen (str));

    sprintf (msg->message, "MSG { %s %s }\n", msgclass, str);
    return (msg);
}


/*****************************************************************************
 *  SHUTDOWNMSG -- Create an END message to the VOClient Server to shut it down.
 */
vocMsg_t *
msg_shutdownMsg ()
{
    vocMsg_t *msg = (vocMsg_t *) calloc (1, sizeof (vocMsg_t));
    sprintf (msg->message, "END");
    return (msg);
}

                    
/*****************************************************************************
 *  QUITMSG -- Create a QUIT message to the VOClient Server to tell it we're
 *  leaving but that it should keep running
 */
vocMsg_t *
msg_quitMsg ()
{
    vocMsg_t *msg = (vocMsg_t *) calloc (1, sizeof (vocMsg_t));
    sprintf (msg->message, "QUIT");
    return (msg);
}


/*****************************************************************************
 *  ACKMSG -- Create an ACK message to the VOClient Server.
 */
vocMsg_t *
msg_ackMsg ()
{
    vocMsg_t *msg = (vocMsg_t *) calloc (1, sizeof (vocMsg_t));
    sprintf (msg->message, "ACK");
    return (msg);
}


/*****************************************************************************
 *  NOACKMSG -- Create an NO-ACK message to the VOClient Server.
 */
vocMsg_t *
msg_noackMsg ()
{
    vocMsg_t *msg = (vocMsg_t *) calloc (1, sizeof (vocMsg_t));
    sprintf (msg->message, "NOACK");
    return (msg);
}

                    
/*****************************************************************************
 *  SENDMSG -- Send the message to the VOClient Server and wait for the ACK.  
 *  The simple form of the message returns the result handle, for the raw 
 *  message we only send to allow to a bullk return object.
 */
vocRes_t *
msg_sendMsg (int fd, vocMsg_t *msg)
{
    int stat = msg_sendRawMsg (fd, msg);
    return ( (stat != ERR) ? msg_getResult (fd) : (vocRes_t *)NULL );
}

int
msg_sendRawMsg (int fd, vocMsg_t *msg)
{
    int stat = OK;

    if (MSG_DEBUG) 
	fprintf (stderr, "SND:  '%s'\n", msg->message);

    strcat (msg->message, "\n");
    stat = msg_write (fd, msg->message, (int)strlen (msg->message));

    if (MSG_DEBUG) 
	fprintf (stderr, "SND: len=%d of %d\n", stat,(int)strlen(msg->message));

    return (stat);
}


/*****************************************************************************
 * GETRESULT -- Read and parse a result message.
 */
vocRes_t *
msg_getResult (int fd)
{
    char c, last_ch = '\0', complete = 0;
    int  i=0, stat, nread = 0;
    char buf[SZ_MSGBUF], rembuf[SZ_MSGBUF];
    vocRes_t *res = (vocRes_t *) NULL;

    memset (buf, 0, SZ_MSGBUF);		/* clear buffers		*/
    memset (rembuf, 0, SZ_MSGBUF);

    while (!complete) {			/* read the result message	*/
        stat = msg_read (fd, &c, 1, &nread);
	if (c == ';' && last_ch == '}') {
	    buf[i++] = c;
	    complete++; 
	} else if (c != '\n' && c != '\0')
	    buf[i++] = c;

	last_ch = c;
    }
    if (MSG_DEBUG) fprintf (stderr, "RCV:%d '%s'\n", complete, buf);
    
    if (complete)			/* parse a complete result	*/
        res = (vocRes_t *) msg_scanResult (buf);

    if (res->type == TY_BULK) {		/* read any bulk data to follow	*/
        int nbytes = msg_getIntResult (res, 0);

	if (nbytes > 0) {
	    /* Read a bulk dataset of a specified size.
	     */
	    res->buf = calloc (1, (nbytes+1));
	    res->buflen = nbytes;
	    stat = msg_read (fd, res->buf, nbytes, &nread);

	} else {
	    int len;

	    res->buf = (char *) msg_readBulk (fd, &len, &stat);
	    res->buflen = len;
	}
    }

    return ((vocRes_t *) res);
}


/*****************************************************************************
 * GETRESULTTOFILE -- Read and parse a result message, save bulk data to the
 * named file.
 */
vocRes_t *
msg_getResultToFile (int fd, char *fname, int overwrite)
{
    char c, last_ch = '\0', complete = 0;
    int  i=0, stat=OK, nread = 0;
    char buf[SZ_MSGBUF], rembuf[SZ_MSGBUF];
    vocRes_t *res = (vocRes_t *) NULL;


    memset (buf, 0, SZ_MSGBUF);		/* clear buffers	*/
    memset (rembuf, 0, SZ_MSGBUF);

    while (!complete && stat == OK) {
        stat = msg_read (fd, &c, 1, &nread);
	if (c == ';' && last_ch == '}') {
	    buf[i++] = c;
	    complete++; 
	} else if (c != '\n' && c != '\0')
	    buf[i++] = c;

	last_ch = c;
    }
    if (MSG_DEBUG) fprintf (stderr, "RCV:%d '%s'\n", complete, buf);
    
    if (complete)
        res = (vocRes_t *) msg_scanResult (buf);

    if (res->type == TY_BULK) {
        int nbytes = msg_getIntResult (res, 0);
	stat = msg_readBulkToFile (fd, fname, overwrite, nbytes, &res->buflen);
    }

    return ((vocRes_t *) res);
}


/*****************************************************************************
 *  ADD<type>PARAM -- Add a int/float/string parameter to an outgoing   
 *  CALL message.  We simply append to an existing message.
 */
void msg_addIntParam (vocMsg_t *msg, int ival)
{
    char str[SZ_PBUF];
    memset (str, 0, SZ_PBUF);
    sprintf (str, "%d", ival);
    msg_addParam (msg, 1, str);
}

void msg_addFloatParam (vocMsg_t *msg, double dval)
{
    char str[SZ_PBUF];
    memset (str, 0, SZ_PBUF);
    sprintf (str, "%f", dval);
    msg_addParam (msg, 2, str);
}

void msg_addStringParam (vocMsg_t *msg, char *str)
{
    msg_addParam (msg, 3, str);
}




/*****************************************************************************
 *  ADD<type>RESULT -- Add an int/float/string value to a results string.
 */
void msg_addIntResult (vocMsg_t *msg, int ival)
{
    register int i;

    for (i=strlen (msg->message); i > 0 && msg->message[i] != '}'; i--) ;
    sprintf (&msg->message[i], " 1 1 %d }", ival);
}

             
void msg_addFloatResult (vocMsg_t *msg, double dval)
{
    register int i;

    for (i=strlen (msg->message); i > 0 && msg->message[i] != '}'; i--) ;
    sprintf (&msg->message[i], " 2 1 %g }", dval);
}

            
void msg_addStringResult (vocMsg_t *msg, char *str)
{
    register int i;

    for (i=strlen (msg->message); i > 0 && msg->message[i] != '}'; i--) ;
    sprintf (&msg->message[i], " 3 %d %s }", (int)strlen (str), str);
}



/*  MSG_RESULTS -- Get various bits of information from the RESULT message.
 */

int msg_resultStatus (vocRes_t *res) { return ( res ? res->status : ERR); }
int msg_resultType   (vocRes_t *res) { return (res->type); 	}
int msg_resultLength (vocRes_t *res) { return (res->nitems); 	}


int msg_getIntResult (vocRes_t *res, int index)
{
    return (atoi (res->value[index]));
}

double msg_getFloatResult (vocRes_t *res, int index)
{
    return ((double)atof (res->value[index]));
}

char *msg_getStringResult (vocRes_t *res, int index)
{
    if (strlen (res->value[index]) > SZ_MSGSTR)
        *res->value[SZ_MSGSTR-1] = '\0';
    return (strdup(res->value[index]));
}

void *msg_getBuffer (vocRes_t *res)
{
    return ((res ? (void *)res->buf : NULL));
}



/******************************************************************************
 * PRIVATE PROCEDURES
 *****************************************************************************/


/* MSG_WRITE -- Asynchronous write of data to the server.  Write exactly
 * nbytes bytes from the buffer to the server.
 */

static int
msg_write (fd, buf, nbytes)
int	fd;				/* connection file descriptor 	*/
char 	*buf;				/* buffer to write		*/
int 	nbytes;				/* number of bytes to write	*/
{
    int n = 0, total = 0, maxbytes = nbytes;
    char *ip = (char *)buf;
    fd_set   fds, allset;
    struct timeval tv;
    SIGFUNC sigpipe;



    /* Enable a signal mask to catch SIGPIPE when the server has died.
     */
    sigpipe = (SIGFUNC) signal (SIGPIPE, (SIGFUNC)msg_onsig);


    for (total=0; total < nbytes; total += n, ip += n) {
	n = nbytes - total;
	if (maxbytes)
	    n = ( (maxbytes < n) ? maxbytes : n);
     
	FD_ZERO (&allset);
	FD_SET (fd, &allset);
	tv.tv_sec=1;
	tv.tv_usec=0;

	fds = allset;
	if ((n = select (SELWIDTH, NULL, &fds, NULL, &tv)) > 0) {
	    if (FD_ISSET(fd,&fds)) {
                if ((n = write (fd, ip, n)) < 0) {
    		    signal (SIGPIPE, sigpipe); 	/* restore the signal mask */
                    return (ERR);
	        }
	    } else {
	        printf ("socket not ready ....\n");
    		signal (SIGPIPE, sigpipe); 	/* restore the signal mask */
	        return (ERR);
	    }
	} else
	    printf ("select timeout ....\n");
    }

    signal (SIGPIPE, sigpipe); 	/* restore the signal mask */

    return ((total < nbytes) ? ERR : total);
}


/* MSG_READ -- Read data from the server.  Try to read at most maxbytes bytes
 * from the server into the buffer, return the number of bytes actually read.
 */

static int
msg_read (fd, buf, maxbytes, nbytes)
int	fd;				/* connection file descriptor 	*/
char 	*buf;				/* buffer to read		*/
int 	maxbytes;			/* max number of bytes to read	*/
int 	*nbytes;			/* number of bytes actually read*/
{
    int   nread;
    int   nleft = maxbytes;
    char  *ptr = buf;

    *nbytes = 0;
    while (nleft > 0) {
        if ( (nread = read(fd, ptr, nleft)) < 0) {
            if (errno == EINTR)
                nread = 0;          /* and call read() again */
            else
                return (ERR);
        } else if (nread == 0) {
	    nleft = 0;
            break;                  /* EOF */
	}

        nleft   -= nread;
        ptr     += nread;
	*nbytes += nread;
    }

    return (OK);
}


/*  MSG_READBULK -- Read a bulk data object from the connection stream.
 *  We don't know in advance how big this is, so read whatever is there
 *  (and hope for the best).
 */

#define SZ_CHUNK	4096
#define SZ_BULKDATA	1024000

static void *
msg_readBulk (int fd, int *len, int *status)
{
    int   i, nbytes=0, nread=0, blen, leading=1;
    void  *chunk, *data, *dp, *ep;
    char  ch;


    /* Allocate a big chunk and initial data buffer.  We'll resize the
     * output buffer as needed but want a fairly large size so we don't
     * reallocate it too often.  The 'chunk' is a much smaller size, i.e.
     * what we can reasonably expect to get from a single network read.
     */

    chunk = calloc (1, SZ_CHUNK);
    data  = calloc (1, SZ_BULKDATA);

    *status  = OK;			/* initialize			*/
    nbytes   = 0;
    dp       = data;
    ep       = data + SZ_BULKDATA;	/* save end of buffer		*/
    blen     = SZ_BULKDATA;

    while (1) {
	memset (chunk, 0, SZ_CHUNK);
        if ( (nread = read (fd, chunk, SZ_CHUNK)) < 0) {
            if (errno == EINTR)
                nread = 0;          	/* and call read() again 	*/
            else {
                *status =  ERR;
		if (nread > 0) {
	            bcopy (chunk, dp, nread);
		    nbytes += nread;
		}
		break;
            }
        } else if (nread == 1 && (ch = *((char*)chunk)) == '\n') {
	    bcopy ((char *)(chunk), dp, nread);
	    dp 	   += nread;
	    nbytes += nread;

        } else if (strncmp ("EOF", chunk+nread-3, 3) == 0) {
	    bcopy (chunk, dp, nread-2); /* copy data to output buffer 	*/
	    nbytes  += (nread - 3);	/* update counters		*/
	    dp 	    += (nread - 3);
            break;                  	/* EOF msg from server 		*/

        } else if (nread == 0)
            break;                  	/* EOF */

	if (leading) {
	    for (i=0; i < nread && ((char *)chunk)[i] == '\n'; i++)
		;
	    leading = 0;
	} else 
	    i = 0;

	bcopy ((char *)(chunk+i), dp, (nread-i)); /* copy to output buffer */

	dp 	+= (nread-i);		/* update counters		*/
	nbytes 	+= (nread-i);


	/* Reallocate the buffer for more data if we're close to filling
	 * the current buffer.  Update other counters as well.
	 */
	if ((ep-dp) < SZ_CHUNK) {
	    blen += SZ_BULKDATA;
	    data = realloc (data, blen);
	    dp = data + nbytes;
	    ep = data + blen;
	}
    }
    *len = (nbytes - 1);
    *len = nbytes;

    if (chunk) free ((void *) chunk);

    return ((void *) data);
}


/*  MSG_READBULKTOFILE -- Read a bulk data object from the connection stream
 *  into the named file.
 */

static int
msg_readBulkToFile (int fd, char *fname, int overwrite, int nexpect, int *len)
{
    int   nbytes=0, nread=0, out = 0, status, leading=1, i;
    void *chunk;


    /* Open the file in the requested mode.  If the 'overwrite' flag
     * is set we'll remove the file first and create it anew.
     */
    if (access ((char *)fname, 0) == 0) {
	if (overwrite) {
	    if (unlink (fname) < 0) {
		if (!vo->quiet)
		    fprintf (stderr, "ERROR: Cannot overwrite file '%s'\n",
			fname);
		return (ERR);
	    }
	} else {
	    if (!vo->quiet) {
		fprintf (stderr, "ERROR: Operation would verwrite file '%s'\n", 
		    fname);
	    }
	    return (ERR);
	}
    }

    /* Open the file. */
    if ((out = creat ((char *)fname, 0666)) < 0) {
        close (out);
        out = open ((char *)fname, 2);
    }

    status  = OK;			/* initialize			*/
    nbytes   = 0;
    chunk = malloc (SZ_CHUNK);

    while (1) {
	memset (chunk, 0, SZ_CHUNK);
        if ( (nread = read (fd, chunk, SZ_CHUNK)) < 0) {
            if (errno == EINTR)
                nread = 0;          	/* and call read() again 	*/
            else {
                status =  ERR;
		break;
            }

        } else if (strncmp ("EOF", chunk+nread-3, 3) == 0) {
	    nread -= 3;
	    write (out, chunk, nread);
	    nbytes += nread;		/* update counters		*/
            break;                  	/* EOF msg from server 		*/

        } else if (nread == 0)
            break;                  	/* EOF */

	if (leading) {
	    for (i=0; i < nread && ((char *)chunk)[i] == '\n'; i++)
		;
	    leading = 0;
	} else 
	    i = 0;

   	/* write data to output file 	*/
	if (write (out, (char *)(chunk+i), (nread-i)) < 0) {
	    if (!vo->quiet) {
		fprintf (stderr,
		    "rdBulkFile: Error writing to output file '%s'\n", fname);
	    }
            status =  ERR;
            break;

	} else
	    nbytes += (nread - i);	/* update counters		*/
    }
    *len = (nbytes - 1);
    *len = nbytes;

    if (nexpect > 0 && nbytes != nexpect)
	status = ERR;

    close (out);
    if (chunk) free ((void *) chunk);	/* free the temp space		*/

    return ((int) status);
}


/* MSG_ADDPARAM -- Add a parameter t  a Query.
 */
static void 
msg_addParam (vocMsg_t *msg, int type, char *str)
{
    register int i, j;
    char  head[SZ_MSGBUF], tail[SZ_MSGBUF];

    if (msg->type != MSG_CALL) {
	if (!vo->quiet) 
	    fprintf (stderr, "Invalid message type for addParam\n");
	return;
    }

    /* Clear the buffer arrays.
     */
    memset (head, 0, SZ_MSGBUF);
    memset (tail, 0, SZ_MSGBUF);

    /* Find the end of the message and backup over the arguments to 
     * copy the 'tail' of the message.
     */
    for (j=i=0; msg->message[i] && j < 5; i++) {
	if (msg->message[i] == ' ')
	    j++;
    }
    strncpy (tail, &msg->message[i], strlen (&msg->message[i]));

    /* Append the new value.
     */
    for (i=strlen (tail); i > 0 && tail[i] != '}'; i--) ;
    i = ((i-1) <= 0) ? 0 : (i-1);
    if (type == 3)
        sprintf (&tail[i], " %d '%s' }", type, str);
    else
        sprintf (&tail[i], " %d %s }", type, str);

    /* Format a new 'head' of the message with the increased 
     *  parameter count.
     */
    msg->nparams++;
    sprintf (head, "CALL { %d %s %d ", msg->objId, msg->method, msg->nparams);

    sprintf (msg->message, "%s%s", head, tail);
}


/* MSG_SCANRESULT --  Scan and parse the RESULT message.
 */
static vocRes_t *
msg_scanResult (char *result)
{
    register int i;
    char    *ip = result;
    char     buf[SZ_MSGSTR], *bp;
    vocRes_t *res = (vocRes_t *) calloc (1, sizeof (vocRes_t));
    
    /* Skip over the keyword and opening brace. 
     */
    for (ip = result; *ip && *ip != '{'; ip++)
	;
    ip++;

    res->status = msg_scanInt (&ip);
    res->type   = msg_scanInt (&ip);
    res->nitems = msg_scanInt (&ip);
	
    for (i=0; i < res->nitems; i++) {
	bp = msg_scanString (&ip, buf, SZ_MSGSTR);
	if (buf[0] != '}') {
	    memset (res->value[i], 0, SZ_MSGSTR);
	    strncpy (res->value[i], bp, SZ_MSGSTR-1);
	}
    }

    return (res);
}


/*  MSG_SCANINT --  Scan an integer from the string, leave the pointer after
 *  the last char read.
 */
static int 
msg_scanInt (char **ip)
{
    char  buf[256], *op = *ip;
    register int i;

    while (*op && isspace (*op)) 		/* skip leading blanks */
	op++;

    memset (buf, 0, 256);
    for (i=0; *op && !isspace (*op); i++)
	buf[i] = *op++;

    *ip = op;
    return (atoi (buf));
}


/*  MSG_SCANSTRING --  Scan a string from the input string, leave the pointer 
 *  after the last char read.
 */
static char * 
msg_scanString (char **ip, char *val, int maxchar)
{
    register int i;
    char *op = *ip;
    char *np = val;

    while (*op && isspace (*op)) 		/* skip leading blanks */
	op++;

    memset (val, 0, maxchar);
    for (i=0; *op && !isspace (*op); ) {
	if (*op == '"') {
    	    for (op++; *op && *op != '"'; )
	        *np++ = *op++;
	    op++;
	} else
	    *np++ = *op++;
    }
    *ip = op;

    return (val);
}


/* MSG_ONSIG -- Catch a signal.
 */
static int
msg_onsig (sig, arg1, arg2)
int     sig;                    /* signal which was trapped     */
int     *arg1;                  /* not used */
int     *arg2;                  /* not used */
{
    /* If we get a SIGPIPE writing to a server the server has probably
     * died.  Make it look like there was an i/o error on the channel.
     */
    if (sig == SIGPIPE)
        ;

    return (sig);
}

