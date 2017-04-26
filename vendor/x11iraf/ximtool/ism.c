#include <stdio.h>
#include <stdlib.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <fcntl.h>
#include <sys/un.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Tcl/tcl.h>
#include <Obm.h>
#include <ObmW/Gterm.h>
#include "ximtool.h"

/*
 * ISM.C -- IRAF Image Support Module (ISM) routines.
 *
 *	        xim_ismOpen  (xim)
 *	       xim_ismClose  (xim)
 *
 *	   ism_connectClient (chan_port, source, id)
 *      ism_disconnectClient (chan)
 *	             ism_io  (chan, fd_addr, id_addr)
 *
 * 	        ism_execute  (xim, task)
 * 	       ism_evaluate  (object, command)
 *
 *           ism_openSocket  (path)
 *	           ism_read  (fd, ptr, nbytes)
 *	          ism_write  (fd, ptr, nbytes)
 */



#define	CALLBACK	0
#define	QUIT		1
#define	SEND		2
#define	READY		3


#define MAXCONN         5
#define MAX_TRY         5

void 	     xim_ismOpen(), xim_ismClose();
IsmModule    ismNameToPtr();

static void  ism_connectClient(), ism_disconnectClient(), ism_io();
static int   ism_read(), ism_write(), ism_type(), ism_parseSend();
static int   ism_openSocket();
static IsmIoChanPtr ism_getChannel();
static char *ism_parse();

static int ism_debug = 0;
extern int errno;


/* WCS/Pixel ISM client callbacks. 
 */
#define	WCSPIX_CMD	DEF_ISM_CMD

void  wcspix_connect(), wcspix_disconnect(), wcspix_command();


/* Definitions for the supported ISM Modules. */
int	  ism_nmodules  = 0;

ismModule ism_modules[] = {
   {"wcspix", WCSPIX_CMD, wcspix_connect, wcspix_disconnect, wcspix_command, 0},
};



/* XIM_ISMOPEN -- Initialize the ISM protocol module and ready the module to
 * accept client connections and begin processing client requests.  This
 * procedure only opens the ISM connection port, once clients are connected
 * they negotiate for a separate channel and I/O resumes there.  Once,
 * connected, the client disconnects from this port and it is free to be used
 * by another client.  Unix sockets are used since we only want to connect to
 * local processes and we want to use the uid for a unique address.
 */
void
xim_ismOpen (xim)
register XimDataPtr xim;
{
        register int s = 0;
        register IsmIoChanPtr chan = &(xim->ism_chan);
        char path[SZ_FNAME], *ism_path;


	if (getenv("DEBUG_ISM") != NULL)
	    ism_debug = atoi(getenv("DEBUG_ISM"));

        /* Setting the addr to "none" or the null string disables ISM
         * socket support.
         */
        if (!xim->ism_addr[0] || strcmp(xim->ism_addr,"none")==0)
            return ;

        /* Get path to be used for the unix domain socket. */
	if ((ism_path = getenv ("ISMDEV")) != NULL) {
	    char *dev = ism_path;

	    while (1) {
		if (*dev == (char)NULL) {
		    dev = ism_path;
		    break;
		} else if (*dev == ':') {
		    ++dev;
		    break;
		}
		dev++;
	    }
            sprintf (path, dev, getuid());
	} else
            sprintf (path, xim->ism_addr, getuid());
        unlink (path);

	if ((s = ism_openSocket (path))) {
            /* Fill in i/o channel descriptor. */
            chan->xim = (XtPointer) xim;
            chan->datain = s;
            chan->dataout = s;
            chan->connected = 0;
            strncpy (chan->name, "", SZ_FNAME);
            strncpy (chan->path, path, SZ_FNAME);

	    if (ism_debug) printf ("opened connection on '%s' %d\n", path, s);

            /* Register connectClient callback. */
            chan->id = xim_addInput (xim, s, ism_connectClient,
		    (XtPointer)chan);
	}

	ism_nmodules = XtNumber (ism_modules);

	/* Set the default ISM path in the GUI. */
	sprintf (path, "wcspix_cmd {%s}", DEF_ISM_CMD);
	wcspix_message (xim, path);
}


/* XIM_ISMCLOSE -- Close down the ISM protocol module.  Disconnect all connected
 * clients and close the port.
 */
void
xim_ismClose (xim)
register XimDataPtr xim;
{
	register IsmIoChanPtr chan = &(xim->ism_chan);
	register int i;

	/* Send a 'quit' message to all connected clients. */
	for (i=0, chan=NULL; i < XtNumber(xim->ism_client); i++) {
	    chan = &xim->ism_client[i];
            if (chan->id) {
                xim_removeInput (xim, chan->id);
                chan->id = NULL;
	    }

	    if (chan->connected)
		ism_write (chan->dataout, "quit", 4);

            close (chan->datain);
	}

	/* Close the ISM request socket. */
	chan = &(xim->ism_chan);
        if (chan->id) {
            xim_removeInput (xim, chan->id);
            chan->id = NULL;
        }
        close (chan->datain);
        unlink (chan->path);
}


/* ISMNAMETOPTR -- Utility procedure to lookup an ISM struct pointer given
 * the ISM name.
 */
IsmModule
ismNameToPtr (name)
char	*name;
{
        IsmModule ism;
        register int i;

        for (i=0; i < ism_nmodules; i++) {
            ism = &ism_modules[i];
            if (strcmp (name, ism->name) == 0)
                return (ism);
        }
	return ((IsmModule)NULL);
}


/*******************************
 *  TRANSPORT LAYER PROCEDURES
 ******************************/


/* ISM_CONNECTCLIENT -- Called when a client has attempted a connection on
 * a socket port.  Accept the connection and set up a new i/o channel to
 * communicate with the new client.
 */
static void
ism_connectClient (chan, source, id)
IsmIoChanPtr chan;
int *source;
XtPointer id;
{
	register XimDataPtr xim = (XimDataPtr) chan->xim;
	register int s;

	/* Accept connection. */
	if ((s = accept ((int)*source, (struct sockaddr *)0, (int *)0)) < 0)
	    return;
	/*if (fcntl (s, F_SETFL, O_RDWR|O_NDELAY) < 0) {*/
	if (fcntl (s, F_SETFL, O_NDELAY) < 0) {
	    close (s);
	    return;
	}

	/* Fill in the ISM i/o channel descriptor. */
	chan->datain = s;
	chan->dataout = s;
	chan->connected = 1;
	chan->id = xim_addInput (xim, s, ism_io, (XtPointer)chan);
}


/* ISM_DISCONNECTCLIENT -- Called to close a client connection when EOF is
 * seen on the input port.  Close the connection and free the channel
 * descriptor.
 */
static void
ism_disconnectClient (chan)
register IsmIoChanPtr chan;
{
	close (chan->datain);
	if (chan->id) {
	    xim_removeInput (chan->xim, chan->id);
	    chan->connected = 0;
	    chan->id = NULL;
	}
}


/* ISM_IO -- Xt file i/o callback procedure, called when there is input
 * pending on the data stream to the ximtool client.
 */
static void
ism_io (chan, fd_addr, id_addr)
IsmIoChanPtr chan;
int *fd_addr;
XtInputId *id_addr;
{
    register XimDataPtr xim = (XimDataPtr) chan->xim;
    register IsmModule ism;
    IsmIoChanPtr new_chan;
    int     datain = *fd_addr;
    int     dataout = chan->dataout;
    int	    s, n, ip, type, count = 0;
    char    name[SZ_FNAME], path[SZ_FNAME];
    char    message[2*SZ_ISMBUF+1];
    char    buf[SZ_ISMBUF+1];
    char    *text = NULL;
    static  int	incomplete_msg = 0;
    static  int null_count = 0;
    static  long pkt=0, nread=0;


    /* Read the message. */
    bzero (buf, SZ_ISMBUF+1);
    count = read (datain, buf, SZ_ISMBUF);
    nread += count;
    if (count > SZ_ISMBUF || count == 0) {
        if (null_count++ > MAX_TRY) {
	    null_count = 0;
            ism_disconnectClient (chan);
	}
        return;
    }

    /* Build up a complete message buffer including any incomplete
     * text from the last read.
     */
    bzero (message, 2*SZ_ISMBUF+1);
    if (chan->msgbuf[0]) {
        n = strlen(chan->msgbuf);

        memmove (message, chan->msgbuf, n);
        memmove (&message[n], buf, count);
	count += n;
    } else {
        bcopy (buf, message, count);
    }
    bzero (chan->msgbuf, SZ_ISMBUF);

    if (ism_debug >= 2)
	printf("\nism_io: nread=%d pkt=%d count=%d n=%d\n",nread,pkt++,count,n);


    ip = 0;
    incomplete_msg = 0;
    while (text = ism_parse (message, &ip, &incomplete_msg, count)) {

        if (incomplete_msg) {
	    /* Save the incomplete message to the buffer for later parsing.
	     */
	    if (ism_debug >= 2)
		printf ("INCOMPLETE '%s' ip=%d len=%d\n", text,ip,strlen(text));
            strcpy (chan->msgbuf, text);
	    break;
	}


	/*  Messages 
	 *  
	 *     CALLBACK - Negotiate a connection on another socket
	 *     QUIT	- client is shutting down and disconnecting
	 *     SEND	- send a message to another object
	 *     READY	- client is ready to begin processing
	 */
        switch (ism_type (text)) {
	case CALLBACK:

	    /* Get the requesting client's name. */
	    sscanf (text, "connect %s", name);

	    /* Get a new i/o channel. */
	    if (new_chan = ism_getChannel (xim)) {

                /* Get path to be used for the unix domain socket. */
                sprintf (path, DEF_ISM_TEMPLATE, getuid(), new_chan->id);
                unlink (path);

		if (ism_debug)
		    printf("ism_io: CONNECT '%s' on socket '%s'\n", name, path);

	        if ((s = ism_openSocket (path))) {
                    /* Fill in i/o channel descriptor. */
                    new_chan->xim = (XtPointer) xim;
                    new_chan->datain = s;
                    new_chan->dataout = s;
                    new_chan->connected = 0;
                    strncpy (new_chan->path, path, SZ_FNAME);
                    strncpy (new_chan->name, name, SZ_FNAME);

                    /* Register connectClient callback. */
                    new_chan->id = xim_addInput (xim, s, ism_connectClient,
		            (XtPointer)new_chan);
	        }
	    }

	    /* Create a new OBM object for the client, save the client name
	     * one the channel descriptor so we'll know who disconnected.
	     */
	    if (ismObjects (name) == 0)
	        obmNewObject (xim->obm, name, "Client", NULL, NULL, 0);
	    strcpy (chan->name, name);

	    /* Now tell the client to call us back on the new channel */
	    sprintf (buf, "connect %s", path);
	    if (ism_debug) printf ("ism_io: msg '%s'\n", buf);
	    ism_write (dataout, buf, strlen(buf));

	    /* Hang up the current connection. */
	    if (ism_debug) printf ("CALLBACK: disconnecting '%s'\n",chan->path);
	    ism_disconnectClient (chan);
	    
	    break;

        case READY:
            if (ism_debug) printf ("READY: ready '%s'\n", chan->name);

            /* Execute the ISM startup callback to initialize it.
	     */
            for (n=0; n < XtNumber (ism_modules) ; n++) {
                ism = &ism_modules[n];
                if (strcmp (chan->name, ism->name) == 0) {
		    ism->connected = 1;
		    ism->chan = chan;
                    (*ism->startupCB) (xim, ism);
		}
            }
            break;

        case QUIT:
            if (ism_debug)
    	        printf ("QUIT: quit '%s' on '%s'\n", chan->name, chan->path);

            /* Execute the ISM shutdown callback.
	     */
            for (n=0; n < XtNumber (ism_modules) ; n++) {
                ism = &ism_modules[n];
                if (strcmp (chan->name, ism->name) == 0) {
                    (*ism->shutdownCB) (xim, ism);
		    ism->connected = 0;
		    ism->chan = (IsmIoChanPtr) NULL;
		}
            }
            ism_disconnectClient (chan);

            break;

	case SEND:
	    /* Deliver the message to the named object.
	     */
	    ism_parseSend (text, name, buf);
	    if (ism_debug >= 3)
		printf ("SEND: len=%d '%s'->'%.45s'\n", strlen(buf), name, buf);
	    xim_message (xim, name, buf);
	    break;

	default:
	    fprintf (stderr, "ism_io: Unknown message '%s'\n", text);
	    break;
	}
    }
}


/* ISM_PARSE -- Parse the client message, returning the type as the function
 * value. 
 */
static char *
ism_parse (msg, ip, incomplete, maxch)
char	*msg;
int	*ip;
int	*incomplete;
int	maxch;
{
	register int j, i = *ip, count=0;
	char	text[SZ_ISMBUF+1];

	if (msg[*ip] == '\0') {
/*	    *incomplete = 1;*/
	    return (NULL);
	}

	/* Zero the retrun buffer and skip any leading NULL input chars. */
	bzero (text, SZ_ISMBUF+1);
	while ((msg[i] == '\0') && i < maxch) i++;

	/* Copy the message up to the terminating NULL. */
	for (j=0; (msg[i] != '\0') && (i < maxch); )
	    text[j++] = msg[i++];
	text[j] = msg[i];

	/* If we hit the end of the input buffer without seeing a null 
	 * then we're parsing a partial message.
	 */
	*incomplete = (i < maxch ? 0 : 1);

	/* update the position ptr */
	while ((msg[i] == '\0') && i < maxch) i++;
	*ip = i;

        if (ism_debug >= 3 && text[0]) {
	    printf ("ism_parse: ip=%d msg=%d i=%d j=%d inc=%d\n",
		*ip, msg[i], i, j, *incomplete);
	}
	    

	return (text[0] ? text : NULL);
}


/* ISM_TYPE -- Determine the message type.
 */
static int
ism_type (message)
char	*message;
{
	register char *ip;

	for (ip=message; isspace(*ip); ip++) ;		/* skip whitespace */

	if (strncmp (ip, "connect", 7) == 0)
	    return (CALLBACK);
	else if (strncmp (ip, "quit", 4) == 0)
	    return (QUIT);
	else if (strncmp (ip, "ready", 4) == 0)
	    return (READY);
	else if (strncmp (ip, "send", 4) == 0)
	    return (SEND);
	else
	    return (ERR);
}


/* ISM_PARSESEND -- Parse the client SEND message.
 */
static int
ism_parseSend (msg, object, text) 
char	*msg;
char	*object;
char	*text;
{
	register int i=0, ip=4, count=0;

	/* skip leading whitespace */
	for ( ; isspace(msg[ip]) && msg[ip]; ip++) ;	

	/* Get the object name */
	for (i=0; !isspace(msg[ip]) && msg[ip]; i++, ip++)
	    object[i] = msg[ip];
	object[i] = '\0';

	for ( ; msg[ip] != '{'; ip++) ;		/* skip to open bracket */

	/* Get the message text.  Count brackets so we can pass
	 * Tcl code properly.
	 */
	for (i=0, ip++; msg[ip]; i++, ip++) {
	    text[i] = msg[ip];
	    if (msg[ip] == '{') 
		count++;
	    else if (msg[ip] == '}') {
		if (count <= 0) { ip++; break; }
		count--;
	    }
	}
	text[i] = '\0';
}


/* ISM_EVALUATE -- Evaluate a command for the named object.  Used by the
 * clientEvaluate() method.  Can also be used by the rest of ximtool to
 * send messages to the ISM, which will be ignored if the client is not
 * connected.
 */
ism_evaluate (xim, object, command)
register XimDataPtr xim;
char	*object;
char	*command;
{
	register IsmIoChanPtr chan;
	register int i=0;
	register int len = strlen(command) + 1;
	char *buf = XtCalloc (len+1, sizeof(char));

	for (i=0; i < XtNumber (xim->ism_client); i++) {
	    chan = &xim->ism_client[i];
	    if (chan->connected && strcmp (chan->name, object) == 0) {
		sprintf (buf, "%s\0", command);
		len = strlen (buf) + 1;       	/* +1 to send the NULL */
		ism_write (chan->dataout, buf, len);
        	if (ism_debug >= 2) printf("writing %d bytes: '%s'\n", len,buf);
		break;
	    }
	}

	XtFree ((char *)buf);
}


/* ISM_MESSAGE -- Convenience wrapper for the evaluate procedure.
 */
ism_message (xim, object, command)
register XimDataPtr xim;
char	*object, *command;
{
	ism_evaluate (xim, object, command);
}


/* ISM_OPENSOCKET --  Open a unix socket on the named path.
 */
static int
ism_openSocket (path)
char	*path;					/* path to the socket */
{
        int addrlen, s = 0, on = 1;
        struct sockaddr_un sockaddr;

        if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
            goto err;

        memset ((void *)&sockaddr, 0, sizeof(sockaddr));
        sockaddr.sun_family = AF_UNIX;
        strcpy (sockaddr.sun_path, path);
        addrlen = sizeof(sockaddr) - sizeof(sockaddr.sun_path) + strlen(path);
        if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&on,sizeof(on)) < 0)
            goto err;
/*
        if (setsockopt(s, SOL_SOCKET, SO_KEEPALIVE, (char *)&on,sizeof(on)) < 0)
            goto err;
*/
        if (bind (s, (struct sockaddr *)&sockaddr, addrlen) < 0)
            goto err;

        if (listen (s, MAXCONN) < 0) {
err: 	    fprintf (stderr, "ximtool: can't open ISM socket on %s, errno=%d\n",
                path, errno);
            if (s)
                close (s);
            return (0);
	}

	return (s);
}



/* ISM_GETCHANNEL --- Get an ISM i/o channel descriptor.
 */
static IsmIoChanPtr
ism_getChannel (xim)
register XimDataPtr xim;
{
        register IsmIoChanPtr chan;
        register int i;

        for (i=0;  i < XtNumber(xim->ism_client); i++) {
            if (!xim->ism_client[i].connected) {
                xim->ism_client[i].id = (XtPointer) i;
                return (&xim->ism_client[i]);
	    }
	}

        return (NULL);
}


/*  ISMOBJECTS -- Add the named client to the list of known objects, or
 *  return zero if this is a new object.  We keep a list so we don't keep
 *  creating the same object in the OBM each time a client connects.
 */
ismObjects (name)
char	*name;
{
	static char objects[SZ_LINE] = "";

	if (strstr (objects, name) == NULL) {
	    strcat (objects, name);
	    strcat (objects, "|\0");
	    return (0);
	} else
	    return (1);
}


/* ISM_READ -- Read exactly "n" bytes from a descriptor. 
 */

static int
ism_read (fd, vptr, nbytes)
int 	fd; 
void 	*vptr; 
int 	nbytes;
{
        char    *ptr = vptr;
        int 	nread = 0, nleft = nbytes, nb = 0;

        while (nleft > 0) {
            if ( (nb = read(fd, ptr, nleft)) < 0) {
                if (errno == EINTR)
                    nb = 0;          	/* and call read() again */
                else
                    return(-1);
            } else if (nb == 0)
                break;                  /* EOF */
            nleft -= nb;
            ptr   += nb;
            nread += nb;
        }

        return (nread);              	/* return no. of bytes read */
}


/* ISM_WRITE -- Write exactly "n" bytes to a descriptor. 
 */

static int
ism_write (fd, vptr, nbytes)
int 	fd; 
void 	*vptr; 
int 	nbytes;
{
        char 	*ptr = vptr;
        int     nwritten = 0,  nleft = nbytes, nb = 0;

	/* Send the bytecount first.
        if ((nb = write (fd, &nleft, sizeof (int))) <= 0)
            return (-1);
	 */

	/* Now send the message. */
        while (nleft > 0) {
            if ( (nb = write(fd, ptr, nleft)) <= 0) {
                if (errno == EINTR)
                    nb = 0;           	/* and call write() again */
                else
                    return (-1);        /* error */
            }
            nleft    -= nb;
            ptr      += nb;
            nwritten += nb;
        }

        return (nwritten);
}
