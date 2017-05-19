/**
 *  VOSUTIL - Utility routines for the VOSAMP tools.
 *
 *  @file       vosUtil.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/11
 *
 *  @brief      Utility routines for the VOSAMP tools.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <time.h>

#include <netdb.h>
#include <sys/errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <curl/curl.h>
#include <curl/easy.h>


#include "samp.h"				/* LIBSAMP interface	    */
#include "voApps.h"				/* voApps interface	    */


#define	SZ_MTYPE	64
#define	SZ_BUF		128
#define	SZ_HOSTIP	16
#define	SZ_APPNAME	16

#define VOS_DEBUG  (getenv("VOS_DBG")!=NULL||access("/tmp/VOS_DBG",F_OK)==0)


typedef struct {
    int   nbytes;				/* length of msg body	    */
    int   mode;					/* sync, async, notify	    */
    int   type;					/* SAMP cmd, data, result   */
    char  fname[SZ_FNAME];			/* upload data name    	    */
    int   fsize;				/* data file size   	    */
    char  session[SZ_APPNAME];			/* session name	    	    */
    int   sessionPort;				/* session port number	    */
    char  recipient[SZ_APPNAME];		/* recipient app name	    */
    char  senderIP[SZ_HOSTIP];			/* sender's IP address	    */
    int   senderPort;				/* sender's port	    */

    int   verbose;				/* verbose flag 	    */
    int   debug;				/* debug flag 		    */
} cmdHdr, *cmdHdrP;

cmdHdr  msgHdr;


extern char     *to, senderIP[SZ_HOSTIP];


char *vos_toURL (char *arg);
char *vos_optArg (char *arg);
char *vos_getLocalIP (void);
char *vos_getFName (char *path);
char *vos_typeName (int type);

int  *vos_toIntArray (char *arg, int *nrows);
int   vos_urlType (char *url);
int   vos_getURL (char *url, char *fname);



/*  Private socket routines.
 */
int   vos_openServerSocket (int port);
int   vos_openClientSocket (char *host, int port, int retry);
int   vos_sockRead (int fd, void *vptr, int nbytes);
int   vos_sockWrite (int fd, void *vptr, int nbytes);
void  vos_setNonBlock (int sock);
int   vos_sockWriteHdr (int fd, int len, char *name, int type, int mode, 
			char *to);
int   vos_sockReadHdr (int fd, int *len, char *name, int *type, int *mode);
void  vos_sockPrintHdr (char *msg, int fd);
struct hostent *vos_getHostByName (char *lhost);
struct hostent *vos_dupHostent (struct hostent *hentry);




/****************************************************************************
 */


/**
 *  VOS_URLTYPE -- Determine the type of a URL parameter
 */
int
vos_urlType (char *url)
{
    if (strncasecmp (url, "http://127.0.0.1", 16) == 0)
	return (VOS_LOCALURL);
    else if (strncasecmp (url, "file://", 7) == 0)
	return (VOS_LOCALURI);
    else if (strncasecmp (url, "http://", 7) == 0)
	return (VOS_REMOTE);
    else if (access (url, F_OK) == 0)
	return (VOS_LOCALFILE);

    return (-1);
}


/**
 *  VOS_GETFNAME -- Get a filename from a path or URL.
 */
char *
vos_getFName (char *path)
{
    static char fname[SZ_FNAME];
    static int  filenum = 0;

    memset (fname, 0, SZ_FNAME);
    if (access (path, R_OK) == 0) {
        int i, len = strlen (path);

        for (i=len-1; i >=0 && path[i] != '/'; i--) ;  /* get filename */
        strcpy (fname, &path[i+1]);
    }

    if (!fname[0])
	sprintf (fname, "vos%d_%03d", (int)getpid(), filenum++);

    return (fname);
}


/**
 *  VOS_TYPENAME -- Convert a message type code to a string.
 */
char *
vos_typeName (int type)
{
    switch (type) {
    case SAMP_DATA:     return ("SAMP_DATA");
    case SAMP_CMD:      return ("SAMP_CMD");
    case SAMP_RELAY:    return ("SAMP_RELAY");
    case SAMP_TEST:     return ("SAMP_TEST");
    case SAMP_QUIT:     return ("SAMP_QUIT");
    default:            return ("unknown");
    }
}


/** 
 *  VOS_GETURL -- Utility routine to do a simple URL download to the file.
 */
int 
vos_getURL (char *url, char *fname)
{
    int  stat = 0;
    char errBuf[SZ_LINE];
    FILE *fd;
    CURL *curl_handle;

	
    if (access (fname, F_OK) == 0)	/* see if file already exists	*/
	unlink (fname);


    /*  For the CURL operation to download the file.
     */
    curl_global_init (CURL_GLOBAL_ALL);     	/* init curl session	*/
    curl_handle = curl_easy_init ();

    /*  Open the output file.
     */
    if ((fd = fopen (fname, "wb")) == NULL) { 	
	fprintf (stderr, "Error: cannot open output file '%s'\n", fname);
        curl_easy_cleanup (curl_handle);
        return -1;
    }

    /*  Set cURL options
     */
    curl_easy_setopt (curl_handle, CURLOPT_URL, url);
    curl_easy_setopt (curl_handle, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt (curl_handle, CURLOPT_WRITEDATA, fd);
    curl_easy_setopt (curl_handle, CURLOPT_ERRORBUFFER, errBuf);
    curl_easy_setopt (curl_handle, CURLOPT_FOLLOWLOCATION, 1);
    curl_easy_setopt (curl_handle, CURLOPT_FAILONERROR, 1);

    /*  Do the download.
     */
    if ((stat = curl_easy_perform (curl_handle)) != 0) {
	/*  Error in download, clean up.
	 */
	fprintf (stderr, "Error: can't download '%s' : %s\n", url, errBuf);
	unlink (fname);
        fclose (fd); 			    	/* close the file 	*/
        curl_easy_cleanup (curl_handle);    	/* cleanup curl stuff 	*/
	return (-1);
    }

    fflush (fd);
    fclose (fd); 			    	/* close the file 	*/
    curl_easy_cleanup (curl_handle); 	    	/* cleanup curl stuff 	*/

    return (1);
}


/**
 *  VOS_OPTARG -- Input command arguments are allowed to be of the form
 *  'param=value', but the SAMP interface only wants the value string. 
 *  Skip past the '=' and return the value, or just return the value if
 *  there is no parameter name.
 */
char *
vos_optArg (char *arg)
{
    char *ip, first = (arg ? *arg : 0);

    if (!arg || !first) return ("");
    return ( ((ip = strchr (arg, (int) '=')) ? ++ip : arg) );
}


/**
 *  VOS_TOURL -- Convert the argument to a URL suitable for a message.
 */
char *
vos_toURL (char *arg)
{
    /*  If we have an existing protocol simply return the argument.
     */
    if ((strncmp (arg, "http:", 5) == 0) ||
        (strncmp (arg, "file:", 5) == 0) ||
        (strncmp (arg, "ftp:", 4) == 0))
    	    return (arg);

    if (access (arg, F_OK) == 0) {
	static char buf[SZ_FNAME];

 	memset (buf, 0, SZ_FNAME);
	if (arg[0] != '/') {
	    char  cwd[SZ_FNAME];

 	    memset (cwd, 0, SZ_FNAME);
	    getcwd (cwd, (unsigned long) SZ_FNAME);
	    sprintf (buf, "file://%s/%s", cwd, arg);
	} else
	    sprintf (buf, "file://%s", arg);

	return (buf);
    }

    return (arg);
}


/**
 *  VOS_TOINTARRAY -- Convert a range string to an unpacked array of ints.
 */
#define MAX_RANGES	 256
#define SZ_ROW		  16

int *
vos_toIntArray (char *arg, int *nrows)
{
    int  i, val, nvalues;
    static int  ranges[MAX_RANGES], values[MAX_ROWS];
    extern int  vot_decodeRanges(), get_next_number();


    memset (values, 0, (sizeof(int) * MAX_ROWS));
    memset (ranges, 0, (sizeof(int) * MAX_RANGES));

    if (vot_decodeRanges (arg, ranges, MAX_RANGES, &nvalues) < 0)
        fprintf (stderr, "Error decoding range string.\n");

    for (i=0, val=0; (val = get_next_number (ranges, val)) > 0; i++ )
	values[i] = val;

    *nrows = nvalues;
    return (values);
}



/*****************************************************************************
****  Socket Utilities
*****************************************************************************/


#define	SELWIDTH	32
#define	SOCK_MAX_TRY	 3

/** 
 *  VOS_OPENSERVERSOCKET -- Open a socket to be used on the 'server' side.
 *
 *  @brief  Open a socket to be used on the 'server' side
 *  @fn     int vos_openServerSocket (int port)
 *
 *  @param  port	port number to open
 *  @return		socket descriptor
 *
 */
int
vos_openServerSocket (int port)
{
    struct  sockaddr_in servaddr; 	/* server address 		*/
    int     ps = 0;                    	/* parallel socket descriptor	*/
    int32_t yes = 1, ntries = 5;


    /* Create a socket.
    */
    if ((ps = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	fprintf (stderr, "openServerSocket(%d): %s\n", port, strerror(errno));
	return (-1);
    }
    setsockopt (ps, SOL_SOCKET, SO_REUSEADDR, (void *)&yes,   sizeof(yes));

    if (VOS_DEBUG)
	fprintf (stderr, "server socket %d on %s:%d\n", ps, 
	    vos_getLocalIP(), port );


    /* Set server address.
    */
    memset (&servaddr, 0, sizeof servaddr);
    servaddr.sin_family      = AF_INET;
    servaddr.sin_port        = htons(port);
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);

    /* Bind to the server socket and listen for a connection.
    */
    while (ntries--) {
        if (bind (ps, (struct sockaddr*)&servaddr, sizeof servaddr) < 0) {
	    if (!ntries)
	        return (-1);
        } else {
            if ((listen (ps, SOMAXCONN)) < 0) {
                fprintf (stderr, 
		    "serverSock: listen(%d:%s)", port, strerror(errno));
	        if (!ntries)
	            return (-1);
            } else { 
	        break;
            }
	}
	sleep (1);
    }

    return (ps);
}


/** 
 *  VOS_OPENCLIENTSOCKET -- Open a socket to be used on the 'client' side.
 *
 *  @brief  Open a socket to be used on the 'client' side
 *  @fn     int vos_openClientSocket (char *host, int port, int retry)
 *
 *  @param  host	host name
 *  @param  port	port number to open
 *  @param  retry	attempt to reconnect?
 *  @return		socket descriptor
 *
 */
int
vos_openClientSocket (char *host, int port, int retry)
{
    char    *ip, lhost[SZ_LINE];
    struct  sockaddr_in servaddr; 	/* server address 		*/
    int     ps = 0;                    	/* parallel socket descriptor	*/
    socklen_t ctry = 0, yes = 1;


    /* Remove any server port information from the host specification.
    */
    memset (lhost, 0, SZ_LINE);
    strcpy (lhost, host);
    if ( (ip = strchr (lhost, (int) ':')) )
	*ip = '\0';

    /* Create a socket.
    */
    if ((ps = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	fprintf (stderr, "openClientSocket(%s:%d): %s\n", 
	    host, port, strerror(errno));
	return (-1);
    }
    setsockopt (ps, SOL_SOCKET, SO_REUSEADDR, (void *)&yes,   sizeof(yes));


    /* Set server address.
    */
    memset (&servaddr, 0, sizeof (struct sockaddr_in));
    servaddr.sin_family      = AF_INET;
    servaddr.sin_port        = htons(port);

    /* Connect to server.
    */
    for (ctry = (retry ? 0 : SOCK_MAX_TRY); ctry <= SOCK_MAX_TRY; ctry++) {

        if ( !inet_aton(lhost, &servaddr.sin_addr) ) {
	    struct hostent *he = vos_getHostByName ( lhost );

            if (!he) {
                fprintf (stderr, "Cannot resolve address.\n");
                exit (2);
            }
            if (he->h_addrtype != AF_INET || 
	        he->h_length != sizeof (servaddr.sin_addr)) {
                    fprintf (stderr, "Cannot handle addr type %d, length %d.\n",
                        he->h_addrtype, he->h_length);
                    exit(2);
            }
            memcpy (&servaddr.sin_addr, he->h_addr_list[0], 
	        sizeof (servaddr.sin_addr) );
        }

        if (connect (ps, (struct sockaddr *)&servaddr, sizeof servaddr) < 0) {
	    if (!retry || ctry == SOCK_MAX_TRY) {
	        fprintf (stderr, 
		    "client connect() failed to %s:%d, try %d, retry %d\n", 
		    lhost, port, ctry, retry);
	        close (ps);
		ps = -1;
		break;
	    } else
	        sleep (1);
        } else
	    break;
    }

    return (ps);
}


/** 
 *  VOS_TESTCLIENTSOCKET -- Test a socket to be used on the 'client' side.
 *
 *  @brief  Test a socket to be used on the 'client' side
 *  @fn     int vos_testClientSocket (char *host, int port)
 *
 *  @param  host	host name
 *  @param  port	port number to open
 *  @return		socket descriptor or -1 if connect fails
 *
 */
int
vos_testClientSocket (char *host, int port)
{
    char    *ip, lhost[SZ_LINE];
    struct  sockaddr_in servaddr; 	/* server address 		*/
    int     sock = 0;                  	/* socket descriptor		*/
    socklen_t yes = 1;


    /* Remove any server port information from the host specification.
    */
    memset (lhost, 0, SZ_LINE);
    strcpy (lhost, host);
    if ( (ip = strchr (lhost, (int) ':')) )
	*ip = '\0';

    /* Create a socket.
    */
    if ((sock = socket (AF_INET, SOCK_STREAM, 0)) < 0)
	return (-1);
    setsockopt (sock, SOL_SOCKET, SO_REUSEADDR, (void *)&yes,   sizeof(yes));


    /* Set server address.
    */
    memset (&servaddr, 0, sizeof (struct sockaddr_in));
    servaddr.sin_family      = AF_INET;
    servaddr.sin_port        = htons(port);

    /* Connect to server.
    */
    if ( !inet_aton(lhost, &servaddr.sin_addr) ) {
	struct hostent *he = vos_getHostByName ( lhost );

        if (!he)
            return (-1);
        if (he->h_addrtype != AF_INET || 
	    he->h_length != sizeof (servaddr.sin_addr)) {
                return (-1);
        }
        memcpy (&servaddr.sin_addr, he->h_addr_list[0], 
	        sizeof (servaddr.sin_addr) );
    }

    if (connect (sock, (struct sockaddr *)&servaddr, sizeof servaddr) < 0) {
	close (sock);
	sock = -1;
    }
    return (sock);
}


/**
 *  VOS_SOCKWRITEHDR -- Write the socket message header.
 */
int
vos_sockReadHdr (int fd, int *len, char *name, int *type, int *mode)
{
    int   nread = 0;

    memset (&msgHdr, 0, sizeof (msgHdr));
    if ((nread = vos_sockRead (fd, &msgHdr, sizeof (msgHdr))) > 0) {
        *len  = msgHdr.nbytes;
        *type = msgHdr.type;
        *mode = msgHdr.mode;
        to = strdup (msgHdr.recipient);
        strcpy (senderIP, msgHdr.senderIP);
        if (name)
            strcpy (name, msgHdr.fname);    /* must be at least SZ_FNAME */
    }

    if (VOS_DEBUG)
	vos_sockPrintHdr ("Read Hdr", fd);

    return (nread);
}


/**
 *  VOS_SOCKWRITEHDR -- Write the socket message header.
 */
int
vos_sockWriteHdr (int fd, int len, char *name, int type, int mode, char *to)
{
    int  nwrite = 0;

    memset (&msgHdr, 0, sizeof (msgHdr));
    msgHdr.nbytes = len;
    msgHdr.type = type;
    msgHdr.mode = mode;
    if (name && name[0]) {
	struct stat st;
	stat (name, &st);
        strcpy (msgHdr.fname, name);
        msgHdr.fsize = st.st_size;
    }
    strcpy (msgHdr.recipient, (to ? to : "all"));
    strcpy (msgHdr.senderIP, vos_getLocalIP());

    if (VOS_DEBUG)
	vos_sockPrintHdr ("Write Hdr", fd);

    return ((nwrite = vos_sockWrite (fd, &msgHdr, sizeof(msgHdr))));
}


/**
 *  VOS_SOCKPRINTHDR -- Debug utility to print a message header.
 */
void
vos_sockPrintHdr (char *msg, int fd)
{
  fprintf (stderr, "%s:  fd=%d\n", msg, fd);
  fprintf (stderr, "  { 'type' : %d,  'mode' : %d,\n", 
	msgHdr.type, msgHdr.mode);
  fprintf (stderr, "    'fname' : '%s', 'nbytes' : %d, 'fsize' : %d,\n", 
	msgHdr.fname, msgHdr.nbytes, msgHdr.fsize);
  fprintf (stderr, "    'session' : '%s',  'port' : %d,\n",  
	msgHdr.session, msgHdr.sessionPort);
  fprintf (stderr, "    'sender' : '%s',  'port' : %d,  'to' : '%s'\n",  
	msgHdr.senderIP, msgHdr.senderPort, msgHdr.recipient);
  fprintf (stderr, "  }\n");
}


/**
 *  VOS_SOCKREAD -- Read exactly "n" bytes from a socket descriptor. 
 *
 *  @brief  Recv exactly "n" bytes from a socket descriptor. 
 *  @fn     int vos_sockRead (int fd, void *vptr, int nbytes)
 *
 *  @param  fd          file descriptor
 *  @param  vptr        data buffer to be written
 *  @param  nbytes      number of bytes to write
 *  @return             number of bytes written
 */
int
vos_sockRead (int fd, void *vptr, int nbytes)
{
    char    *ptr = vptr;
    int     nread = 0, nleft = nbytes, nb = 0;
    fd_set  allset, fds;


    /*  Set non-blocking mode on the descriptor.
     */
    vos_setNonBlock (fd);

    FD_ZERO (&allset);
    FD_SET (fd, &allset);

    while (nleft > 0) {
      fds = allset;
      if (select (SELWIDTH, &fds, NULL, NULL, NULL)) {
        if ( (nb = recv (fd, ptr, nleft, 0)) < 0) {
            if (errno == EINTR || errno == EAGAIN)
                nb = 0;             /* and call recv() again */
            else {
		fprintf (stderr, "[%d] vos_sockRead:[%d] %s\n", 
		    (int) getpid(), fd, strerror(errno));
                return (-1);
	    }
        } else if (nb == 0)
            break;                  /* EOF */
        nleft -= nb;
        ptr   += nb;
        nread += nb;
      }
    }

    if (VOS_DEBUG)
	fprintf (stderr, "sockRead:  %d bytes on fd=%d\n", nread, fd);

    return (nread);                 /* return no. of bytes read */
}


/**
 *  VOS_SOCKWRITE -- Write exactly "n" bytes to a socket descriptor. 
 *
 *  @brief  Send exactly "n" bytes to a socket descriptor. 
 *  @fn     int vos_sockWrite (int fd, void *vptr, int nbytes)
 *
 *  @param  fd          file descriptor
 *  @param  vptr        data buffer to be written
 *  @param  nbytes      number of bytes to write
 *  @return             number of bytes written
 */
int
vos_sockWrite (int fd, void *vptr, int nbytes)
{
    char    *ptr = vptr;
    int     nwritten = 0,  nleft = nbytes, nb = 0;
    fd_set  allset, fds;


    /*  Set non-blocking mode on the descriptor.
     */
    vos_setNonBlock (fd);

    FD_ZERO (&allset);
    FD_SET (fd, &allset);

    while (nleft > 0) {
      fds = allset;
      if (select (SELWIDTH, NULL, &fds, NULL, NULL)) {
        if ( (nb = send (fd, ptr, nleft, 0)) <= 0) {
            if (errno == EINTR || errno == EAGAIN)
                nb = 0;             /* and call send() again */
            else {
		/*
		fprintf (stderr, "[%d] vos_sockWrite[%d]: %s\n", 
		    (int) getpid(), fd, strerror (errno));
		*/
                return (-1);
	    }
        }
        nleft    -= nb;
        ptr      += nb;
        nwritten += nb;
      }
    }

    if (VOS_DEBUG)
	fprintf (stderr, "sockWrite:  %d bytes on fd=%d\n", nwritten, fd);

    return (nwritten);
}


/**
 *  VOS_FILEREAD -- Read exactly "n" bytes from a socket descriptor. 
 *
 *  @brief  Recv exactly "n" bytes from a socket descriptor. 
 *  @fn     int vos_sockRead (int fd, void *vptr, int nbytes)
 *
 *  @param  fd          file descriptor
 *  @param  vptr        data buffer to be written
 *  @param  nbytes      number of bytes to write
 *  @return             number of bytes written
 */
int
vos_fileRead (int fd, void *vptr, int nbytes)
{
    char    *ptr = vptr;
    int     nread = 0, nleft = nbytes, nb = 0;

    while (nleft > 0) {
        if ( (nb = read(fd, ptr, nleft)) < 0) {
            if (errno == EINTR)
                nb = 0;             /* and call read() again */
            else
                return(-1);
        } else if (nb == 0)
            break;                  /* EOF */
        nleft -= nb;
        ptr   += nb;
        nread += nb;
    }
    return (nread);                 /* return no. of bytes read */
}


/**
 *  VOS_FILEWRITE -- Write exactly "n" bytes to a file descriptor. 
 *
 *  @brief  Send exactly "n" bytes to a file descriptor. 
 *  @fn     int vos_fileWrite (int fd, void *vptr, int nbytes)
 *
 *  @param  fd          file descriptor
 *  @param  vptr        data buffer to be written
 *  @param  nbytes      number of bytes to write
 *  @return             number of bytes written
 */
int
vos_fileWrite (int fd, void *vptr, int nbytes)
{
    char    *ptr = vptr;
    int     nwritten = 0,  nleft = nbytes, nb = 0;

    while (nleft > 0) {
        if ( (nb = write(fd, ptr, nleft)) <= 0) {
            if (errno == EINTR)
                nb = 0;             /* and call write() again */
            else
                return(-1);         /* error */
        }
        nleft    -= nb;
        ptr      += nb;
        nwritten += nb;
    }
    return (nwritten);
}


/**
 *  VOS_SETNONBLOCK -- Set a non-blocking mode on the socket descriptor.
 */
void
vos_setNonBlock (int sock)
{
    int flags;


    /* Set socket to non-blocking.
    */
    if ((flags = fcntl (sock, F_GETFL, 0)) < 0) {
        /* Handle error */
        return;
    }
    if (fcntl (sock, F_SETFL, flags | O_NONBLOCK) < 0) {
        /* Handle error */
        return;
    }
}


/**
 *  VOS_GETLOCALIP -- Get the IP address of the local machine.
 */
char *
vos_getLocalIP (void)
{
    const char *kGoogleDnsIp = "8.8.8.8";  /* Google's public DNS server */
    unsigned short kDnsPort  = 53;
    struct sockaddr_in serv;
    int    sock, err;
    const  char *p;
    char   buffer[SZ_BUF];
    static int initialized = 0;
    static char localIP[SZ_BUF];


    /*  Only get the IP once, afterwards just return the value.
     */
    if (initialized++)
	return (localIP);

    memset (buffer, 0, SZ_BUF);
    memset (localIP, 0, SZ_BUF);

    if ((sock = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
        return ("127.0.0.1");   /* cannot get socket, punt     */

    memset (&serv, 0, sizeof (serv));
    serv.sin_family = AF_INET;
    serv.sin_addr.s_addr = inet_addr(kGoogleDnsIp);
    serv.sin_port = htons(kDnsPort);

    if ((err = connect(sock, (struct sockaddr*)&serv, sizeof(serv))) >= 0) {
        struct sockaddr_in name;
        socklen_t namelen = sizeof(name);

        if ((err=getsockname(sock, (struct sockaddr*)&name, &namelen)) < 0)
            strcpy (buffer, "127.0.0.1");       /* cannot connect socket  */
        else
            if ((p=inet_ntop(AF_INET, &name.sin_addr, buffer, SZ_BUF)) == NULL)
                strcpy (buffer, "127.0.0.1");   /* cannot get IP buffer name */
    } else
        strcpy (buffer, "127.0.0.1");   /* cannot get IP buffer name */

    if (strncmp (buffer, "192.168.", 8) == 0)
        strcpy (buffer, "127.0.0.1");   /* cannot get IP buffer name */

    close(sock);

    strcpy (localIP, buffer);
    return (localIP);
}


/**
 *  VOS_GETHOSTBYNAME -- Get the host entry associated with a (cached) name.
 *
 *  @fn   char *vos_getHostByName (char *name)
 *
 *  @param  name 	host name
 *  @return 		host entry structure pointer
 */
typedef struct {
    char   name[SZ_LINE];
    char   ip[SZ_LINE];
    struct hostent *host;
}  hostTab, *hostTabP;

struct hostent_wrapper {
    struct hostent hentry;
    int    status;
};

static hostTab hostab[MAX_CLIENTS];


struct hostent *
vos_getHostByName (char *name)
{
    static int initialized = 0;
    struct in_addr x_addr;
    struct hostent *hp = (struct hostent *) NULL;
    hostTab  *h = (hostTabP) hostab;
    int    i, len;


    if (!initialized) {
	memset (hostab, 0, sizeof (hostab));
	initialized++;
    }

    for (i=0; i < MAX_CLIENTS; i++, h++) {
	if (h && h->name[0]) {
	    len = min (strlen (name), strlen (h->name));
	    if (strncmp (name, h->name, len) == 0)
		return (h->host);
	} else 
	    break;				/* end of cache list */
    }

    /*  If we overflow the cache use a DNS lookup.
     */
    if (i >= MAX_CLIENTS) {
	fprintf (stderr, "vos_getHostByName(): cache overflow on '%s'", name);
        hp = gethostbyname (name);
        return (hp);
    }

    /*  Host not found, resolve and add it to the cache.
     */
    hp = gethostbyname (name);
    if (hp == (struct hostent *) NULL) {
	fprintf (stderr, "vos_getHostByName: cannot resolve '%s'\n", name);
	exit (0);
    }

    strcpy (h->name, name);
    x_addr.s_addr = *((unsigned long *) hp->h_addr_list[0]);
    strcpy (h->ip,   inet_ntoa (x_addr));

    h->host = vos_dupHostent (hp); 
    return (h->host);
}


/**
 *   VOS_DUPHOSTENT -- Duplicate a hostent structure via a deep copy.
 */
struct hostent *
vos_dupHostent (struct hostent *hentry)
{
    struct hostent_wrapper *oldhw = NULL;
    struct hostent_wrapper *newhw = NULL;
    int i = 0;
    int aliascount=0;
    int addrcount=0;

	
    if (!hentry)
    	return NULL;

    oldhw = (struct hostent_wrapper *) hentry;
    newhw = (struct hostent_wrapper *) malloc (sizeof (struct hostent_wrapper));
    bzero(newhw, sizeof (struct hostent_wrapper));

    newhw->hentry.h_addrtype = hentry->h_addrtype;
    newhw->hentry.h_length = hentry->h_length;
    newhw->status = oldhw->status;

    if (hentry->h_name)
    	newhw->hentry.h_name = strdup(hentry->h_name);

    if (hentry->h_aliases) {
    	for (i=0; hentry->h_aliases[i] != 0; i++)
    	    aliascount++;
    	aliascount++;

	newhw->hentry.h_aliases = (char **)malloc (aliascount * sizeof (char*));
	bzero(newhw->hentry.h_aliases, aliascount * sizeof(char*));

	for (i=0; hentry->h_aliases[i] != 0; i++) {
	    if (hentry->h_aliases[i])
		newhw->hentry.h_aliases[i] = strdup (hentry->h_aliases[i]);
	}
    }
	
    if (hentry->h_addr_list) {
	for (i=0; hentry->h_addr_list[i] != 0; i++)
	    addrcount++;
	addrcount++;
		
	newhw->hentry.h_addr_list = 
	    (char **) malloc (addrcount * sizeof (char *));
	bzero (newhw->hentry.h_addr_list, addrcount * sizeof (char *));
		
	for (i=0; hentry->h_addr_list[i] != 0; i++) {
	    if (hentry->h_addr_list[i])
		newhw->hentry.h_addr_list[i] = strdup (hentry->h_addr_list[i]);
	}
    }

    return (struct hostent *) newhw;
}

/**
 *  VOS_STRSUB -- Do a string subsitution.
 */
int
vos_strsub (char *in, char *from, char *to, char *outstr, int maxch)
{
    int   flen = strlen (from);
    int   nsub = 0;
    char  *ip, *op;

    if (!from || !to)
        return (0);

    for (ip=in, op=outstr; *ip; ip++) {
        if (! *ip || (ip - in) > maxch)
            break;
        if (*ip == '$') {
            /* Start of a macro.
             */
            if (strncasecmp (ip, from, flen) == 0) {
                /* Our macro, do the substitution.
                 */
                char *tp = to;

                ip += flen - 1;         /* skip the input macro         */
                while (*tp)             /* copy replacement string      */
                    *op++ = *tp++;
                nsub++;
            } else {
                /* Not our macro, just pass it through.
                 */
                *op++ = *ip;
            }
        } else {
            *op++ = *ip;
        }
    }
    *op = '\0';

    return (nsub);
}
