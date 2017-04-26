/**
 *  SESSION_CMD -- Session Manager test client.
 *
 *  @file       session_cmd.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Session Manager test client.
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

#include "samp.h"			/* SAMP interface	        */
#include "voApps.h"			/* voApps interface	        */


#define	SZ_BUF		1024
#define SELWIDTH	4


/*  Utility socket routines.
 */
extern int  vos_uploadFiles (int sock, char *cmd);
extern int  vos_openClientSocket (char *host, int port, int retry);
extern int  vos_sockRead (int fd, void *vptr, int nbytes);
extern int  vos_sockWrite (int fd, void *vptr, int nbytes);
extern int  vos_sockWriteHdr (int fd, int len, char *name, int type, 
				int mode, char *to);
extern int  vos_sockReadHdr (int fd, int *len, char *name, int *type, 
				int *mode);

extern int  vos_openSession (char *host, int port, char *session_name);
extern int  vos_closeSession (int sock);

extern void vos_setNonBlock (int sock);
extern struct hostent *vos_getHostByName (char *lhost);
        


/****************************************************************************
 *  Program entry point.
 */
int
main (int argc, char **argv)
{
    int    i, j, rc, debug=0, len=0, nw=0, cb_sock=0, nfiles=0;
    int    mgr_port = SESS_DEFPORT;
    char  *mgr_host = SESS_DEFHOST;
    char  *sname    = "test";
    char   buf[SZ_BUF];

    fd_set allset, fds;


    /*  Process cmdline args.
     */
    for (i=1; i < argc; i++) {
        for (j=1; j < argc; j++) {
            switch ( argv[i][j] ) {
            case 'd':   debug++;			break;

	    default:
	        fprintf (stderr, "Invalid option '%c'\n", argv[i][0]);
	        return (1);
	    }
	}
    }



    /*  Open a client connection to the VOSAMP Session Manager.
     */
    if ((cb_sock = vos_openSession (mgr_host, mgr_port, sname)) == 0) {
	fprintf (stderr, "Cannot open connection to session manager\n");
	return (1);
    }

    /*  Take commands from the stdin, write results to stdout.
     */
    printf ("vosession> "); fflush (stdout);

    FD_ZERO (&allset);
    FD_SET (cb_sock, &allset); 		vos_setNonBlock (cb_sock);
    FD_SET (fileno(stdin), &allset); 	vos_setNonBlock (fileno(stdin));


    /*  Process input either from the stdin or the session manager.
     */
    while (1) {
        /*  Initialize the input file descriptor set.
         */
        memcpy (&fds, &allset, sizeof(allset));
        rc = select (SELWIDTH, &fds, NULL, NULL, NULL);

        for (i=0; i < SELWIDTH+1; i++) {
            if (FD_ISSET(i, &fds)) {
        	if (i == fileno(stdin)) {
    		    if (fgets (buf, SZ_BUF, stdin) == NULL)
			goto done_; 		 /*  EOF, time to quit 	*/
		    buf[strlen(buf)-1] = '\0';	 /*  kill newline 	*/ 

                } else {
		    int  nread, nbytes, type, mode;
		    char fname[SZ_FNAME];

        	    if (vos_sockReadHdr (i, &nbytes, fname, &type, &mode))
        	        nread = vos_sockRead (i, buf, nbytes);
		    else
    			goto done_;

		    switch (type) {
		    case SAMP_QUIT:
			goto done_;
		    case SAMP_RESULT:
			printf ("\r\n%s\n", buf);
			goto next_cmd;
		    case SAMP_CMD:
			printf ("Got cmd: '%s'\n", buf);
			goto next_cmd;
		    case SAMP_TEST:
			printf ("Got test cmd: '%s'\n", buf);
			goto next_cmd;
		    case SAMP_RELAY:
			printf ("Got relayed cmd: '%s'\n", buf);
			goto next_cmd;
		    default:
			printf ("Got msg type: %d\n", type);
			goto next_cmd;
		    }
		}
	    }
        }

	/*  If we want to quit, let the vos_closeSession() send the 'quit'.
	 */
	if (strncasecmp (buf, "quit", 4) == 0)
	    break;

	/*  Upload any local files to the session manager.
	 */
        nfiles = vos_uploadFiles (cb_sock, buf);
        if (debug)
            if (nfiles) fprintf (stderr, "sent %d data files....\n", nfiles);

        /*  Send the command string.
         */
	len = strlen (buf);
        if (vos_sockWriteHdr(cb_sock, len, NULL, SAMP_CMD, SAMP_NOTIFY, "smgr"))
	    nw = vos_sockWrite (cb_sock, buf, len);

next_cmd:
        memset (buf, 0, SZ_BUF);
        printf ("vosession> "); fflush (stdout);
    }

done_:
    vos_closeSession (cb_sock);
    return (0);
}
