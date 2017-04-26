#include <sys/stat.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <netdb.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

#define	SELWIDTH	32

short	host_port	= 6200;
char	*machine	= "localhost";
extern int errno;

/**
 * Console - A simple VOClient administrative console program.
 * (not yet fully implemented)
 */
main (argc, argv)
int	argc;
char	**argv;
{
	register int  i, fd;
	char obj[64], msg[2048];
	unsigned long host_addr;
        struct sockaddr_in sockaddr;
	struct hostent *hp;
	fd_set  fds, allset;


        /* Get a socket. */
        if ((fd = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
            goto err;
	}

        /* Compose network address. */
        bzero ((char *)&sockaddr, sizeof(sockaddr));
        sockaddr.sin_family = AF_INET;
        sockaddr.sin_port = htons(host_port);
	hp = gethostbyname ("localhost");
        bcopy (hp->h_addr, (char *)&sockaddr.sin_addr, sizeof(host_addr));

        /* Connect to server. */
        if (connect(fd,(struct sockaddr *)&sockaddr,sizeof(sockaddr)) < 0) {
	    fprintf (stderr, "error %d\n", errno);
            close (fd);
            goto err;
        }

	if (argc > 1) {
	    /* Get object name as first arg */
	    sprintf (obj, "%s\0", argv[1]);

	    /* Start writing cmdline args to the server.... */
	    msg[0] = '\0';
	    for (i=2; i < argc; i++) {
	        strcat (msg, argv[i]);
	        if (i < (argc-1))
	            strcat (msg, " ");
	    }
	    strcat (msg, "\0");

	    i=write(fd, msg, strlen(msg));
	    sleep (1);

	}


	/* Read the socket printing out whatever it finds. */
	int n;
	char buf[2048];

	FD_ZERO (&allset);
	FD_SET (fd, &allset);
	FD_SET (fileno(stdin), &allset);
	while (1) {
	    fds = allset;
	    bzero (buf, 2048);

	    if ((n = select (SELWIDTH, &fds, NULL, NULL, NULL)) > 0) {

		if (FD_ISSET(fd, &fds)) {
	            n = read (fd, buf, 2048);
		    if (n)
		        printf ("fd: n=%d  msg='%s'\n", n, buf);

		} else if (FD_ISSET(fileno(stdin), &fds)) {
	            n = read (fileno(stdin), buf, 2048);
		    printf ("stdin: n=%d  msg='%s'\n", n, buf);
	            n = write (fd, buf, strlen(buf));
		}

	    } else {
                fprintf (stderr, "Error: select error\007\n");
                exit (-1);
	    }
	}

        fprintf (stderr, "Closing down....\n");
	close (fd);
	exit (0);

err:
        fprintf (stderr, "Cannot open server connection on 'inet:%d:%s'.\n", 
	    host_port, machine);
	close (fd);
	exit (1);
}
