#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>

#define	PORT	5138

/*
 * Dummy client to test xtapemon server.
 */

main()
{
	struct sockaddr_in sockaddr;
	struct hostent *hp;
	unsigned short portaddr;
	static	char msg[] = "hello, world!\\n\n";
	int	connected;
	int	port, s, i;

	if ((hp = gethostbyname ("lepus")) == NULL)
	    fprintf (stderr, "host lookup fails\n");
	if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0)
	    fprintf (stderr, "cannot open socket\n");
	fcntl (s, F_SETFD, O_RDWR|O_NDELAY);

	for (connected=i=0;  i < 2;  i++) {
	    port = PORT + i;
	    portaddr = htons((short)port);
	    memset ((char *)&sockaddr, 0, sizeof(sockaddr));
	    memmove ((char *)&sockaddr.sin_addr, (char *)hp->h_addr,
		hp->h_length);
	    sockaddr.sin_family = AF_INET;
	    sockaddr.sin_port = portaddr;

	    if (connect (s,(struct sockaddr *)&sockaddr,sizeof(sockaddr))>=0) {
		fprintf (stderr, "connected on port %d\n", port);
		connected++;
		break;
	    }
	}

	if (!connected)
	    fprintf (stderr, "cannot connect socket\n");

	write (s, msg, strlen(msg));
	sleep (5);
	for (i=1;  i <= 10;  i++) {
	    sprintf (msg, "message %d\\n\nfile = %d\n", i, i);
	    write (s, msg, strlen(msg));
	    sleep (3);
	}

	for (i=1;  i <= 500;  i++) {
	    sprintf (msg, "record = %d\n", i);
	    write (s, msg, strlen(msg));
	}

	close (s);
}
