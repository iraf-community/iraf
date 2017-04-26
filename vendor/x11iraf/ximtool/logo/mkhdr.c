#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#define	NBYTES		262144		/* 512 x 512 bytes */

unsigned char buf[NBYTES];

main (int argc, char **argv)
{
    int i, fd, sz;
    
    fd = open (argv[1], O_RDONLY, 0644);
    sz = read (fd, buf, NBYTES);
    close (fd);

    printf ("#define LOGO_XDIM\t\t512\n");
    printf ("#define LOGO_YDIM\t\t512\n");
    printf ("#define LOGO_NCOLORS\t\t200\n");
    printf ("#define LOGO_NBYTES\t\t262144\n");
    printf ("\n");
    printf ("unsigned char logo_data[] = {");
    for (i=0; i < NBYTES; i++) {
	if ((i % 15) == 0)
	    printf ("\n  ");
	printf ("0x%02x%s", (unsigned char)buf[i],(i==(NBYTES-1) ? "\n" : ","));
    }
    printf ("};\n\n");
}


