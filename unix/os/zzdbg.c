/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#define import_spp
#define import_kernel
#define import_knames
#define import_xnames
#define import_prtype
#include <iraf.h>



void zzval_ (XINT *val) 
{ 
    fprintf (stderr, "zzprnt: %ld 0x%lx\n", (long)*val, (long)*val); 
}


void zzprnt_ (XINT *val, XINT *len) 
{   
    int  i;


    fprintf (stderr, "zzprnt:\n");
    for (i=0; i < *len; i++)
        fprintf (stderr, "%ld\n", (long)*val); 
    fprintf (stderr, "\n");
}


void zzmsg_ (XCHAR *buf, XINT *val) 
{ 
    char i, *cp = (char *) buf;

    fprintf (stderr, "zzmsg: ");
    for (i=0; i < 64; i++, cp++) {
	if (*cp == '\n' || (*cp == '\0' && *(cp+1) == '\0'))
	    break;
	fprintf (stderr, "%c", *cp);
    }
    fprintf (stderr, "   %ld 0x%lx\n", (long)*val, (long)*val); 
    fflush (stderr);
}


void zzmfd_ (XCHAR *buf, XINT *fd, XINT *val) 
{ 
    char i, *cp = (char *) buf;

    fprintf (stderr, "zzmfd[%2d]: ", (int) *fd);
    for (i=0; i < 64; i++) {
	if (*cp == '\n' || (*cp == '\0' && *(cp+1) == '\0'))
	    break;
	fprintf (stderr, "%c", *cp++);
    }
    fprintf (stderr, "   %ld 0x%lx\n", (long)*val, (long)*val); 
    fflush (stderr);
}


void zzmstr_ (XCHAR *s1, XCHAR *s2, XINT *val) {
 
    char i, *c1 = (char *) s1, *c2 = (char *) s2;

    fprintf (stderr, "zzmstr: ");
    for (i=0; i < 64; i++) {
	if (*c1 == '\n' || (*c1 == '\0' && *(c1+1) == '\0'))
	    break;
	fprintf (stderr, "%c", *c1++);
    }
    fprintf (stderr, "   ");
    for (i=0; i < 64; i++) {
	if (*c2 == '\n' || (*c2 == '\0' && *(c2+1) == '\0'))
	    break;
	fprintf (stderr, "%c", *c2++);
    }
    fprintf (stderr, "   %ld 0x%lx\n", (long)*val, (long)*val); 
    fflush (stderr);
}


void zzdmp_ (XCHAR *buf, XINT *len) { 
    int i; 
    char *cp = (char *) buf;
    for (i=0; i < *len; i++, cp++) {
	if (*cp == '\0' && *(cp+1) == '\0')
	    break;
	else
            fprintf (stderr, "%c'", *cp);
    }
    fprintf (stderr, "\n");
    fflush (stderr);
}


void mdump_ (XINT *buf, XINT *nbytes)
{
    register int i=0, j=0, nb=(*nbytes);
    char ch, *p = LOC_TO_ADDR(*buf,char);

    /*
    fprintf (stderr, "*buf = %d  %d %d   %d %d\n", 
      *buf, ((int)p)*2, (((int)p)*4-4), ((int)(*buf))*2, (((int)(*buf))*4-4) );
    p = ((*buf) * 4 - 4);
    */

    printf ("\n");
    while ( i < nb ) {
        printf ("%4d  %ld 0x%lx\t", i, (long)(p+i), (long)(p+i) );
        for (j=0; j < 8; j++) {
            ch = *(p+i);
            printf ("0x%02x ", (ch & 0xff));
            i++;
        }
        printf ("\n");
    }
    printf ("\n");
}


void
zzpeek_ (void *a, XINT *nelems, XINT *nl)
{
    XINT  i;
    char *c;

    for (i=0, c=(char *)a; i < *nelems; i++)
        printf ("%2d ", *c++);
    printf ("%s", (*nl ? " : " : "\n"));
}


void
zzpdat_ (XCHAR *msg, void *a, XINT *nelems, XINT *nl)
{
    XINT   i;
    char  *c;

    for (i=0; i < 32; i++) {
        if (msg[i])
            printf ("%c", (char )msg[i]);
        else {
            printf (": ");
            break;
        }
    }

    for (i=0, c=(char *)a; i < *nelems; i++)
        printf ("%2d ", *c++);
    printf ("%s", (*nl ? " : " : "\n"));
}
