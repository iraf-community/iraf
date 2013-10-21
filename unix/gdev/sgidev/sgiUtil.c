/**
 *  SGIUTIL.C -- Shared utility procedures for the SGI translators.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define	import_spp
#define	import_error
#include <iraf.h>


/** 
 *  BSWAP2 -- Move bytes from array "a" to array "b", swapping successive
 *  pairs of bytes.  The two arrays may be the same but may not be offset
 *  and overlapping.
 */
void
bswap2 (
    unsigned char *a,			/* input array			*/
    unsigned char *b,			/* output array			*/
    int	  nbytes		/* number of bytes to swap	*/
)
{
    register unsigned char *ip, *op, *otop;
    register unsigned int temp;

    ip = a;
    op = b;
    otop = op + (nbytes & ~1);

    /* Swap successive pairs of bytes.
     */
    while (op < otop) {
        temp  = *ip++;
        *op++ = *ip++;
        *op++ = temp;
    }

    /* If there is an odd byte left, move it to the output array.
     */
    if (nbytes & 1)
        *op = *ip;
}


/* BSWAP4 - Move bytes from array "a" to array "b", swapping the four bytes
 * in each successive 4 byte group, i.e., 12345678 becomes 43218765.
 * The input and output arrays may be the same but may not partially overlap.
*/
void
bswap4 (
    unsigned char  *a,                     /* input array                  */
    unsigned char  *b,                     /* output array                 */
    int    nbytes                 /* number of bytes to swap      */
)
{
    register int    n;
    register unsigned char *ip, *op, *tp;
    static   unsigned char  temp[4];

    tp = temp;
    ip = (unsigned char *)a;
    op = (unsigned char *)b;

    /* Swap successive four byte groups.
     */
    for (n = nbytes >> 2;  --n >= 0;  ) {
        *tp++ = *ip++;
        *tp++ = *ip++;
        *tp++ = *ip++;
        *tp++ = *ip++;
        *op++ = *--tp;
        *op++ = *--tp;
        *op++ = *--tp;
        *op++ = *--tp;
    }

    /* If there are any odd bytes left, move them to the output array.
     * Do not bother to swap as it is unclear how to swap a partial
     * group, and really incorrect if the data is not modulus 4.
     */
    for (n = nbytes & 03;  --n >= 0;  )
        *op++ = *ip++;
}


/**
 *  ISSWAPPED -- Test whether we are running on a byte-swapped machine.
 */
int
isSwapped (void)
{
    union {
        short   tswap;
        char    b[2];
    } test;

    test.tswap = 1;
    return ((int) test.b[0]);
}	


/**
 *  GET_IARG -- Get an integer argument, whether appended directly to flag
 *  or separated by a whitespace character; if error, report and assign
 *  default.
 */
int
get_iarg (
    char   argp,
    char **argv,
    int    argno,
    int    def_val
)
{
    int     temp_val;

    if (argp == (char) 0) {
        if (argv[argno+1] == NULL) {
            fprintf (stderr, "missing arg to switch `%c';", argp);
            fprintf (stderr, " reset to %d\n", def_val);
            temp_val = def_val;
        } else
            temp_val = atoi (argv[++argno]);
    } else
        temp_val = atoi (argv[argno]+2);

    return (temp_val);
}
