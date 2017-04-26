#define	import_spp
#define import_knames
#include <iraf.h>


#ifdef INT32_SUM

/**
 *  STRSUM -- Compute the 32-bit checksum of an SPP string.
 */

int
STRSUM (XCHAR *array, XINT *length, XINT *maxch)
{
    int      i, len, carry=0, newcarry=0;
    unsigned int *iarray, sum = 0;
    char     pkstr[*maxch];

    register int     n = *maxch;
    register XCHAR  *ip = array;
    register char   *op = (char *) pkstr;


    /*  Convert the input string to a packed char array.
     */
    while ((*op++ = *ip++) != XEOS && --n >= 0)
	;
    *--op = EOS;

    /*  Compute the checksum.
     */
    iarray = (unsigned int *) pkstr;
    len = *length / 4;

    for (i=0; i<len; i++) {
        if (iarray[i] > ~ sum)
            carry++;

        sum += iarray[i];
    }

    while (carry) {
        if (carry > ~ sum)
            newcarry++;
        sum += carry;
        carry = newcarry;
        newcarry = 0;
    }

    return (sum);
}

#else

/**
 *  STRSUM -- Compute the 32-bit checksum of an SPP string.
 */

int
STRSUM (XCHAR *array, XINT *length, XINT *maxch)
{
    int      i, len, carry=0, newcarry=0;
    unsigned int *iarray, sum = 0;
    unsigned long lsum = 0;
    char     pkstr[*maxch];

    register int     n = *maxch;
    register XCHAR  *ip = array;
    register char   *op = (char *) pkstr;


    /*  Convert the input string to a packed char array.
     */
    while ((*op++ = *ip++) != XEOS && --n >= 0)
	;
    *--op = EOS;

    /*  Compute the checksum.
     */
    iarray = (unsigned int *) pkstr;
    len = *length / 4;

    for (i=0; i<len; i++) {
        if (iarray[i] > ~ lsum)
            carry++;
        lsum += iarray[i];
    }

    while (carry) {
        if (carry > ~ lsum)
            newcarry++;
        lsum += carry;
        carry = newcarry;
        newcarry = 0;
    }

    return (abs(sum = lsum));
}

#endif
