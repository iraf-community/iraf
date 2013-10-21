/******************************************************************************
**  RANGES -- Simple range-specification package to decode lists of numbers
**  or ranges of the form:
**
**		<start> '-' <end> [ 'x' <stride>]
**
**  Sample code using this package might look like:
**
**
**    #include <stdlib.h>
**    #include <ctype.h>
**
**    int vot_decodeRanges(char *str, int *ranges, int max_ranges,int *nvalues);
**    int get_next_number (int *ranges, int number);
**
**    int main (int argc, char **argv)
**    {
**        char *rtest[] = { "1-10,12,16",      "1-7x2,12-14",
**		            "1,3,5,7,9-12",    "1,8",
**		            "12-8",            "12-8x-1",
**		            NULL };
**        int  i, *ranges, max_ranges=100, nvalues=0, val;
**
**        ranges = (int *) malloc (1024);
**        for (i=0; rtest[i]; i++) {
** 	    printf ("vot_decodeRanges: '%s'\n\n   ", rtest[i]);
**	    val = vot_decodeRanges (rtest[i], ranges, max_ranges, &nvalues);
**	    for (val=0; (val = get_next_number (ranges, val)) > 0; )
** 	        printf (" %d ", val);
**        }
**    }
** 	    
**  Assumes ranges are positive and always increasing, i.e. a range "3-1"
**  will not return (3,2,1) but (1,2,3).  Default stride is 1, open-ended
**  ranges are not permitted.
**
**  M. Fitzpatrick, NOAO, June 2007  (Translated from IRAF xtools$ranges.x)
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <ctype.h>
#include "VOClient.h"
#include "voAppsP.h"


#define	FIRST	1			/* Default starting range	*/
#define	LAST	MAX_INT			/* Default ending range		*/
#define	STEP	1			/* Default step			*/
#define	EOLIST	0			/* End of list			*/

#define	MAX_INT	65535
#define	INDEF	65534
/*#define ERR	-1*/
#define EOS	'\0'
#define OK	0


static int ctoi (char *str, int *ip, int *ival);

/*************************************************************************
**  DECODERANGES -- Parse a string containing a list of integer numbers or
**  ranges, delimited by either spaces or commas.  Return as output a list
**  of ranges defining a list of numbers, and the count of list numbers.
**  Range limits must be positive nonnegative integers.  ERR is returned as
**  the function value if a conversion error occurs.  The list of ranges is
**  delimited by EOLIST.
*/
int 
vot_decodeRanges (range_string, ranges, max_ranges, nvalues)

char	*range_string;		/* Range string to be decoded		*/
int	ranges[];		/* Range array				*/
int	max_ranges;		/* Maximum number of ranges		*/
int	*nvalues;		/* The number of values in the ranges	*/

{
    int	  ip, nrange, first, last, step, diff, *ra = &ranges[0];

    ip = 0;
    *nvalues = 0;

    for (nrange = 0; nrange < (max_ranges - 1); nrange += 3) {
	/* Defaults to all nonnegative integers. 
	*/
	first = FIRST;
	last = LAST;
	step = STEP;

	/* Skip delimiters.
	*/
	while (isspace(range_string[ip]) || range_string[ip] == ',')
	    ip++;

	/* Get first limit.  Must be a number, '-', 'x', or EOS.  
	** If not return ERR.
	*/
	if (range_string[ip] == EOS) {		/* end of list	 	*/
	    if (nrange == 0) {
		ra[0] = first; 			/* null string defaults */
		ra[1] = last;
		ra[2] = step;
		ra[3] = EOLIST;
	    	*nvalues = MAX_INT;
		return (OK);
	    } else {
		ra[nrange] = EOLIST;
		return (OK);
	    }
	} else if (range_string[ip] == '-')
	    ;
	else if (range_string[ip] == 'x')
	    ;
	else if (isdigit((int)range_string[ip])) {	/* ,n..		*/
	    if (ctoi (range_string, &ip, &first) == 0)
		return (ERR);
	} else
	    return (ERR);

	/* Skip delimiters.
	*/
	while (isspace(range_string[ip]) || range_string[ip] == ',')
	    ip++;

	/* Get last limit.  Must be '-', or 'x' otherwise last = first.
	*/
	if (range_string[ip] == 'x')
	    ;
	else if (range_string[ip] == '-') {
	    ip++;
	    while (isspace(range_string[ip]) || range_string[ip] == ',')
		ip++;
	    if (range_string[ip] == EOS)
		;
	    else if (isdigit((int)range_string[ip])) {
		if (ctoi (range_string, &ip, &last) == 0)
		    return (ERR);
	    } else if (range_string[ip] == 'x')
		;
	    else
		return (ERR);
	} else
	    last = first;

	/* Skip delimiters
	*/
	while (isspace(range_string[ip]) || range_string[ip] == ',')
	    ip++;

	/* Get step.  Must be 'x' or assume default step.
	*/
	if (range_string[ip] == 'x') {
	    ip++;
	    while (isspace(range_string[ip]) || range_string[ip] == ',')
	        ip++;
	    if (range_string[ip] == EOS)
		;
	    else if (isdigit((int)range_string[ip])) {
		if (ctoi (range_string, &ip, &step) == 0)
		    ;
		if (step == 0)
		    return (ERR);
	    } else if (range_string[ip] == '-')
		;
	    else
		return (ERR);
	}

	/* Output the range triple. 
	*/
	ra[nrange  ] = first;
	ra[nrange+1] = last;
	ra[nrange+2] = step;
	diff = last - first;
	if (diff < 0)
	    diff = - (diff);
	*nvalues = *nvalues + diff / step + 1;
    }

    return (ERR);				/* ran out of space */
}


/*  GET_NEXT_NUMBER -- Given a list of ranges and the current file number,
**  find and return the next file number.  Selection is done in such a way
**  that list numbers are always returned in monotonically increasing order,
**  regardless of the order in which the ranges are given.  Duplicate entries
**  are ignored.  EOF is returned at the end of the list.
*/
int 
get_next_number (int ranges[], int number)
{
    int	    a, b, ip, first, last, step, next_number, remainder;


    /* If number+1 is anywhere in the list, that is the next number,
    ** otherwise the next number is the smallest number in the list which
    ** is greater than number+1.
    */
    number = number + 1;
    next_number = MAX_INT;

    for (ip=0;  ranges[ip] != EOLIST;  ip=ip+3) {
	a = ranges[ip];
	b = ranges[ip + 1];
	first = (a < b) ? a : b;
	last  = (a > b) ? a : b;
	step  = ranges[ip+2];
	if (step == 0) {
	    fprintf (stderr, "ERROR: Step size of zero in range list");
	    return (0);
	}

	if (number >= first && number <= last) {
	    remainder = (number - first) % step;
	    if (remainder == 0)
	        return (number);
	    if ((number - remainder + step) <= last)
	        next_number = number - remainder + step;
	} else if (first > number)
	    next_number = (next_number <  first) ? next_number : first;
    }

    if (next_number == MAX_INT)
	return (EOF);
    else {
	number = next_number;
	return (number);
    }
}


/*  IS_IN_RANGE -- Test number to see if it is in range.
*/
int 
is_in_range (int ranges[], int number)
{
    int	  a, b, ip, first, last, step, num = number;

    for (ip=0;  ranges[ip] != EOLIST;  ip += 3) {
	a = ranges[ip];
	b = ranges[ip + 1];
	first = (a < b) ? a : b;
	last  = (a > b) ? a : b;
	step = ranges[ip+2];
	if (num >= first && num <= last)
	    if (((num - first) % step) == 0)
		return (1);
    }

    return (0);
}


/* CTOI -- Simple character to integer (decimal radix) that advances the 
** character pointer (index).
*/
static int
ctoi (char *str, int *ip, int *ival)
{
    int	neg, sum, ip_start;

    while (isspace (str[*ip]))
	*ip = *ip + 1;
    ip_start = *ip;

    if ((neg = (str[*ip] == '-')))
	*ip = *ip + 1;

    sum = 0;
    while (isdigit ((int) str[*ip])) {
	sum = sum * 10 + (int) (str[*ip] - '0');
	*ip = *ip + 1;
    }

    if (neg)
        *ival = -sum;
    else
        *ival = sum;

    return (*ip - ip_start);
}
