/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

/*
** Copyright (c) 1980 Regents of the University of California.
** All rights reserved.
**
** Redistribution and use in source and binary forms are permitted
** provided that the above copyright notice and this paragraph are
** duplicated in all such forms and that any documentation,
** advertising materials, and other materials related to such
** distribution and use acknowledge that the software was developed
** by the University of California, Berkeley.  The name of the
** University may not be used to endorse or promote products derived
** from this software without specific prior written permission.
** THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
** IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
** WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
*/

#define import_libc
#include <iraf.h>

/*
** QSORT -- Quicker sort.  Adapted from the BSD sources.
*/

#define	THRESH		4		/* threshold for insertion */
#define	MTHRESH		6		/* threshold for median */

static  int (*qcmp)();			/* the comparison routine */
static  int qsz;			/* size of each record */
static  int thresh;			/* THRESHold in chars */
static  int mthresh;			/* MTHRESHold in chars */
static	void qst();


/* QSORT -- First, set up some global parameters for qst to share.  Then,
** quicksort with qst(), and then a cleanup insertion sort ourselves.
** Sound simple? It's not...
*/
void
qsort (
  char	*base,
  int	n,
  int	size,
  int	(*compar)()
)
{
	register char c, *i, *j, *lo, *hi;
	char	*minval, *maxval;


	if (n <= 1)
	    return;

	qsz     = size;
	qcmp    = compar;
	thresh  = qsz * THRESH;
	mthresh = qsz * MTHRESH;
	maxval  = base + n * qsz;

	if (n >= THRESH) {
	    qst (base, maxval);
	    hi = base + thresh;
	} else
	    hi = maxval;

	/* First put smallest element, which must be in the first THRESH, in
	 * the first position as a sentinel.  This is done just by searching
	 * the first THRESH elements (or the first n if n < THRESH), finding
	 * the min, and swapping it into the first position.
	 */
	for (j=lo=base;  (lo += qsz) < hi;  )
	    if ((*qcmp)(j, lo) > 0)
		j = lo;
	if (j != base) {
	    /* Swap j into place */
	    for (i=base, hi=base+qsz;  i < hi;  ) {
		c = *j;
		*j++ = *i;
		*i++ = c;
	    }
	}

	/* With our sentinel in place, we now run the following hyper-fast
	 * insertion sort.  For each remaining element, min, from [1] to [n-1],
	 * set hi to the index of the element AFTER which this one goes.
	 * Then, do the standard insertion sort shift on a character at a time
	 * basis for each element in the frob.
	 */
	for (minval=base;  (hi = minval += qsz) < maxval;  ) {
	    while ((*qcmp) (hi -= qsz, minval) > 0)
		/* void */;
	    if ((hi += qsz) != minval) {
		for (lo = minval + qsz; --lo >= minval; ) {
		    c = *lo;
		    for (i=j=lo;  (j -= qsz) >= hi;  i=j)
			*i = *j;
		    *i = c;
		}
	    }
	}
}


/* QST -- Do a quicksort.
 * First, find the median element, and put that one in the first place as the
 * discriminator.  (This "median" is just the median of the first, last and
 * middle elements).  (Using this median instead of the first element is a big
 * win).  Then, the usual partitioning/swapping, followed by moving the
 * discriminator into the right place.  Then, figure out the sizes of the two
 * partions, do the smaller one recursively and the larger one via a repeat of
 * this code.  Stopping when there are less than THRESH elements in a partition
 * and cleaning up with an insertion sort (in our caller) is a huge win.
 * All data swaps are done in-line, which is space-losing but time-saving.
 * (And there are only three places where this is done).
 */
static void
qst (
  char	*base, 
  char  *maxval
)
{
	register char c, *i, *j, *jj;
	register int ii;
	char *mid, *tmp;
	int lo, hi;

	/* At the top here, lo is the number of characters of elements in the
	 * current partition.  (Which should be maxval - base).
	 * Find the median of the first, last, and middle element and make
	 * that the middle element.  Set j to largest of first and middle.
	 * If maxval is larger than that guy, then it's that guy, else compare
	 * maxval with loser of first and take larger.  Things are set up to
	 * prefer the middle, then the first in case of ties.
	 */
	lo = maxval - base;		/* number of elements as chars */

	do {
	    mid = i = base + qsz * ((lo / qsz) >> 1);
	    if (lo >= mthresh) {
		j = ((*qcmp)((jj = base), i) > 0 ? jj : i);
		if ((*qcmp)(j, (tmp = maxval - qsz)) > 0) {
		    /* switch to first loser */
		    j = (j == jj ? i : jj);
		    if ((*qcmp)(j, tmp) < 0)
			j = tmp;
		}
		if (j != i) {
		    ii = qsz;
		    do  {
			c = *i;
			*i++ = *j;
			*j++ = c;
		    } while (--ii);
		}
	    }

	    /* Semi-standard quicksort partitioning/swapping
	     */
	    for (i = base, j = maxval - qsz; ; ) {
		while (i < mid && (*qcmp)(i, mid) <= 0)
		    i += qsz;
		while (j > mid) {
		    if ((*qcmp)(mid, j) <= 0) {
			j -= qsz;
			continue;
		    }
		    tmp = i + qsz;	/* value of i after swap */
		    if (i == mid) {
			/* j <-> mid, new mid is j */
			mid = jj = j;
		    } else {
			/* i <-> j */
			jj = j;
			j -= qsz;
		    }
		    goto swap;
		}

		if (i == mid) {
		    break;
		} else {
		    /* i <-> mid, new mid is i */
		    jj = mid;
		    tmp = mid = i;	/* value of i after swap */
		    j -= qsz;
		}

	    swap:
		ii = qsz;
		do	{
		    c = *i;
		    *i++ = *jj;
		    *jj++ = c;
		} while (--ii);
		i = tmp;
	    }

	    /* Look at sizes of the two partitions, do the smaller
	     * one first by recursion, then do the larger one by
	     * making sure lo is its size, base and maxval are update
	     * correctly, and branching back.  But only repeat
	     * (recursively or by branching) if the partition is
	     * of at least size THRESH.
	     */
	    i = (j = mid) + qsz;
	    if ((lo = j - base) <= (hi = maxval - i)) {
		if (lo >= thresh)
		    qst(base, j);
		base = i;
		lo = hi;
	    } else {
		if (hi >= thresh)
		    qst(i, maxval);
		maxval = j;
	    }

	} while (lo >= thresh);
}
