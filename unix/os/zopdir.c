/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>

#include <dirent.h>


#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/*
 * ZOPDIR.C -- Routines for returning the contents of a directory as a list
 * of filename strings.
 *
 *	zopdir (fname, chan)
 *	zcldir (chan, status)
 *	zgfdir (chan, outstr, maxch, status)
 *
 * zopdir opens the directory, reads the contents into memory, and (for
 * unix systems) sorts the file list.  Successive calls to zgfdir return
 * successive elements of the list.  EOF is returned at the end of the list.
 */

#define	DEF_SBUFLEN	8192
#define	DEF_MAXENTRIES	512

struct dir {
	int	nentries;
	int	entry;
	char	*sbuf;
	int	*soff;
	DIR	*dir;
};

static	int _getfile();
static	int d_compar();
static	void d_qsort();
static	char *sbuf;
static	int *soff;
static	int nentries;


/* ZOPDIR -- Open a directory file.  A directory file is interfaced to FIO
 * as a textfile, using a portable set of textfile driver subroutines.
 * The directory access primitives contained in this file are called by the
 * driver subroutines to read successive machine dependent filenames from
 * a directory.
 */
int
ZOPDIR (PKCHAR *fname, XINT *chan)
{
	register char	*ip, *op;
	register DIR	*dir;
	char	osfn[SZ_PATHNAME+1];
	int	maxentries, sbuflen;
	int	nchars, sbufoff, fd;
	struct	dir *dp = NULL;


	/* The file name should have an "/" appended, if it is a proper
	 * directory prefix.  This must be removed to get the name of the
	 * directory file.
	 */
	memset (osfn, 0, SZ_PATHNAME+1);
	for (ip=(char *)fname, op=osfn;  (*op = *ip++) != EOS;  op++)
	    ;
	if (*--op == '/' && op > osfn)
	    *op = EOS;

	/* Open the directory. */
	dir = opendir (osfn);
	if (dir == NULL) {
	    *chan = XERR;
	    return (XERR);
	}

	nentries = 0;
	sbuflen = DEF_SBUFLEN;
	maxentries = DEF_MAXENTRIES;
	sbuf = (char *) malloc (sbuflen);
	soff = (int *) malloc (maxentries * sizeof(int));
	if (sbuf == NULL || soff == NULL)
	    goto err;

	/* Read the contents into the string buffer. */
	op = sbuf;
	while ((nchars = _getfile (dir, op, SZ_FNAME)) != EOF) {
	    soff[nentries++] = op - sbuf;
	    op += nchars + 1;

	    if (nentries >= maxentries) {
		maxentries *= 2;
		if ((soff = (int *) realloc (soff,
		    maxentries * sizeof(int))) == NULL)
		    goto err;
	    }
	    if (op + SZ_FNAME + 1 >= sbuf + sbuflen) {
		sbuflen *= 2;
		sbufoff = op - sbuf;
		if ((sbuf = (char *) realloc (sbuf, sbuflen)) == NULL)
		    goto err;
		op = sbuf + sbufoff;
	    }
	}

	/* Sort the file list. */
	d_qsort (soff, nentries, sizeof(int), d_compar);

	/* Free unused space. */
	if ((soff = (int *) realloc (soff, nentries * sizeof(int))) == NULL)
	    goto err;
	if ((sbuf = (char *) realloc (sbuf, op-sbuf)) == NULL)
	    goto err;
	if ((dp = (struct dir *) malloc (sizeof (struct dir))) == NULL)
	    goto err;

	/* Set up directory descriptor. */
	dp->nentries = nentries;
	dp->sbuf = sbuf;
	dp->soff = soff;
	dp->entry = 0;
	dp->dir = dir;

	fd = dirfd(dir);
	zfd[fd].fp = (FILE *)dp;

	*chan = fd;
	return (*chan);

err:
	if (soff)
	    free (soff);
	if (sbuf)
	    free (sbuf);
	if (dp)
	    free (dp);
	closedir (dir);
	*chan = XERR;

	return (XERR);
}


/* ZCLDIR -- Close a directory file.
 */
int
ZCLDIR (XINT *chan, XINT *status)
{
	register struct dir *dp = (struct dir *)zfd[*chan].fp;

	closedir (dp->dir);
	free (dp->sbuf);
	free (dp->soff);
	free (dp);

	*status = XOK;

	return (XOK);
}


/* ZGFDIR -- Get the next file name from an open directory file.  We are
 * called by the text file driver for a directory file, hence file names
 * are returned as simple packed strings.
 */
int
ZGFDIR (
  XINT	  *chan, 
  PKCHAR  *outstr,
  XINT    *maxch, 
  XINT    *status
)
{
	register struct dir *dp = (struct dir *)zfd[*chan].fp;
	register int	n, nchars;
	register char	*ip, *op;

	if (dp->entry < dp->nentries) {
	    ip = dp->sbuf + dp->soff[dp->entry++];  
	    op = (char *)outstr;
	    for (n = *maxch, nchars=0;  --n >= 0 && (*op++ = *ip++);  )
		nchars++;
	    ((char *)outstr)[nchars] = EOS;
	    *status = nchars;
	} else
	    *status = XEOF;

	return (*status);
}


/* GETFILE -- Get the next file name from an open directory file.
 */
static int
_getfile (DIR *dir, char *outstr, int maxch)
{
	register char *ip, *op;
	register int n;
	int status;
	register struct	dirent *dp;

	for (dp = readdir(dir);  dp != NULL;  dp = readdir(dir))
	    if (dp->d_ino != 0) {
		n = strlen (dp->d_name);
		status = n;
		for (ip=dp->d_name, op=outstr;  --n >= 0;  )
		    *op++ = *ip++;
		*op = EOS;
		return (status);
	    }

	return (EOF);
}


/*
 * QSORT -- Local version of quicksort, to make this code self contained.
 * -----------------------------
 */

/* COMPAR -- String comparision routine for what follows.
 */
static int
d_compar (char *a, char *b)
{
	return (strcmp (&sbuf[*(int *)a], &sbuf[*(int *)b]));
}

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * QSORT -- Quicker sort.  Adapted from the BSD sources.
 */

#define	THRESH		4		/* threshold for insertion */
#define	MTHRESH		6		/* threshold for median */

#ifdef min
#undef min
#undef max
#endif

static  int (*qcmp)();			/* the comparison routine */
static  int qsz;			/* size of each record */
static  int thresh;			/* THRESHold in chars */
static  int mthresh;			/* MTHRESHold in chars */
static	void d_qst();

/* QSORT -- First, set up some global parameters for qst to share.  Then,
 * quicksort with qst(), and then a cleanup insertion sort ourselves.
 * Sound simple? It's not...
 */
static void
d_qsort (char *base, int n, int size, int (*compar)())
{
	register char c, *i, *j, *lo, *hi;
	char	*min, *max;

	if (n <= 1)
	    return;

	qsz     = size;
	qcmp    = compar;
	thresh  = qsz * THRESH;
	mthresh = qsz * MTHRESH;
	max     = base + n * qsz;

	if (n >= THRESH) {
	    d_qst (base, max);
	    hi = base + thresh;
	} else
	    hi = max;

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
	for (min=base;  (hi = min += qsz) < max;  ) {
	    while ((*qcmp) (hi -= qsz, min) > 0)
		/* void */;
	    if ((hi += qsz) != min) {
		for (lo = min + qsz; --lo >= min; ) {
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
d_qst (char *base, char *max)
{
	register char c, *i, *j, *jj;
	register int ii;
	char *mid, *tmp;
	int lo, hi;

	/* At the top here, lo is the number of characters of elements in the
	 * current partition.  (Which should be max - base).
	 * Find the median of the first, last, and middle element and make
	 * that the middle element.  Set j to largest of first and middle.
	 * If max is larger than that guy, then it's that guy, else compare
	 * max with loser of first and take larger.  Things are set up to
	 * prefer the middle, then the first in case of ties.
	 */
	lo = max - base;		/* number of elements as chars */

	do {
	    mid = i = base + qsz * ((lo / qsz) >> 1);
	    if (lo >= mthresh) {
		j = ((*qcmp)((jj = base), i) > 0 ? jj : i);
		if ((*qcmp)(j, (tmp = max - qsz)) > 0) {
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
	    for (i = base, j = max - qsz; ; ) {
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
	     * making sure lo is its size, base and max are update
	     * correctly, and branching back.  But only repeat
	     * (recursively or by branching) if the partition is
	     * of at least size THRESH.
	     */
	    i = (j = mid) + qsz;
	    if ((lo = j - base) <= (hi = max - i)) {
		if (lo >= thresh)
		    d_qst(base, j);
		base = i;
		lo = hi;
	    } else {
		if (hi >= thresh)
		    d_qst(i, max);
		max = j;
	    }

	} while (lo >= thresh);
}
