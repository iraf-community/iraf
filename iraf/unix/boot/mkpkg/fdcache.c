/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>

#include "../bootlib/bootlib.h"
#include "mkpkg.h"

/*
 * FDCACHE -- Maintain a cache of filenames and their associated modification
 * dates.  This can greatly reduce the amount of time required to determine
 * which, if any, of the modules in a library need updating because an include
 * file they depend upon has been modified.
 *
 * External entry points:
 *
 *   l = m_fdate (fname)		# return file (modification) date
 *	m_fdinit (debug)		# initialize cache
 */

#define FD_MAX_FILES	20		/* size of the cache		*/
#define SZ_NAME		32		/* size of filename slot	*/
#define EOS		'\0'

struct _fdate {				/* cache list element structure	*/
	struct	_fdate *uplnk;
	struct	_fdate *dnlnk;
	int	nrefs;			/* number of references		*/
	int	chksum;			/* speeds searches		*/
	long	fdate;			/* file modification date	*/
	char	fname[SZ_NAME+1];	/* file name			*/
};

static struct _fdate fdcache[FD_MAX_FILES];	/* the cache		*/
static struct _fdate *fd_head;			/* doubly linked list	*/
static struct _fdate *fd_tail;
static int fd_hits, fd_misses;

static struct _fdate *fd_unlink( struct _fdate * );
static struct _fdate *fd_tohead( struct _fdate * );
#if 0
static struct _fdate *fd_totail( struct _fdate * );
#endif
static int fd_chksum ( const char * );


/* M_FDATE -- Get file modification date.  This is functionally equivalent to
 * os_fdate().
 */
long m_fdate ( const char *fname )
{
	struct _fdate *fd;
	int chksum;

	/* Look in the cache first.
	 */
	chksum = fd_chksum (fname);
	for (fd=fd_head;  fd != NULL;  fd=fd->dnlnk)
	    if (fd->chksum == chksum && strcmp (fname, fd->fname) == 0) {
		fd_tohead (fd_unlink (fd));
		fd->nrefs++;
		fd_hits++;
		return (fd->fdate);
	    }

	/* Cache miss.  Don't put in cache it name is too long.
	 */
	fd_misses++;
	if (strlen (fname) > SZ_NAME)
	    return (os_fdate (fname));

	/* Put fname in the cache.  Reuse slot at tail of list.
	 */
	fd = fd_tohead (fd_unlink (fd_tail));
	safe_strcpy (fd->fname, SZ_NAME+1, fname);
	fd->chksum = fd_chksum (fname);
	fd->fdate  = os_fdate (fname);
	fd->nrefs  = 1;

	return (fd->fdate);
}


/* M_FDINIT -- Initialize (clear) the fdate cache.
 */
void m_fdinit ( int debug )
{
	struct _fdate *fd;
	int i;
	int total;

	if (debug) {
	    total = fd_hits + fd_misses;
	    printf ("file date cache: %d hits, %d misses, %d%% of %d\n",
		fd_hits, fd_misses, (total ? fd_hits * 100 / total : 0), total);

	    for (fd=fd_head;  fd != NULL;  fd=fd->dnlnk)
		if (fd->fname[0])
		    printf ("%3d %10ld (%05d) %s\n",
			fd->nrefs, fd->fdate, fd->chksum, fd->fname);

	    fd_hits   = 0;
	    fd_misses = 0;

	    fflush (stdout);
	}

	fd = fd_head = fd_tail = &fdcache[0];
	fd->uplnk   = NULL;
	fd->dnlnk   = NULL;
	fd->nrefs   = 0;
	fd->chksum  = -1;
	fd->fname[0] = EOS;

	for (i=1;  i < FD_MAX_FILES;  i++) {
	    fd = fd_tohead (&fdcache[i]);
	    fd->fname[0] = EOS;
	    fd->chksum   = -1;
	    fd->nrefs    = 0;
	}
}


/* FD_TOHEAD -- Link a fdate struct at the head of the list.
 */
static struct _fdate *fd_tohead ( struct _fdate *fd )
{
	if (fd != fd_head) {
	    fd->uplnk = NULL;
	    fd->dnlnk = fd_head;
	    fd_head->uplnk = fd;
	    fd_head = fd;
	}

	return (fd);
}


#if 0
/* FD_TOTAIL -- Link a fdate struct at the tail of the list.
 */
static struct _fdate *fd_totail ( struct _fdate *fd )
{
	if (fd != fd_tail) {
	    fd->uplnk = fd_tail;
	    fd->dnlnk = NULL;
	    fd_tail->dnlnk = fd;
	    fd_tail = fd;
	}

	return (fd);
}
#endif


/* FD_UNLINK -- Unlink an fdate struct.
 */
static struct _fdate *fd_unlink ( struct _fdate *fd )
{
	if (fd == fd_head)
	    fd_head = fd->dnlnk;
	if (fd == fd_tail)
	    fd_tail = fd->uplnk;

	if (fd->uplnk)
	    fd->uplnk->dnlnk = fd->dnlnk;
	if (fd->dnlnk)
	    fd->dnlnk->uplnk = fd->uplnk;
	
	return (fd);
}


/* FD_CHKSUM -- Compute the checksum of a character string.
 */
static int fd_chksum ( const char *s )
{
	int sum=0;

	while (*s)
	    sum += *s++;

	return (sum);
}
