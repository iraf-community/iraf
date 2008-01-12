/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>

#include "../bootlib/bootlib.h"

/*
 * FNCACHE -- Maintain a cache of system logical filenames (e.g., <config.h>)
 * and their associated virtual filenames (e.g., "host$config/config.h").
 * This can greatly reduce the amount of time required to resolve references
 * to system include files in dependency file lists.
 *
 * External entry points:
 *
 *   nc = m_sysfile (lname, fname, bufsize)	# return file name
 *	   m_fninit (debug)			# initialize cache
 */

#define MAX_FILES	20		/* size of the cache		*/
#define SZ_SYSFILE_LNAME	32	/* size of logical name		*/
#define SZ_SYSFILE_FNAME	32	/* size of virtual file name	*/
#define EOS		'\0'

struct _sysfile {			/* cache list element structure	*/
	struct	_sysfile *uplnk;
	struct	_sysfile *dnlnk;
	int	nrefs;			/* number of references		*/
	int	chksum;			/* speeds searches		*/
	char	lname[SZ_SYSFILE_LNAME+1];	/* logical name			*/
	char	fname[SZ_SYSFILE_FNAME+1];	/* file name			*/
};

static struct _sysfile fncache[MAX_FILES];	/* the cache		*/
static struct _sysfile *fn_head;		/* doubly linked list	*/
static struct _sysfile *fn_tail;
static int fn_hits, fn_misses;

static struct _sysfile *fn_unlink( struct _sysfile * );
static struct _sysfile *fn_tohead( struct _sysfile * );
#if 0
static struct _sysfile *fn_totail( struct _sysfile * );
#endif
static int fn_chksum ( const char * );
static int fn_strncpy ( char *, const char *, size_t );

/* M_SYSFILE -- Search for the named system file and return the virtual file
 * name in the output string if the system file is found.  This is functionally
 * equivalent to os_sysfile().
 */
/* lname : logical name of system file	*/
/* fname : receives virtual file name	*/
int m_sysfile ( const char *lname, char *fname, size_t bufsize )
{
	struct _sysfile *fn;
	int chksum, fnlen;

	/* Look in the cache first.  For a small cache a linear search is
	 * plenty fast enough.
	 */
	chksum = fn_chksum (lname);
	for (fn=fn_head;  fn != NULL;  fn=fn->dnlnk)
	    if (fn->chksum == chksum && strcmp (lname, fn->lname) == 0) {
		fn_tohead (fn_unlink (fn));
		fn->nrefs++;
		fn_hits++;
		return (fn_strncpy (fname, fn->fname, bufsize));
	    }

	/* Cache miss.  Don't put in cache it name is too long.
	 */
	fn_misses++;
	fnlen = os_sysfile (lname, "sppincludes", fname, bufsize);
	if (fnlen > SZ_SYSFILE_FNAME || strlen(lname) > SZ_SYSFILE_LNAME)
	    return (fnlen);

	/* Put fname in the cache.  Reuse slot at tail of list.
	 */
	fn = fn_tohead (fn_unlink (fn_tail));
	safe_strcpy (fn->lname, SZ_SYSFILE_LNAME+1, lname);
	safe_strcpy (fn->fname, SZ_SYSFILE_LNAME+1, fname);
	fn->chksum = fn_chksum (lname);
	fn->nrefs  = 1;

	return (fnlen);
}


/* M_FNINIT -- Initialize (clear) the sysfile cache.
 */
void m_fninit ( int debug )
{
	struct _sysfile *fn;
	int i, total;

	if (debug) {
	    char    lname[SZ_SYSFILE_FNAME+1];

	    total = fn_hits + fn_misses;
	    printf ("file name cache: %d hits, %d misses, %d%% of %d\n",
		fn_hits, fn_misses, (total ? fn_hits * 100 / total : 0), total);

	    for (fn=fn_head;  fn != NULL;  fn=fn->dnlnk)
		if (fn->lname[0]) {
		    snprintf (lname, SZ_SYSFILE_FNAME+1, "<%s>", fn->lname);
		    printf ("%3d (%05d) %-20s => %s\n",
			fn->nrefs, fn->chksum, lname, fn->fname);
		}

	    fn_hits   = 0;
	    fn_misses = 0;

	    fflush (stdout);
	}

	fn = fn_head = fn_tail = &fncache[0];
	fn->uplnk   = NULL;
	fn->dnlnk   = NULL;
	fn->nrefs   = 0;
	fn->chksum  = -1;
	fn->lname[0] = EOS;

	for (i=1;  i < MAX_FILES;  i++) {
	    fn = fn_tohead (&fncache[i]);
	    fn->lname[0] = EOS;
	    fn->chksum   = -1;
	    fn->nrefs    = 0;
	}
}


/* FN_TOHEAD -- Link a sysfile struct at the head of the list.
 */
struct _sysfile *fn_tohead ( struct _sysfile *fn )
{
	if (fn != fn_head) {
	    fn->uplnk = NULL;
	    fn->dnlnk = fn_head;
	    fn_head->uplnk = fn;
	    fn_head = fn;
	}

	return (fn);
}


#if 0
/* FN_TOTAIL -- Link a sysfile struct at the tail of the list.
 */
struct _sysfile *fn_totail ( struct _sysfile *fn )
{
	if (fn != fn_tail) {
	    fn->uplnk = fn_tail;
	    fn->dnlnk = NULL;
	    fn_tail->dnlnk = fn;
	    fn_tail = fn;
	}

	return (fn);
}
#endif


/* FN_UNLINK -- Unlink an sysfile struct.
 */
struct _sysfile *fn_unlink ( struct _sysfile *fn )
{
	if (fn == fn_head)
	    fn_head = fn->dnlnk;
	if (fn == fn_tail)
	    fn_tail = fn->uplnk;

	if (fn->uplnk)
	    fn->uplnk->dnlnk = fn->dnlnk;
	if (fn->dnlnk)
	    fn->dnlnk->uplnk = fn->uplnk;
	
	return (fn);
}


/* FN_CHKSUM -- Compute the checksum of a character string.
 */
static int fn_chksum ( const char *s )
{
	int sum=0;

	while (*s)
	    sum += *s++;

	return (sum);
}


/* FN_STRNCPY -- Copy up to bufsize characters from a string and return the
 * number of characters copied as the function value.
 */
static int fn_strncpy ( char *out, const char *in, size_t bufsize )
{
	char *op, *maxop;
	const char *ip;

	maxop = out + bufsize -1;
	for ( ip=in, op=out ; op < maxop && (*ip) ; op++, ip++ )
	    *op = *ip;
	if ( op <= maxop ) *op = EOS;
	return (op-out);
}
