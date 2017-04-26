/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#define	import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

/* ZFNBRK -- Determine the offsets of the components of a virtual file name:
 *
 * 	"ldir$subdir/root.extn"
 * 
 * Both logical and OS dependent directory prefixes should be successfully
 * recognized, hence this routine is potentially machine dependent.  This
 * procedure appears to work correctly for AOS, VMS, and UNIX filenames, as well
 * as IRAF virtual filenames.
 * 
 * The legal characters in an IRAF VFN are [a-zA-Z0-9_.].  The character '.',
 * if present, separates the root file name from the extension.  If multiple
 * period delimited fields are present, the final field is taken to be the
 * extension, and the previous fields are included in the root name.  Other
 * characters may or may not be permitted in filenames depending upon the
 * restrictions of the host system.
 * 
 * The end of the logical directory prefix, if present, is marked by the index
 * of the last non-VFN character encountered.
 */
int
ZFNBRK (
  XCHAR	*vfn,			/* VFN to be scanned			*/
  XINT	*uroot_offset,		/* index of first char in root, or 0	*/
  XINT	*uextn_offset		/* index of first char in extn, or 0	*/
)
{
	register int	ch;
	register XCHAR	*ip;
	XCHAR	*root_offset, *extn_offset;

	root_offset = vfn;
	extn_offset = NULL;

	for (ip=vfn;  *ip != EOS;  ip++) {
	    ch = *ip;
	    if (ch == '\\' && *(ip+1) != EOS)
		ip++;
	    else if (ch == '.')
		extn_offset = ip;		/* possibly start of extn */
	    else if (ch == '$' || ch == '/' || ch == ':' || ch == ']')
		root_offset = ip+1;		/* part of logical name */
	}

	if (extn_offset <= root_offset)
	    extn_offset = ip;			/* no extension */
	else if (*(extn_offset+1) == EOS)
	    extn_offset = ip;			/* no extn if "root." */

	*uroot_offset = root_offset - vfn + 1;
	*uextn_offset = extn_offset - vfn + 1;

	return (XOK);
}
