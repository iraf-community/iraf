/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define import_knames
#include <iraf.h>

/* C_VFNBRK -- Break a virtual filename (or host filename) into its component
 * parts, i.e., logical directory (ldir), root, and extension.  No characters
 * are actually moved, rather, the offsets to the root and extn fields are
 * returned as output arguments.
 */
c_vfnbrk (vfn, root, extn)
char	*vfn;			/* virtual filename (or osfn)	*/
int	*root;			/* offset of root field.	*/
int	*extn;			/* offset of extn field.	*/
{
	XCHAR	sppvfn[SZ_PATHNAME];

	ZFNBRK (c_strupk(vfn,sppvfn,SZ_PATHNAME), &root, &extn);

	/* Make offsets zero-indexed. */
	*root -= 1;
	*extn -= 1;
}
