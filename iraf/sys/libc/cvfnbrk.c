/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_knames
#include <iraf.h>

/* C_VFNBRK -- Break a virtual filename (or host filename) into its component
 * parts, i.e., logical directory (ldir), root, and extension.  No characters
 * are actually moved, rather, the offsets to the root and extn fields are
 * returned as output arguments.
 */
/* vfn  : virtual filename (or osfn) */
/* root : offset of root field.      */
/* extn : offset of extn field.      */
void c_vfnbrk ( const char *vfn, int *root, int *extn )
{
	XCHAR	sppvfn[SZ_PATHNAME];
	XINT	x_root;
	XINT	x_extn;

	ZFNBRK (c_strupk(vfn,sppvfn,SZ_PATHNAME), &x_root, &x_extn);

	/* Make offsets zero-indexed. */
	x_root -= 1;
	x_extn -= 1;

	*root = x_root;
	*extn = x_extn;
}
