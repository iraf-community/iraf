/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#define	NOLIBCNAMES
#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_knames
#include <iraf.h>
#include "bootlib.h"


static	char	vfn[SZ_PATHNAME+1];

#ifdef NOVOS

/* OSFN2VFN -- Convert a local-directory OS filename into a virtual filename.
 * On UNIX this is a no-op since escape sequence encoding is not needed and
 * the IRAF file extensions are the same as UNIX.
 */
char *
osfn2vfn (
  char	*osfn 			/* input OS filename	*/
)
{
	strcpy (vfn, osfn);		/* [MACHDEP */
	return (vfn);
}

#else

/* OSFN2VFN -- Convert a local-directory OS filename into a virtual filename.
 * Undo the escape sequence encoding and map the OS filename extension into
 * the IRAF one.  No attempt is made to map OS directory names into IRAF
 * logical directory names; this is a local directory operation only.
 */
char *
osfn2vfn (
    char *osfn			/* input OS filename	*/
)
{
	XCHAR	x_osfn[SZ_PATHNAME+1];
	XCHAR	x_vfn[SZ_PATHNAME+1];
	XINT	x_maxch = SZ_PATHNAME;
	XINT	x_mode, vp, nchars;

	extern  void _envinit(void);


	_envinit();
	
	os_strupk ("./", x_vfn, SZ_PATHNAME);
	x_mode = VFN_UNMAP;
	iferr (vp = VFNOPEN (x_vfn, (integer *)&x_mode)) {
	    vp = 0;
	    goto err_;
	}

	strcpy ((char *)x_osfn, osfn);
	iferr (nchars = VFNUNMAP ((integer *)&vp, x_osfn, x_vfn, 
	    (integer *)&x_maxch))
	        goto err_;
	if (nchars < 0)
	    goto err_;

	x_mode = VFN_NOUPDATE;
	VFNCLOSE ((integer *)&vp, (integer *)&x_mode);

	os_strpak (x_vfn, vfn, SZ_PATHNAME);
	return (vfn);

err_:
	fprintf (stderr, "cannot unmap filename `%s'\n", osfn);
	if (vp > 0)
	    VFNCLOSE ((integer *)&vp, (integer *)&x_mode);

	strcpy (vfn, osfn);
	return (vfn);
}

#endif
