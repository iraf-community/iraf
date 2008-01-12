/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#define NOLIBCNAMES
#define import_spp
#define import_libc
#define import_xnames
#define import_knames
#include <iraf.h>

#include "bootlib.h"

static	PKCHAR	pk_osfn[SZ_PATHNAME+1];
static	char	*osfn = (char *)pk_osfn;

#ifdef NOVOS

/* VFN2OSFN -- Map an IRAF virtual filename into an OS filename.  This is
 * a simplified version for UNIX which does not use the VOS.  This version
 * should also be almost sufficient to compile the system libraries when
 * starting from scratch on a new machine, since the filenames in the system
 * directories are simple and the full generality of the FIO filename mapping
 * code is not required (extension mapping is about all that is required).
 * Only the well-known system logical directories are recognized in this
 * version, however ZGTENV is called to replace logical directories, and
 * this in turn references the host system environment, so one can bootstrap
 * things by using the host environment facilities.
 */
/* vfn : input IRAF virtual filename	*/
/* new : new file			*/
const char *vfn2osfn ( const char *vfn, int new )
{
	const char *ip, *ldir;
	char *op, *maxop;
	char fname[SZ_PATHNAME+1];

	/* Recursively expand logical directories, but don't do anything
	 * about subdirectories, extensions, etc.  This is all that is
	 * needed for UNIX.
	 */
	maxop = fname + SZ_PATHNAME+1 -1;
	for ( ip=vfn, op=fname ; op < maxop && (*ip) ; op++, ip++ ) {
	    if (*ip == '$') {
		*op = EOS;
		if (ldir = os_getenv (fname))
		    safe_strcpy (fname, SZ_PATHNAME+1, ldir);
		safe_strcat (fname, SZ_PATHNAME+1, ip+1);
		return (vfn2osfn (fname, 0));
	    }
	    *op = *ip;
	}
	*op = EOS;

	/* Copy filename to the output string.  Fix up the "//" sequences
	 * that occur because IRAF likes the / at the end of logical directory
	 * names.
	 */
	for ( ip=fname, op=osfn ; (*ip) ; op++, ip++ ) {
	    *op = *ip;
	    if (*op == '/' && op > osfn && *(op-1) == '/')
		--op;
	}
	*op = EOS;

	return (osfn);
}


#else

/* VFN2OSFN -- Map an IRAF virtual filename into an OS filename.  This is
 * the portable version using the VOS (libsys.a+libvops.a+libos.a) to do the
 * mapping.  The system libraries must have been built before we can do this,
 * of course.
 */
/* vfn : input IRAF virtual filename	*/
/* new : new file			*/
const char *vfn2osfn ( const char *vfn, int new )
{
	const char *ip;
	XCHAR *op, *maxop;
	XINT vp, mode, x_maxch = SZ_PATHNAME;
	PKCHAR upkvfn[SZ_PATHNAME+1];
	int err;

	/* Copy the input filename into local storage before calling envinit,
	 * below, to avoid any chance of overwriting the input string in a
	 * recursive call to vfn2osfn by envinit.
	 */
	maxop = upkvfn + SZ_PATHNAME+1 -1;
	for ( ip=vfn, op=upkvfn ; op < maxop && *ip != (XCHAR)EOS ; op++, ip++ )
	    *op = *ip;
	*op = XEOS;
	mode = new ? VFN_WRITE : VFN_READ;

	/* Nasty beast that can call vsn2osfn recursively. */
	_envinit();

	err = 0;
	iferr (vp = VFNOPEN (upkvfn, &mode)) {
	    fprintf (stderr, "Warning: cannot open vfn `%s' for %s\n",
		vfn, mode == VFN_WRITE ? "writing" : "reading");
	    err++;
	}

	if (new) {
	    if (!err)
		iferr (VFNADD (&vp, pk_osfn, &x_maxch))
		    fprintf (stderr, "Warning: cannot add filename `%s'\n",vfn);
	} else {
	    if (!err)
		iferr (VFNMAP (&vp, pk_osfn, &x_maxch))
		    fprintf (stderr, "Warning: cannot map filename `%s'\n",vfn);
	}

	mode = (mode == VFN_WRITE) ? VFN_UPDATE : VFN_NOUPDATE;
	if (!err) {
	    iferr (VFNCLOSE (&vp, &mode))
		fprintf (stderr, "Warning: error closing mapping file\n");
	} else
	    *osfn = EOS;

	return (osfn);
}


/*
 * KISTUB -- Stub out selected KI (kernel network interface) routines.  This
 * is done when VOS filename mapping is in use to avoid linking in a lot of
 * objects that will never be used, since the HSI does not use networking.
 */
XINT KI_GETHOSTS( void )
{
    return 0;
}

XINT KI_SEND( XINT *server, XINT *opcode, XINT *subcode )
{
    return 0;
}
XINT KI_RECEIVE( XINT *server, XINT *opcode, XINT *subcode )
{
    return 0;
}
#endif

#ifdef SUNOS
/* Stub out the following too, since there is no floating point in the HSI. */
typedef void (*sigfpe_handler_type)();
int ieee_flags( const char *action, const char *mode, const char *in, 
		char **out )
{
    return 0;
}
long ieee_handler( const char *action, const char *exception,
		   sigfpe_handler_type hdl )
{
    return 0;
}
void abrupt_underflow_( void )
{
    return;
}
#endif
