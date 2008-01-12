/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

#include "zos.h"

/*
 * ZSHLIB.C -- This file contains dummy shared library descriptors to be linked
 * into executables which do not use the Sun/IRAF shared library.  See zzstrt.c
 * and the code in the directory unix/shlib for additional information on the
 * shared library facility.
 */
int	sh_debug = 0;
unsigned XINT USHLIB[3] = { 0, 0, 0 };	/* actual length does not matter */
unsigned XINT VSHLIB[3] = { 0, 0, 0 };
unsigned XINT VSHEND;

#ifdef SOLARIS
int VLIBINIT( char *((*u_environ)[]), PFI u_malloc, PFI u_realloc, PFI u_free,
	      PFI u_dlopen, PFI u_dlclose, PFI u_dlsym, PFI u_dlerror )
{
    return 0;
}
#else
int VLIBINIT( char *((*u_environ)[]), PFI u_malloc, PFI u_realloc, PFI u_free )
{
    return 0;
}
#endif
