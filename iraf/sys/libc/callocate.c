/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_ALLOCATE -- Allocate a device (and mount it, if necessary).
 */
/* device : device to be allocated */
int c_allocate ( const char *device )
{
	int	status;

	iferr (status = XALLOCATE (c_sppstr(device)))
	    return (ERR);
	else
	    return (status);
}


/* C_DEALLOCATE -- Deallocate a device.
 */
/* device : device to be allocated  */
/* rewind : rewind flag, if magtape */
int c_deallocate ( const char *device, int rewind )
{
	int	status;
	XINT	x_rewind = rewind;

	iferr (status = XDEALLOCATE (c_sppstr(device), &x_rewind))
	    return (ERR);
	else
	    return (status);
}


/* C_DEVSTATUS -- Print the current status of the named device.
 */
/* device : device name */
/* out    : output file */
void c_devstatus ( const char *device, int out )
{
	XINT x_out = out;

	XDEVSTATUS (c_sppstr(device), &x_out);
}


/* C_DEVOWNER -- Determine if a device is allocated, and if so return
 * the name of the owner.
 */
/* device : device to be allocated     */
/* owner  : receives owner name string */
int c_devowner ( const char *device, char *owner, size_t bufsize )
{
	PKCHAR	x_owner[SZ_FNAME+1];
	XINT	x_maxch = SZ_FNAME;
	int	status;

	iferr (status = XDEVOWNER (c_sppstr(device), x_owner, &x_maxch)) {
	    owner[0] = EOS;
	    return (ERR);
	} else {
	    c_strpak (x_owner, owner, bufsize);
	    return (status);
	}
}
