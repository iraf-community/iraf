/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_ALLOCATE -- Allocate a device (and mount it, if necessary).
 */
c_allocate (device)
char	*device;		/* device to be allocated	*/
{
	int	status;

	iferr (status = XALLOCATE (c_sppstr(device)))
	    return (ERR);
	else
	    return (status);
}


/* C_DEALLOCATE -- Deallocate a device.
 */
c_deallocate (device, rewind)
char	*device;		/* device to be allocated	*/
int	rewind;			/* rewind flag, if magtape	*/
{
	int	status;

	iferr (status = XDEALLOCATE (c_sppstr(device), &rewind))
	    return (ERR);
	else
	    return (status);
}


/* C_DEVSTATUS -- Print the current status of the named device.
 */
c_devstatus (device, out)
char	*device;	/* device name		*/
int	out;		/* output file		*/
{
	XDEVSTATUS (c_sppstr(device), &out);
}


/* C_DEVOWNER -- Determine if a device is allocated, and if so return
 * the name of the owner.
 */
c_devowner (device, owner, maxch)
char	*device;		/* device to be allocated	*/
char	*owner;			/* receives owner name string	*/
int	maxch;
{
	PKCHAR	x_owner[SZ_FNAME+1];
	XINT	x_maxch = SZ_FNAME;
	int	status;

	iferr (status = XDEVOWNER (c_sppstr(device), x_owner, &x_maxch)) {
	    owner[0] = EOS;
	    return (ERR);
	} else {
	    c_strpak (x_owner, owner, maxch);
	    return (status);
	}
}
