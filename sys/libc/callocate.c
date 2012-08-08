/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_ALLOCATE -- Allocate a device (and mount it, if necessary).
*/
int
c_allocate (
  char *device			/* device to be allocated	*/
)
{
	int	status;

	iferr (status = XALLOCATE (c_sppstr(device)))
	    return (ERR);
	else
	    return (status);
}


/* C_DEALLOCATE -- Deallocate a device.
*/
int
c_deallocate (
  char	*device,		/* device to be allocated	*/
  int	rewind			/* rewind flag, if magtape	*/
)
{
	int	status;
	XINT    x_rewind = rewind;

	iferr (status = (int) XDEALLOCATE (c_sppstr(device), &x_rewind))
	    return (ERR);
	else
	    return (status);
}


/* C_DEVSTATUS -- Print the current status of the named device.
*/
void
c_devstatus (
  char	*device,		/* device name		*/
  int	out			/* output file		*/
)
{
	XINT  x_out = out;

	XDEVSTATUS (c_sppstr(device), &x_out);
}


/* C_DEVOWNER -- Determine if a device is allocated, and if so return
** the name of the owner.
*/
int
c_devowner (
  char	*device,		/* device to be allocated	*/
  char	*owner,			/* receives owner name string	*/
  int	maxch 
)
{
	PKCHAR	x_owner[SZ_FNAME+1];
	XINT	x_maxch = SZ_FNAME;
	int	status;


	iferr (status = (int) XDEVOWNER(c_sppstr(device), x_owner, &x_maxch)) {
	    owner[0] = EOS;
	    return (ERR);
	} else {
	    c_strpak (x_owner, owner, maxch);
	    return (status);
	}
}
