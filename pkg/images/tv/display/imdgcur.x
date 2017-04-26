# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	"iis.h"

# IMD_GCUR -- This is functionally equivalent to CLGCUR and should be used in
# place of the latter routine in programs which directly map the display.
# Its function is to close off the display at a low level in order to free
# the display device for access by the CL process for the cursor read.

int procedure imd_gcur (param, wx, wy, wcs, key, strval, maxch)

char	param[ARB]		# parameter to be read  [not used]
real	wx, wy			# cursor coordinates
int	wcs			# wcs to which coordinates belong
int	key			# keystroke value of cursor event
char	strval[ARB]		# string value, if any
int	maxch

int	status
bool	devopen
int	clgcur()
include	"iis.com"
include	"imd.com"

begin
	devopen = (iisnopen > 0)
	if (imd_magic == -1 && devopen)
	    call zclsgd (iischan, status)

	status = clgcur (param, wx, wy, wcs, key, strval, maxch)

	if (imd_magic == -1 && devopen)
	    call zopngd (imd_devinfo, imd_mode, iischan)

	return (status)
end
