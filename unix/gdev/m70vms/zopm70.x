# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<knet.h>
include	"m70.h"

# ZOPM70 -- Open the IIS for binary file i/o.  The device will be automatically
# allocated if necessary.

procedure zopm70 (device, mode, chan)

char	device[ARB]		# packed VMS device name
int	mode			# access mode
int	chan			# receives device channel

pointer	fcb
int	kchan
char	upkdev[SZ_FNAME]
int	ki_connect()

short	ier
%	character m70*4
%	integer*2 namw(2)
%	equivalence (m70, namw)

begin
	call calloc (fcb, LEN_FCB, TY_SHORT)

	# Use the binary file driver if the device resides on a remote node.
	# This precludes remote access to a VMS hosted IIS at present.

	if (ki_connect (device) != NULL) {
	    call zopnbf (device, mode, kchan)
	    if (kchan != ERR)
		FCB_KCHAN(fcb) = kchan
	} else {
	    # Load string descriptor for device name into FCB.
	    call strupk (device, upkdev, SZ_FNAME)
%	    call f77pak (upkdev, m70, 4)

	    FCB_U_NAME(fcb,1) = namw[1]
	    FCB_U_NAME(fcb,2) = namw[2]
	    FCB_KCHAN(fcb)    = NULL
	    FCB_STATUS(fcb)   = IIS_INACTIVE
	    FCB_NBYTES(fcb)   = 0

	    # Allocate and open the device.
	    call m70get (Mems[fcb], ier)
	    kchan = ier
	    if (kchan != 0)
		kchan = ERR
	}

	if (kchan < 0) {
	    call mfree (fcb, TY_SHORT)
	    chan = ERR
	} else
	    chan = fcb
end
