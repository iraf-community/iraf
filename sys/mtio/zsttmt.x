# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fio.h>
include	"mtio.h"

# ZSTTMT -- Get magtape device characteristics (status).  Since the magtape
# device is a streaming device, the "file size" parameter is meaningless.
# The device block size must be returned as zero to signify that the device
# is a sequential byte stream (streaming) device.
#
# The optimum (default) buffer size is in principal device dependent, but more
# often it is merely system dependent (the same for all drives).  The optimum
# buffer size for input should be larger than the largest tape record expected.
# For output, it is the size device block which we wish to write.

procedure zsttmt (mtchan, what, lvalue)

int	mtchan		# mtio descriptor
int	what		# status parameter to be returned
long	lvalue		# returned status value
include	"mtio.com"

begin
	switch (what) {
	case FSTT_FILSIZE:				# not used
	    lvalue = 1
	case FSTT_BLKSIZE:				# streaming device
	    lvalue = 0
	case FSTT_OPTBUFSIZE:
	    if (MT_ACMODE(mtchan) == READ_ONLY)
		lvalue = MT_SZBDEFIBUF
	    else
		lvalue = MT_SZBDEFOBUF
	case FSTT_MAXBUFSIZE:
	    lvalue = max (0, MT_MAXBUFSIZE(mtchan))
	default:
	    lvalue = ERR
	}
end
