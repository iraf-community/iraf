# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "m75.h"
include "iis.h"

# M75PUT -- Translate an M70 instruction+data into an M75 instruction+data and
# output it to the display device.  We are called after both the instruction
# header and the data (if any) have been received.  The M70 header has been
# saved in the the channel descriptor and the data, if any, is in BUF.

procedure m75put (fcb, buf, nbytes_buf, offset)

pointer	fcb			# pointer to channel descriptor
short	buf[ARB]		# data array
int	nbytes_buf		# nbytes of data in buffer
long	offset			# not used

int	ifcb
bool	use_altbuf
short	altbuf[128]
short	m70[LEN_IISHDR]
short	m75[LEN_IISHDR]
int	nbytes, status, sum, i
int	xferid, subunit, xreg, yreg
int	and(), or(), not()

begin
	ifcb = fcb
	use_altbuf = false

	# Retrieve the M70 header from the channel descriptor.
	call amovs (FCB_IISHDR(fcb), m70, LEN_IISHDR)
	
	xferid  = XFERID(m70)
	subunit = SUBUNIT(m70)
	xreg	= XREG(m70)
	yreg	= YREG(m70)

	if (THINGCT(m70) == 0)
	    nbytes = 0
	else
	    nbytes = nbytes_buf

	# Start with a copy of the header for the M75, but turn off those bits
	# in the transfer id which the M70 knows nothing about and hence could
	# not have set.

	call amovs (m70, m75, LEN_IISHDR)
	XFERID(m75) = and (xferid,
	    not (BYTEORDER+PMA+ACCELERATE+REPEAT+PAGEMODE))

	# Translate the remaining fields of the header as necessary for each
	# subunit.

	switch (and (subunit, 77B)) {
	case REFRESH:
	    if (and (xreg, M70_ADVXONTC) != 0)
		subunit = or (subunit, M75_ADVXONTC)
	    if (and (xreg, M70_ADVXONYOV) != 0)
		subunit = or (subunit, M75_ADVXONYOV)
	    if (and (yreg, M70_ADVYONTC) != 0)
		subunit = or (subunit, M75_ADVYONTC)
	    if (and (yreg, M70_ADVYONXOV) != 0)
		subunit = or (subunit, M75_ADVYONXOV)

	    SUBUNIT(m75) = subunit
	    XREG(m75) = and (xreg, IIS_XDIM-1)
	    YREG(m75) = and (yreg, IIS_YDIM-1)

	case LUT:
	    XREG(m75) = and (xreg, 1777B)
	    YREG(m75) = 0

	case OFM:
	    XREG(m75) = and (xreg, 1777B)
	    YREG(m75) = 0

	    # The M70 feeds a 10 bit output DAC while the M75 DAC is 8 bits.
	    do i = 1, nbytes_buf / (SZB_CHAR * SZ_SHORT)
		buf[i] = buf[i] / 4

	case FEEDBACK:
	    subunit = COMMAND + FEEDBACK
	    SUBUNIT(m75) = subunit
	    XREG(m75) = 0
	    YREG(m75) = 0

	case GRAPHICS:
	    XREG(m75) = and (xreg, 777B)

	    # In a command mode transfer, the status register value is passed
	    # as data for the M70, but in the T register for the M75.

	    if (and (subunit, COMMAND) != 0) {
		TREG(m75) = buf[1]
		THINGCT(m75) = 0
		nbytes = 0
	    }

	case CURSOR:
	    XREG(m75) = and (xreg, 7777B)
	    YREG(m75) = 0

	case M70_SCROLL:
	    SUBUNIT(m75) = SCROLLZOOM
	    XREG(m75) = and (xreg, 3B)
	    YREG(m75) = 0
	    ZREG(m75) = ALLBITPL
	    TREG(m75) = SCROLL + WRAP

	case M70_ZOOM:
	    SUBUNIT(m75) = SCROLLZOOM
	    THINGCT(m75) = 2
	    XREG(m75) = and (xreg, 3B)
	    YREG(m75) = 0
	    ZREG(m75) = ALLBITPL
	    TREG(m75) = ZOOM + SCROLL + WRAP

	    # There are up to 3 words of data for the M70: zoom factor,
	    # x center, y center.  For the M75 the zoom is specified
	    # separately for each axis in the high bits of the word which
	    # contains the axis center.  For simplicity we require that
	    # all 3 words always be given.

	    altbuf[1] = buf[1] * 10000B + buf[2]
	    altbuf[2] = buf[1] * 10000B + buf[3]
	    use_altbuf = true
	    nbytes = 2 * (SZ_SHORT * SZB_CHAR)
	}

	# Compute the checksum for the new header.

	CHECKSUM(m75) = 1
	if (THINGCT(m75) > 0)
	    THINGCT(m75) = -THINGCT(m75)

	sum = 0
	do i = 1, LEN_IISHDR
	    sum = sum + m75[i]
	
	CHECKSUM(m75) = -sum

	# Output the header.

	call zzwrii (fcb, m75, SZB_IISHDR, offset)
	call zwtm75 (ifcb, status)
	if (status == ERR) {
	    FCB_STATUS(fcb) = ERR
	    return
	}

	# Output the data block, if any.

	if (nbytes > 0)
	    if (use_altbuf)
		call zzwrii (fcb, altbuf, nbytes, offset)
	    else
		call zzwrii (fcb,    buf, nbytes, offset)
end
