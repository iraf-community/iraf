# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"m75.h"
include	"iis.h"

# ZRDM75 -- Initiate an asynchronous read of data from the IIS.  Note that
# the zwrm75 procedure is called to write the header for data reads, as well
# as writes.  Hence we should be called only after the header has been saved
# in the channel descriptor by ZWRM75, leaving the channel in state DATA_READ.
# Our task is to translate and output the header, read the M75 data block, and
# return the data block to the user after performing any transformations
# necessary to make it look like M70fdata.

procedure zrdm75 (ifcb, buf, nbytes_buf, offset)

int	ifcb			# pointer to channel descriptor passed as int
short	buf[ARB]		# data array
int	nbytes_buf		# nbytes of data in buffer
long	offset			# not used

pointer	fcb
short	m70[LEN_IISHDR]
short	m75[LEN_IISHDR]
int	nbytes, status, sum, i
int	xferid, subunit, xreg, yreg
int	and(), or(), not()

begin
	fcb = ifcb
	nbytes = nbytes_buf

	if (FCB_STATE(fcb) != DATA_READ) {
	    FCB_STATUS(fcb) = ERR
	    return
	}

	# Retrieve the M70 header from the channel descriptor.
	call amovs (FCB_IISHDR(fcb), m70, LEN_IISHDR)
	
	xferid  = XFERID(m70)
	subunit = SUBUNIT(m70)
	xreg	= XREG(m70)
	yreg	= YREG(m70)

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

	    # The M70 OFM lookup table is 10 bits deep, whereas the M75 table
	    # is only 8 bits deep, so scale the 8 bit M75 values up to 10 bits.

	    do i = 1, nbytes_buf / (SZB_CHAR * SZ_SHORT)
		buf[i] = buf[i] * 4

	case FEEDBACK:
	    subunit = COMMAND + FEEDBACK
	    SUBUNIT(m75) = subunit
	    XREG(m75) = 0
	    YREG(m75) = 0

	case GRAPHICS:
	    XREG(m75) = and (xreg, 777B)
	    TREG(m75) = 0			# ??

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
	    nbytes = 2 * (SZ_SHORT * SZB_CHAR)
	    XREG(m75) = and (xreg, 3B)
	    YREG(m75) = 0
	    ZREG(m75) = ALLBITPL
	    TREG(m75) = ZOOM
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

	# Read the data block.

	if (nbytes > 0) {
	    call zzrdii (fcb, buf, nbytes, offset)
	    call zwtm75 (ifcb, status)
	    if (status <= 0) {
		FCB_STATUS(fcb) = ERR
		return
	    }
	}

	# Perform any transformations on the data just read necessary to
	# convert it into M70 format.  If the number of bytes read is
	# different than that expected by the M70, be sure to set the
	# expected count in the channel descriptor for the next ZWTM75.

	if (and (subunit, 77B) == ZOOM) {
	    FCB_NBYTES(fcb) = 3 * (SZ_SHORT * SZB_CHAR)
	    FCB_STATUS(fcb) = IIS_INACTIVE
	    buf[3] = mod (int(buf[2]), 10000B) 
	    buf[2] = mod (int(buf[1]), 10000B) 
	    buf[1] = buf[1] / 10000B
	}

	FCB_STATE(fcb) = READY
end
