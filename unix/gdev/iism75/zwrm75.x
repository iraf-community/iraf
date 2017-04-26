# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"m75.h"
include	"iis.h"

# ZWRM75 -- Initiate an asynchronous write to the IIS.  We are called to
# output the header of all instructions sent to the IIS.  There are 3 types
# of instructions; those which consist only of a header write, those which
# consist of a header write followed by a data write, and those which
# consist of a header write followed by a data read.  Translation of an M70
# instruction into an M75 instruction may involve moving information between
# the header and data block, hence we must save the headers of the read and
# write instructions until the data has been read or written.  The STATE
# variable in the channel descriptor is used to keep track of the instruction
# processing state.

procedure zwrm75 (ifcb, buf, nbytes, offset)

int	ifcb			# pointer to channel descriptor passed as int
char	buf[ARB]		# input buffer
int	nbytes			# number of  bytes to write
long	offset			# not used for this device

pointer	fcb
int	xferid, and()

begin
	fcb = ifcb

	if (FCB_STATE(fcb) == READY) {
	    # Start a new instruction.

	    if (nbytes != SZB_IISHDR) {
		FCB_STATUS(fcb) = ERR
		return
	    }

	    # Save the M70 header in the descriptor.
	    call amovs (buf, FCB_IISHDR(fcb), LEN_IISHDR)
	    xferid = XFERID(buf)

	    # Determine the state for the new instruction.

	    if (THINGCT(buf) == 0)
		FCB_STATE(fcb) = READY
	    else if (and (xferid, IREAD) != 0)
		FCB_STATE(fcb) = DATA_READ
	    else
		FCB_STATE(fcb) = DATA_WRITE

	    # If the new state is READY, no data read or write is needed,
	    # so just translate and output the header.

	    if (FCB_STATE(fcb) == READY)
		call m75put (fcb, buf, nbytes, offset)
	    else {
		# Set up a channel status as if we had just written the new
		# header, so that the next ZWTM75 will not return an error.

		FCB_STATUS(fcb) = IIS_INACTIVE
		FCB_NBYTES(fcb) = SZB_IISHDR
	    }
	    
	} else if (FCB_STATE(fcb) == DATA_WRITE) {
	    # This is the second zwrm75 call for a hdr+data output
	    # instruction.

	    call m75put (fcb, buf, nbytes, offset)
	    FCB_STATE(fcb) = READY

	} else {
	    # ZRDM75 should have been called, set error on the channel.
	    FCB_STATUS(fcb) = ERR
	}
end
