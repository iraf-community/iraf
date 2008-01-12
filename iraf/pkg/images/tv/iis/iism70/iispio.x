# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<knet.h>
include "iis.h"

# IISPIO -- Pixel i/o to the IIS.

procedure iispio (buf, ny)

short	buf[IIS_XDIM,ny]	# Cell array
int	ny			# number of image lines

pointer	iobuf
bool	first_time
int	xferid, status, npacked, szline, i
int	and()
include	"iis.com"
data	first_time /true/

begin
	if (first_time) {
	    call malloc (iobuf, IIS_MAXBUFSIZE, TY_CHAR)
	    first_time = false
	}

	# Wait for the last i/o transfer.
	call iiswt (iischan, status)
	if (status == ERR)
	    return

	# Transmit the packet header.
	call zawrgd (iischan, hdr, SZB_IISHDR, 0)
	call iiswt  (iischan, status)
	if (status == ERR)
	    return

	# Read or write the data block.
	npacked = ny * IIS_XDIM
	szline  = IIS_XDIM / (SZ_SHORT * SZB_CHAR)

	# Transmit the data byte-packed to increase the i/o bandwith
	# when using network i/o.

	xferid = XFERID(hdr)
	if (and (xferid, IREAD) != 0) {
	    # Read from the IIS.

	    call zardgd (iischan, Memc[iobuf], npacked, 0)
	    call iiswt  (iischan, status)

	    # Unpack and line flip the packed data.
	    do i = 0, ny-1
		call achtbs (Memc[iobuf+i*szline], buf[1,ny-i], IIS_XDIM)

	} else {
	    # Write to the IIS.

	    # Bytepack the image lines, doing a line flip in the process.
	    do i = 0, ny-1
		call achtsb (buf[1,ny-i], Memc[iobuf+i*szline], IIS_XDIM)

	    call zawrgd (iischan, Memc[iobuf], npacked, 0)
	}
end
