# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <knet.h>
include "zdisplay.h"
include "iis.h"

# IISPIO -- Asynchronous pixel i/o to the IIS.

procedure iispio (buf, nx, ny)

short	buf[nx,ny]		# Cell array
int	nx, ny			# length, number of image lines

pointer	iobuf
bool	first_time
int	xferid, status, nbytes, szline, i
int	and()
include	"iis.com"
data	first_time /true/

begin
	if (first_time) {
	    if (packit)
		i = IIS_MAXBUFSIZE
	    else
		i = IIS_MAXBUFSIZE * (SZ_SHORT * SZB_CHAR)
	    call malloc (iobuf, i, TY_SHORT)
	    first_time = false
	}

	# Wait for the last i/o transfer.
	call iiswt (iischan, status)
	if (status == ERR)
	    return

	# Disable interrupts while transmitting to or receiving data from
	# the display, to avoid loss of synch on the datastream and resulting
	# loss of communications with the device.

	call intr_disable()
	xferid = XFERID(hdr)

	# Transmit the packet header.
	if (swap_bytes)
	    call bswap2 (hdr, 1, hdr, 1, SZB_IISHDR)
	call zawrgd (iischan, hdr, SZB_IISHDR, 0)
	call iiswt  (iischan, status)
	if (status == ERR) {
	    call intr_enable()
	    return
	}

	# Read or write the data block.
	nbytes = ny * iis_xdim
	szline = iis_xdim

	if (packit)
	    szline = szline / (SZ_SHORT * SZB_CHAR)
	else
	    nbytes = nbytes * (SZ_SHORT * SZB_CHAR)

	# Transmit the data byte-packed to increase the i/o bandwith
	# when using network i/o.

	if (and (xferid, IREAD) != 0) {
	    # Read from the IIS.

	    call zardgd (iischan, Mems[iobuf], nbytes, 0)
	    call iiswt  (iischan, status)

	    # Unpack and line flip the packed data.
	    if (packit) {
		do i = 0, ny-1
		    call achtbs (Mems[iobuf+i*szline], buf[1,ny-i], iis_xdim)
	    } else {
		do i = 0, ny-1
		    call amovs  (Mems[iobuf+i*szline], buf[1,ny-i], szline)
	    }

	} else {
	    # Write to the IIS.

	    # Bytepack the image lines, doing a line flip in the process.
	    if (packit) {
		do i = 0, ny-1
		    call achtsb (buf[1,ny-i], Mems[iobuf+i*szline], iis_xdim)
	    } else {
		do i = 0, ny-1
		    call amovs  (buf[1,ny-i], Mems[iobuf+i*szline], szline)
	    }

	    call zawrgd (iischan, Mems[iobuf], nbytes, 0)
	}

	call intr_enable()
end
