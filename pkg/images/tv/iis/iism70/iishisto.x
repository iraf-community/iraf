# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

# IISHISTO -- Activate, Read histogram.

procedure iishisto (rw, color, offset, a_n, data)

short	rw			# read or write
short	color[ARB]		# color(s) to write
short	offset			# offset into histogram table
short	a_n			# number of data values
short	data[ARB]		# the data

int	n, command, off, len, x, y, z
include "iis.com"

begin
	n = a_n
	if (n < 1) 
	    return

	# set the area to be histogrammed ... in data[1], currently
	# device very specific  ( 2 == whole region) .  Need to fix this
	# perhaps via specific graph plane filled with gkifill command to
	# depict area desired.
	# n must be twice the number of datum values.  Upper level code
	# must know this to leave enough room.  Would be better if upper
	# code could ignore this (fact).

	if (rw == IDS_WRITE) {
	    command = IWRITE+VRETRACE
	    x = 0
	    y = 0
	    z = 0
	    len = 1
	    data[1] = 2
	    call iishdr (command, len, VIDEOM+COMMAND, x, y, z, 0)
	    call iisio (data[1], len * SZB_CHAR)
	    return
	}

	off = offset
	command = IREAD+VRETRACE
	len = min (n, LEN_VIDEOM-off+1)
	off = min (LEN_VIDEOM, off) - 1
	y = off/MAXX + ADVYONXOV
	x = mod (off, MAXX) + ADVXONTC
	call iishdr (command, len, VIDEOM, x, y, z, 0)
	call iisio (data, len * SZB_CHAR)
end
