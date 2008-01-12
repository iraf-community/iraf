# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <fset.h>
include "iis.h"
include "../lib/ids.h"

define	XSPLIT	LEN_SELECT+1
define	YSPLIT	LEN_SELECT+2

# ZSNAP_INIT -- initialize snap data structures.

procedure zsnap_init(kind)

short	kind

pointer	ptr
short	gram[LEN_GRAM]
short	select[LEN_SELECT+2]			# include split points
short	color[4]
short	frame[2]
short	cds, off, num
short	xsplit, x_right

int	i, j, k, temp
int	khp, val, frame_count
bool	used, mono
int	and(), or(), fstati()

include	"zsnap.com"
include "iis.com"
include "../lib/ids.com"

begin
	i_snap = true
	sn_frame = iframe
	sn_bitpl = iplane
	sn_fd = i_out
	call flush(sn_fd)
	call fseti(sn_fd, F_CANCEL, OK)
	prev_y = -1

	# Determine what snap range to do
	if (kind == IDS_SNAP_MONO)
	    mono= true
	else
	    mono = false

	switch (kind) {
	    case IDS_SNAP_RGB:
		# Note: BLU < RD and covers full color range
	        sn_start = BLU
	        sn_end   = RD

	    case IDS_SNAP_MONO, IDS_SNAP_BLUE:
	        sn_start = BLU
	        sn_end   = BLU

	    case IDS_SNAP_GREEN:
		sn_start = GR
		sn_end	 = GR

	    case IDS_SNAP_RED:
		sn_start = RD
		sn_end	 = RD
	}

	# Find out which planes are active -- any quadrant

	call iishdr (IREAD, LEN_SELECT+2, COMMAND+LUT, ADVXONTC, 0, 0, 0)
	call iisio (select, (LEN_SELECT+2)*SZB_CHAR)

	# record split point.  Adjust x_split so 511 becomes
	# 512. This is so the "right" side of a quadrant is given by one
	# plus the last used point.

	ysplit = select[YSPLIT]
	xsplit = select[XSPLIT]
	x_right = xsplit
	if (x_right == IIS_XDIM-1)
	    x_right = IIS_XDIM


	# For certain split positions, some quadrants don't appear at all.

	if (xsplit == 0)
	    call nullquad (0, 2, select)
	else if (xsplit == IIS_XDIM-1)
	    call nullquad (1, 3, select)
	if (ysplit == 0)
	    call nullquad (0, 1, select)
	else if (ysplit == IIS_YDIM-1)
	    call nullquad (2, 3, select)

	# Which frames are active, in any quadrant?

	temp = 0
	do i = 1, LEN_SELECT
	    temp = or (temp, int(select[i]))
	do i = 1, i_maxframes {
	    if ( and (temp, 2**(i-1)) != 0)
		on[i] = true
	    else
		on[i] = false
	}

	# Find out where each active plane starts and stops.  Split points
	# are screen coordinates, not picture coordinates.  Graphics does
	# not split (!).  left coord is inclusive, right is one beyond end.
	# left/right dimensions: color, above/below_ysplit, image_plane.
	# Frame_count counts frames in use.  Could be clever and only count
	# active frames whose pixels are on the screen (pan/zoom effects).

	frame_count = 0
	do i = 1, i_maxframes {
	    if ( !on[i] )
	        next
	    else
		frame_count = frame_count + 1
	    do j = sn_start, sn_end {	# implicit BLUE  (GREEN  RED)
		# quadrants for IIS are UL:0, UR:1, LL:2, LR:3
		do k = 0, 3 {
		    temp = select[(j-1)*4 + k + 1]
		    used = (and(temp, 2**(i-1)) != 0)
		    khp = k/2 + 1
		    switch (k) {
			case 0, 2:
			    if (used) {
				left[j,khp,i] = 0
				right[j,khp,i] = x_right
			    } else {
				left[j,khp,i] = -1
			    }
			
			case 1, 3:
			    if (used) {
				if ( left[j,khp,i] == -1)
				    left[j,khp,i] = xsplit
				right[j,khp,i] = IIS_XDIM
			    }
		    }   # end switch
		}       # end k ( quad loop)
	    }		# end j ( color loop)
	}		# end i ( frame loop)

	# now do range and offset

	cds = IDS_READ
	num = 3
	color[1] = IDS_BLUE
	color[2] = IDS_GREEN
	color[3] = IDS_RED
	color[4] = IDS_EOD
	call iisrange(cds, color, num, range)
	call iisoffset(cds, color, num, offset)
	do i = sn_start, sn_end
	    range[i] = 2**range[i]

	# now allocate memory for all the various tables

	call malloc (input,  IIS_XDIM, TY_SHORT)
	call malloc (answer, IIS_XDIM, TY_SHORT)
	call malloc (zs,     IIS_XDIM, TY_SHORT)
	# for each color:
	do j = sn_start, sn_end {
	    call malloc (result[j], IIS_XDIM,   TY_SHORT)
	    call malloc (ofmp[j],   LEN_OFM,    TY_SHORT)
	    call malloc (grp[j],    LEN_GRAM/2, TY_SHORT)
	    do i = 1, i_maxframes {
		if ( on[i] )
		    call malloc (lutp[j,i], LEN_LUT, TY_SHORT)
	    }
	}
	call malloc (grbit_on, IIS_XDIM, TY_INT)

	# fill these up

	cds = IDS_READ
	off = 1
	frame[2] = IDS_EOD
	color[2] = IDS_EOD
	do j = sn_start, sn_end {
	    if (j == BLU)
	        color[1] = IDS_BLUE
	    else if ( j == GR)
	        color[1] = IDS_GREEN
	    else
	        color[1] = IDS_RED
	    num = LEN_OFM
	    call iisofm (cds, color, off, num, Mems[ofmp[j]])
	    do i = 1, i_maxframes {
		if (on[i]) {
	            frame[1] = i
		    num = LEN_LUT
		    call iislut (cds, frame, color, off, num, Mems[lutp[j,i]])
		}
	    }
	}

	# the graphics planes ... assume insert mode!!
	# Note if any graphics mapping ram is in use...if no graphics on,
	# snap can run faster.

	call iishdr (IREAD, LEN_GRAM, GRAPHICS, ADVXONTC, 0, 0, 0)
	call iisio (gram, LEN_GRAM * SZB_CHAR)

	gr_in_use = false
	do j = sn_start, sn_end
	    call aclrs(Mems[grp[j]], LEN_GRAM/2)
	# Leave first one 0; don't mess with cursor plane
	do i = 2, LEN_GRAM/2 {
	    temp = and (77777B, int(gram[i]))
	    if (temp != 0)
		gr_in_use = true
	    if (! mono) {
		do j = sn_start, sn_end
		    switch (j) {
			case RD:
	                    Mems[grp[RD]+i-1]  = and (temp,76000B)/32
			case GR:
	                    Mems[grp[GR]+i-1]  = and (temp, 1740B)
			case BLU:
	                    Mems[grp[BLU]+i-1] = and (temp, 37B)*32
		    }
	    } else {
		# All graphics planes
		val = or ( and (temp, 76000B)/32, and (temp, 1740B))
		val = or ( and (temp, 37B)*32, val)
		Mems[grp[sn_start]+i-1] = val
	    }
	}

	if (gr_in_use)
	    frame_count = frame_count + 1
	if (frame_count > 1) {
	    multi_frame = true
	    # set buffer to size of one line
	    zbufsize = fstati (sn_fd, F_BUFSIZE)
	    call fseti (sn_fd, F_BUFSIZE, IIS_XDIM)
	} else
	    multi_frame = false

	# Now adjust look up tables for fact that they do 9 bit 2's complement
	# arithmetic!
	do j = sn_start, sn_end {
	    do i = 1, i_maxframes {
		if (on[i]) {
		    ptr = lutp[j,i]
		    do k = 1, LEN_LUT {
			if (Mems[ptr+k-1] > 255 )
			    Mems[ptr+k-1] = Mems[ptr+k-1] - 512
		    }
		}
	    }
	}
end


# NULLQUAD -- zero out lut mapping for quadrants that cannot appear on
# screen

procedure nullquad (q, p, sel)

int	q, p			# two quadrants to eliminate, zero based
short	sel[ARB]		# the mapping array

int	i

begin
	do i = 0,2 {
	    sel[i*4 + q + 1] = 0
	    sel[i*4 + p + 1] = 0
	}
end


# ZSNAP_DONE -- reset paramters

procedure zsnap_done()

int	i,j

include	"iis.com"
include "zsnap.com"
include "../lib/ids.com"

begin
	if ( ! i_snap )
	    return
	i_snap = false
	call fseti(sn_fd, F_CANCEL, OK)
	if (multi_frame) {
	    # restore buffering
	    call fseti (sn_fd, F_BUFSIZE, zbufsize)
	}
	iframe = sn_frame
	iplane = sn_bitpl

	# release storage
	call mfree (grbit_on, TY_INT)
	do j = sn_start, sn_end {
	    call mfree (result[j], TY_SHORT)
	    call mfree (ofmp[j],   TY_SHORT)
	    call mfree (grp[j],    TY_SHORT)
	    do i = 1, i_maxframes {
		if ( on[i] )
		    call mfree (lutp[j,i], TY_SHORT)
	    }
	}

	call mfree (zs,     TY_SHORT)
	call mfree (answer, TY_SHORT)
	call mfree (input,  TY_SHORT)
end
