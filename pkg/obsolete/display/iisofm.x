# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <math.h>
include "zdisplay.h"
include "iis.h"

# These procedures have been modified to limit the maximum output level.

define	NIN	256			# Number of input levels
define	NOUT	1024			# Number of output levels

# IISOFM -- Output color mapping.

procedure iisofm (map)

char	map[ARB]		# type of mapping

int	i
short	lutr[LEN_OFM]
short	lutg[LEN_OFM]
short	lutb[LEN_OFM]

begin
	if (map[1] == 'm') {				# MONO
	    do i = 1, LEN_OFM
		lutr[i] = min ((i - 1) * NOUT / NIN, NOUT)
	    call iiswom (MONO, lutr)
	    return
	}

	call aclrs (lutr, LEN_OFM)
	call aclrs (lutg, LEN_OFM)
	call aclrs (lutb, LEN_OFM)

	if (map[1] == 'l') {				# LINEAR
	    call iislps (lutb, lutg, lutr)

	} else if (map[1] == '8') {			# 8COLOR
	    do i = 33, 64 {
		lutb[i] = NOUT - 1
		lutr[i] = NOUT - 1
	    }
	    do i = 65, 96
		lutb[i] = NOUT - 1
	    do i = 97, 128 {
		lutb[i] = NOUT - 1
		lutg[i] = NOUT - 1
	    }
	    do i = 129, 160
		lutg[i] = NOUT - 1
	    do i = 161, 192 {
		lutg[i] = NOUT - 1
		lutr[i] = NOUT - 1
	    }
	    do i = 193, 224
		lutr[i] = NOUT - 1
	    do i = 225, 256 {
		lutr[i] = NOUT - 1
		lutg[i] = NOUT - 1
		lutb[i] = NOUT - 1
	    }
	    do i = 257, LEN_OFM {
		lutr[i] = NOUT - 1
		lutg[i] = NOUT - 1
		lutb[i] = NOUT - 1
	    }	

	} else if (map[1] == 'r') {			# RANDOM
	    do i = 2, LEN_OFM, 8 {
		lutr[i] = NOUT - 1
		lutb[i] = NOUT - 1
	    }
	    do i = 3, LEN_OFM, 8
		lutb[i] = NOUT - 1
	    do i = 4, LEN_OFM, 8 {
		lutb[i] = NOUT - 1
		lutg[i] = NOUT - 1
	    }
	    do i = 5, LEN_OFM, 8
		lutg[i] = NOUT - 1
	    do i = 6, LEN_OFM, 8 {
		lutg[i] = NOUT - 1
		lutr[i] = NOUT - 1
	    }
	    do i = 7, LEN_OFM, 8
		lutr[i] = NOUT - 1
	    do i = 8, LEN_OFM, 8 {
		lutr[i] = NOUT - 1
		lutg[i] = NOUT - 1
		lutb[i] = NOUT - 1
	    }
	}

	call iiswom (RED, lutr)
	call iiswom (GREEN, lutg)
	call iiswom (BLUE, lutb)
end


# IISWOM -- Write output color look up table.

procedure iiswom (color, lut)

int	color
short	lut[ARB]
int	status

begin
	call iishdr (IWRITE+VRETRACE, LEN_OFM, OFM, ADVXONTC, ADVYONXOV,
	    color, 0)
	call iisio (lut, LEN_OFM * SZB_CHAR, status)
end


# IISROM -- Read color look up table.

procedure iisrom (color, lut)

int	color
short	lut[ARB]
int	status

begin
	call iishdr (IREAD+VRETRACE, LEN_OFM, LUT, ADVXONTC, ADVYONXOV,
	    color, 0)
	call iisio (lut, LEN_OFM * SZB_CHAR, status)
end


# Linear Pseudocolor Modelling code.

define	BCEN	64
define	GCEN	128
define	RCEN	196

# IISLPS -- Load the RGB luts for linear pseudocolor.

procedure iislps (lutb, lutg, lutr)

short	lutb[ARB]		# blue lut
short	lutg[ARB]		# green lut
short	lutr[ARB]		# red lut

begin
	# Set the mappings for the primary color bands.
	call iislps_curve (lutb, NIN, BCEN, NOUT - 1, NIN/2)
	call iislps_curve (lutg, NIN, GCEN, NOUT - 1, NIN/2)
	call iislps_curve (lutr, NIN, RCEN, NOUT - 1, NIN/2)

	# Add one half band of white color at the right.
	call iislps_curve (lutb, NIN, NIN, NOUT - 1, NIN/2)
	call iislps_curve (lutg, NIN, NIN, NOUT - 1, NIN/2)
	call iislps_curve (lutr, NIN, NIN, NOUT - 1, NIN/2)
end


# IISLPS_CURVE -- Compute the lookup table for a single color.

procedure iislps_curve (y, npts, xc, height, width)

short	y[npts]			# output curve
int	npts			# number of points
int	xc			# x center
int	height, width

int	i
real	dx, dy, hw

begin
	hw = width / 2.0
	dy = height / hw * 2.0

	do i = 1, npts {
	    dx = abs (i - xc)
	    if (dx > hw)
		;
	    else if (dx > hw / 2.0)
		y[i] = max (int(y[i]), min (height, int((hw - dx) * dy)))
	    else
		y[i] = height
	}
end
