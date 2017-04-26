# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "zdisplay.h"
include "iis.h"

# IISWND -- Window IIS display frame with the trackball.

procedure iiswnd3 (chan1, chan2, chan3)

int	chan1[ARB], chan2[ARB], chan3[ARB]

int	i, j
real	x, y
short	lut[LEN_LUT]
int	status, xcur, ycur, lutval
int	iisflu(), and()

begin
	if (iisflu(chan1) == GRCHAN)
	    return
	call iisrlt (chan1, lut)

	# Starting point at lut[2] because lut[1] is background
	for (i=3;  (i < 257) && (lut[i] == lut[2]);  i=i+1)
	    ; 		
	i = i - 1

	for (j=255;  (j > i) && (lut[j] == lut[256]);  j=j-1)
	    ; 
	j = j + 1

	if ((i == j) || (lut[i] == lut[j])) {
	    xcur = 256
	    ycur = 384
	} else {
	    y = real (lut[j] - lut[i]) / (j - i)
	    xcur = 2 * (i - 1) - (2 * lut[i] - 256) / y + 1
	    if (y > 1)
		y = 2 - (1 / y)
	    if (y < -1)
		y = -2 - (1 / y)
	    ycur = 128 * y + 256.5
	}

	xcur = xcur * MCXSCALE
	ycur = ycur * MCYSCALE
	call iiswcr (status, xcur, ycur)
	status = 0

	while (and (status, PUSH) == 0) {
	    call iisrcr (status, xcur, ycur)
	    if (status == EOF)
		break

	    xcur = xcur / MCXSCALE
	    ycur = ycur / MCYSCALE
	    x = xcur / 2
	    y = (ycur - 255.5) / 128.

	    if (y > 1)
		y = 1. / (2 - y)
	    if (y < - 1)
		y = -1. / (2 + y)
	    do i = 1, 256 {
		lutval = y * (i - 1 - x) + 127.5
		lut[i] = max (0, min (255, lutval))
	    }

	    lut[1] = 0			# Make background black
	    if ((chan1[1] == chan2[1]) && (chan1[1] == chan3[1]))
	        call iiswlt (chan1, lut)
	    else {
		call iiswlt (chan1, lut)
		call iiswlt (chan2, lut)
		call iiswlt (chan3, lut)
	    }
	}
end


# IISWLT -- Write monochrome look up table.

procedure iiswlt (chan, lut)

int	chan[ARB]
short	lut[ARB]

int	status
int	iisflu()

begin
	if (iisflu (chan) == GRCHAN)
	    return
	call iishdr (IWRITE+VRETRACE, LEN_LUT, LUT, ADVXONTC, 0, chan[2],
	    iisflu (chan))
	call iisio (lut, LEN_LUT * SZB_CHAR, status)
end


# IISRLT -- Read monochrome look up table.

procedure iisrlt (chan, lut)

int	chan[ARB]
short	lut[ARB]

int	status
int	iisflu()

begin
	if (iisflu (chan) == GRCHAN)
	    return
	call iishdr (IREAD+VRETRACE, LEN_LUT, LUT, ADVXONTC, 0, 0,
	    iisflu (chan))
	call iisio (lut, LEN_LUT * SZB_CHAR, status)
end
