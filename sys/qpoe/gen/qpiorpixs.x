# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../qpio.h"

# QPIO_READPIX -- Sample the event list within the indicated rectangular
# region, using the given blocking factor, to produce a rectangular array
# of "pixels", where each pixel is a count of the number of events mapping
# to that location which pass the event attribute filter and region mask.
#
# NOTE -- It is left up to the caller to zero the output buffer before
# we are called.  (We merely increment the counts of the affected pixels).

int procedure qpio_readpixs (io, obuf, vs, ve, ndim, block)

pointer	io			#I QPIO descriptor
short	obuf[ARB]		#O output pixel buffer
int	vs[ndim], ve[ndim]	#I vectors defining region to be extracted
int	ndim			#I should be 2 for QPOE
int	block			#I blocking factor

pointer	sp, evl, ev
int	maxpix, maskval, xoff, yoff, xw, nev, totev, x, y, pix, i
errchk	qpio_getevents, qpio_setrange
int	qpio_getevents()

begin
	xw = (ve[1] - vs[1]) / block + 1
	if (xw <= 0 || ve[2] < vs[2])
	    return (0)

	call smark (sp)
	call salloc (evl, SZ_EVLIST, TY_POINTER)

	maxpix = xw * (ve[2] - vs[2] + 1)
	xoff = IO_EVXOFF(io)
	yoff = IO_EVYOFF(io)
	totev = 0

	# Define the region from which we wish to read events.
	call qpio_setrange (io, vs, ve, ndim)

	# Read the events.
	while (qpio_getevents (io, Memi[evl], maskval, SZ_EVLIST, nev) > 0) {
	    # Process a sequence of neighbor events.
	    do i = 1, nev {
		ev = Memi[evl+i-1]
		x  = Mems[ev+xoff]  
		y  = Mems[ev+yoff]

		pix = (y - vs[2]) / block * xw + (x - vs[1]) / block + 1
		if (pix > 0 && pix <= maxpix)
		    obuf[pix] = obuf[pix] + 1
	    }

	    totev = totev + nev
	}

	call sfree (sp)
	return (totev)
end
