# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <syserr.h>
include	"../qpio.h"

# QPIO_READPIX -- Sample the event list within the indicated rectangular
# region, using the given blocking factor, to produce a rectangular array
# of "pixels", where each pixel is a count of the number of events mapping
# to that location which pass the event attribute filter and region mask.
#
# NOTE -- It is left up to the caller to zero the output buffer before
# we are called.  (We merely increment the counts of the affected pixels).

int procedure qpio_readpixi (io, obuf, vs, ve, ndim, xblock, yblock)

pointer	io			#I QPIO descriptor
int	obuf[ARB]		#O output pixel buffer
int	vs[ndim], ve[ndim]	#I vectors defining region to be extracted
int	ndim			#I should be 2 for QPOE
real	xblock, yblock		#I blocking factors

double	x, y
pointer	sp, evl, ev_p
int	evtype, maxpix, maskval, xoff, yoff, xw, yw, nev, totev, pix, i, j
errchk	qpio_getevents, qpio_setrange, syserr
int	qpio_getevents()

begin
	# Verify arguments.
	if (xblock <= 0 || xblock > (ve[1] - vs[1] + 1))
	    call syserr (SYS_QPBLOCKOOR)
	if (yblock <= 0 || yblock > (ve[2] - vs[2] + 1))
	    call syserr (SYS_QPBLOCKOOR)

	# Compute the size of the output matrix in integer pixels.  This
	# truncates the last partially filled pixel in each axis.

	xw = int ((ve[1] - vs[1] + 1) / xblock + (EPSILOND * 1000))
	yw = int ((ve[2] - vs[2] + 1) / yblock + (EPSILOND * 1000))
	if (xw <= 0 || yw <= 0)
	    return (0)

	call smark (sp)
	call salloc (evl, SZ_EVLIST, TY_POINTER)

	xoff = IO_EVXOFF(io)
	yoff = IO_EVYOFF(io)
	maxpix = xw * yw
	totev = 0

	evtype = IO_EVXTYPE(io)
	if (IO_EVXTYPE(io) != IO_EVYTYPE(io))
	    call syserr (SYS_QPINVEVT)

	# Define the region from which we wish to read events.
	call qpio_setrange (io, vs, ve, ndim)

	# Read the events.
	while (qpio_getevents (io, Memi[evl], maskval, SZ_EVLIST, nev) > 0) {
	    switch (evtype) {

	    case TY_SHORT:
		# Process a sequence of neighbor events.
		do i = 1, nev {
		    ev_p = (Memi[evl+i-1] - 1) * SZ_SHORT / SZ_SHORT + 1

		    x = Mems[ev_p+xoff]  
		    y = Mems[ev_p+yoff]

		    j = int ((y - vs[2]) / yblock + (EPSILOND * 1000))
		    if (j >= 0 && j < yw) {
			pix = j * xw + (x - vs[1]) / xblock + 1
			if (pix > 0 && pix <= maxpix)
			    obuf[pix] = obuf[pix] + 1
		    }
		}

	    case TY_INT:
		# Process a sequence of neighbor events.
		do i = 1, nev {
		    ev_p = (Memi[evl+i-1] - 1) * SZ_SHORT / SZ_INT + 1

		    x = Memi[ev_p+xoff]  
		    y = Memi[ev_p+yoff]

		    j = int ((y - vs[2]) / yblock + (EPSILOND * 1000))
		    if (j >= 0 && j < yw) {
			pix = j * xw + (x - vs[1]) / xblock + 1
			if (pix > 0 && pix <= maxpix)
			    obuf[pix] = obuf[pix] + 1
		    }
		}

	    case TY_LONG:
		# Process a sequence of neighbor events.
		do i = 1, nev {
		    ev_p = (Memi[evl+i-1] - 1) * SZ_SHORT / SZ_LONG + 1

		    x = Meml[ev_p+xoff]  
		    y = Meml[ev_p+yoff]

		    j = int ((y - vs[2]) / yblock + (EPSILOND * 1000))
		    if (j >= 0 && j < yw) {
			pix = j * xw + (x - vs[1]) / xblock + 1
			if (pix > 0 && pix <= maxpix)
			    obuf[pix] = obuf[pix] + 1
		    }
		}

	    case TY_REAL:
		# Process a sequence of neighbor events.
		do i = 1, nev {
		    ev_p = (Memi[evl+i-1] - 1) * SZ_SHORT / SZ_REAL + 1

		    x = Memr[ev_p+xoff]  
		    y = Memr[ev_p+yoff]

		    j = int ((y - vs[2]) / yblock + (EPSILOND * 1000))
		    if (j >= 0 && j < yw) {
			pix = j * xw + (x - vs[1]) / xblock + 1
			if (pix > 0 && pix <= maxpix)
			    obuf[pix] = obuf[pix] + 1
		    }
		}

	    case TY_DOUBLE:
		# Process a sequence of neighbor events.
		do i = 1, nev {
		    ev_p = (Memi[evl+i-1] - 1) * SZ_SHORT / SZ_DOUBLE + 1

		    x = Memd[ev_p+xoff]  
		    y = Memd[ev_p+yoff]

		    j = int ((y - vs[2]) / yblock + (EPSILOND * 1000))
		    if (j >= 0 && j < yw) {
			pix = j * xw + (x - vs[1]) / xblock + 1
			if (pix > 0 && pix <= maxpix)
			    obuf[pix] = obuf[pix] + 1
		    }
		}

	    }

	    totev = totev + nev
	}

	call sfree (sp)
	return (totev)
end
