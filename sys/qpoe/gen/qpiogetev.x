# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<pmset.h>
include	"../qpio.h"

define	RLI_NEXTLINE	9998
define	RLI_INITIALIZE	9999
define	SZ_CODE		7

# QPIO_GETEVENTS -- Return a sequence of events sharing the same mask value
# which satisfy the current event attribute filter.  The returned events will
# be only those in a rectangular subregion of the image (specified by a prior
# call to qpio_setrange) which are also visible through the current mask.
# Sequences of events are returned in storage order until the region is
# exhausted, at which time EOF is returned.
#
# NOTE - If debug statements (printfs) are placed in this code they will cause
# i/o problems at runtime due to reentrancy, since this routine is called in
# a low level FIO pseudodevice driver (QPF).  This is also true of any of the
# routines called by this procedure, and of the related routine QPIO_READPIX.

int procedure qpio_gvtevents (io, o_ev, maskval, maxev, o_nev)

pointer	io			#I QPIO descriptor
pointer	o_ev[maxev]		#O receives the event struct pointers
int	maskval			#O receives the mask value of the events
int	maxev			#I max events out
int	o_nev			#O same as function value (nev_out|EOF)

int	status
char	code[SZ_CODE]
int	qpx_gvs(), qpx_gvi(), qpx_gvl(), qpx_gvr(), qpx_gvd()
errchk	syserrs
define	err_ 91

begin
	# The generic routines currently require that X,Y be the same type.
	# It wouldn't be hard to remove this restriction if necessary, but
	# it simplifies things and I doubt if a mixed types feature would
	# be used very often.

	if (IO_EVXTYPE(io) != IO_EVYTYPE(io))
	    goto err_

	# Get the events.
	switch (IO_EVXTYPE(io)) {
	case TY_SHORT:
	    status = qpx_gvs (io, o_ev, maskval, maxev, o_nev)
	case TY_INT:
	    status = qpx_gvi (io, o_ev, maskval, maxev, o_nev)
	case TY_LONG:
	    status = qpx_gvl (io, o_ev, maskval, maxev, o_nev)
	case TY_REAL:
	    status = qpx_gvr (io, o_ev, maskval, maxev, o_nev)
	case TY_DOUBLE:
	    status = qpx_gvd (io, o_ev, maskval, maxev, o_nev)
	default:
err_	    call sprintf (code, SZ_CODE, "%d")
		call pargi (IO_EVXTYPE(io))
	    call syserrs (SYS_QPINVEVT, code)
	}

	return (status)
end




# QPX_GV -- Internal generic code for qpio_getevents.  There is one copy
# of this routine for each event coordinate datatype.  The optimization
# strategy used here assumes that executing qpio_gv is much more expensive
# than building the call in qpio_getevents.  This will normally be the case
# for a large event list or a complex expression, otherwise the operation
# is likely to be fast enough that it doesn't matter anyway.

int procedure qpx_gvs (io, o_ev, maskval, maxev, o_nev)

pointer	io			#I QPIO descriptor
pointer	o_ev[maxev]		#O receives the event struct pointers
int	maskval			#O receives the mask value of the events
int	maxev			#I max events out
int	o_nev			#O same as function value (nev_out|EOF)

int     x1, x2, y1, y2, xs, xe, ys, ye, x, y
pointer pl, rl, rp, bp, ex, ev, ev_p, bbmask, bb_bufp
bool    useindex, lineio, bbused, rmused, nodata
int     bb_xsize, bb_ysize, bb_xblock, bb_yblock, ii, jj
int     v[NDIM], szs_event, mval, nev, evidx, evtop, temp, i
int     ev_xoff, ev_yoff

pointer	plr_open()
bool	pl_linenotempty(), pl_sectnotempty()
int	qpio_rbucket(), qpex_evaluate(), btoi(), plr_getpix()

define	swap {temp=$1;$1=$2;$2=temp}
define	putevent_  91
define	again_     92
define	done_      93
define	exit_      94

begin
	pl = IO_PL(io)		# pixel list (region mask) descriptor
	rl = IO_RL(io)		# range list buffer
	bp = IO_BP(io)		# bucket buffer (type short)
	ex = IO_EX(io)		# QPEX (EAF) descriptor

	# The following is executed when the first i/o is performed on a new
	# region, to select the most efficient type of i/o to be performed,
	# and initialize the i/o parameters for that case.  The type of i/o
	# to be performed depends upon whether or not an index can be used,
	# and whether or not there is a region mask (RM) or bounding box (BB).
	# The presence or absence of an event attribute filter (EAF) is not
	# separated out as a special case, as it is quick and easy to test
	# for the presence of an EAF and apply one it if it exists.

	if (IO_ACTIVE(io) == NO) {
	    # Check for an index.  We have an index if the event list is
	    # indexed, and the index is defined on the Y-coordinate we will
	    # be using for extraction.

	    useindex = (IO_INDEXLEN(io) == IO_NLINES(io) &&
			IO_EVYOFF(io)   == IO_IXYOFF(io) &&
			IO_NOINDEX(io)  == NO)

	    # Initialize the V and VN vectors.
	    do i = 1, NDIM {
		IO_VN(io,i) = IO_VE(io,i) - IO_VS(io,i) + 1
		if (IO_VN(io,i) < 0) {
		    swap (IO_VS(io,i), IO_VE(io,i))
		    IO_VN(io,i) = -IO_VN(io,i)
		}
	    }
	    call amovi (IO_VS(io,1), IO_V(io,1), NDIM)

	    # Determine if full lines are to be accessed, and if a bounding
	    # box (subraster of the image) is defined.

	    lineio = (IO_VS(io,1) == 1 && IO_VE(io,1) == IO_NCOLS(io))
	    bbused = (!lineio || IO_VS(io,2) > 1 || IO_VE(io,2) < IO_NLINES(io))

	    # Determine if region mask data is to be used and if there is any
	    # data to be read.

	    nodata = (IO_NEVENTS(io) <= 0)
	    rmused = false

	    if (pl != NULL)
		if (pl_sectnotempty (pl, IO_VS(io,1), IO_VE(io,1), NDIM))
		    rmused = true
		else
		    nodata = true

	    # Select the optimal type of i/o to be used for extraction.
	    if (nodata) {
		IO_IOTYPE(io) = NoDATA_NoAREA
		useindex = false
		bbused = false

	    } else if (bbused || rmused) {
		if (useindex)
		    IO_IOTYPE(io) = INDEX_RMorBB
		else
		    IO_IOTYPE(io) = NoINDEX_RMorBB

	    } else {
		# If we are reading the entire image (no bounding box) and
		# we are not using a mask, then there is no point in using
		# indexed i/o.

		IO_IOTYPE(io) = NoINDEX_NoRMorBB
		useindex = false
	    }

	    # Initialize the range list data if it will be used.
	    if (useindex) {
		# Dummy range specifying full line segment.
		RLI_LEN(rl)   = RL_FIRST
		RLI_AXLEN(rl) = IO_NCOLS(io)

		rp = rl + ((RL_FIRST - 1) * RL_LENELEM)
		Memi[rp+RL_XOFF] = IO_VS(io,1)
		Memi[rp+RL_NOFF] = IO_VN(io,1)
		Memi[rp+RL_VOFF] = 1

		IO_RLI(io) = RLI_INITIALIZE
	    }

	    # Open the mask for random access if i/o is not indexed and
	    # a region mask is used.

	    bbmask = IO_BBMASK(io)
	    if (bbmask != NULL)
		call plr_close (bbmask)

	    if (IO_IOTYPE(io) == NoINDEX_RMorBB && rmused) {
		bbmask = plr_open (pl, v, 0)	# (v is never referenced)
		call plr_setrect (bbmask, IO_VS(io,1),IO_VS(io,2),
		    IO_VE(io,1),IO_VE(io,2))
		call plr_getlut (bbmask,
		    bb_bufp, bb_xsize, bb_ysize, bb_xblock, bb_yblock)
	    }

	    # Update the QPIO descriptor.
	    IO_LINEIO(io)   = btoi(lineio)
	    IO_RMUSED(io)   = btoi(rmused)
	    IO_BBUSED(io)   = btoi(bbused)
	    IO_BBMASK(io)   = bbmask

	    IO_EVI(io)      = 1
	    IO_BKNO(io)     = 0
	    IO_BKLASTEV(io) = 0

	    IO_ACTIVE(io) = YES
	}

	# Initialize event extraction parameters.
	szs_event = IO_EVENTLEN(io)
	maskval = 0
	nev = 0

	ev_xoff = IO_EVXOFF(io)
	ev_yoff = IO_EVYOFF(io)

	# Extract events using the most efficient type of i/o for the given
	# selection critera (index, mask, BB, EAF, etc.).
again_
	switch (IO_IOTYPE(io)) {
	case NoDATA_NoAREA:
	    # We know in advance that there are no events to be returned,
	    # either because there is no data, or the area of the region
	    # mask within the bounding box is empty.

	    goto exit_

	case NoINDEX_NoRMorBB:
	    # This is the simplest case; no index, region mask, or bounding
	    # box.  Read and output all events in sequence.

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Copy out the event pointers.
	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    nev = min (maxev, IO_BKLASTEV(io) - IO_EVI(io) + 1)

	    do i = 1, nev {
		o_ev[i] = ev
		ev = ev + szs_event
	    }

	    IO_EVI(io) = IO_EVI(io) + nev
	    maskval = 1

	case NoINDEX_RMorBB:
	    # Fully general selection, including any combination of bounding
	    # box, region mask, or EAF, but no index, either because there is
	    # no index for this event list, or the index is for a different Y
	    # attribute than the one being used for extraction.

	    bbused = (IO_BBUSED(io) == YES)
	    x1 = IO_VS(io,1);  x2 = IO_VE(io,1)
	    y1 = IO_VS(io,2);  y2 = IO_VE(io,2)

	    # Refill the event bucket?
	    while (IO_EVI(io) > IO_BKLASTEV(io)) {
		# Get the next bucket.
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

		# Reject buckets that do not contain any events lying
		# within the specified bounding box, if any.

		if (bbused) {
		    ev_p = (IO_MINEVB(io) - 1) * SZ_SHORT / SZ_SHORT + 1
			xs = Mems[ev_p+ev_xoff]
			ys = Mems[ev_p+ev_yoff]

		    ev_p = (IO_MAXEVB(io) - 1) * SZ_SHORT / SZ_SHORT + 1
			xe = Mems[ev_p+ev_xoff]
			ye = Mems[ev_p+ev_yoff]

		    if (xs > x2 || xe < x1 || ys > y2 || ye < y1)
			IO_EVI(io) = IO_BKLASTEV(io) + 1
		}
	    }

	    # Copy out any events which pass the region mask and which share
	    # the same mask value.  Note that in this case, to speed mask
	    # value lookup at random mask coordinates, the region mask for
	    # the bounding box is stored as a populated array in the QPIO
	    # descriptor.

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io) - 1) * szs_event
	    bbmask = IO_BBMASK(io)
	    mval = 0

	    do i = IO_EVI(io), IO_BKLASTEV(io) {
		# Get event x,y coordinates in whatever coord system.
		ev = ev + szs_event
		ev_p = (ev - 1) * SZ_SHORT / SZ_SHORT + 1

		    x = Mems[ev_p+ev_xoff]
		    y = Mems[ev_p+ev_yoff]

		# Reject events lying outside the bounding box.
		if (bbused)
		    if (x < x1 || x > x2 || y < y1 || y > y2)
			next

		# Take a shortcut if no region mask is in effect for this BB.
		if (bbmask == NULL)
		    goto putevent_

		# Get the mask pixel associated with this event.
		ii = (x - 1) / bb_xblock
		jj = (y - 1) / bb_yblock
		mval = Memi[bb_bufp + jj*bb_xsize + ii]
		if (mval < 0)
		    mval = plr_getpix (bbmask, x, y)

		# Accumulate points lying in the first nonzero mask range
		# encountered.

		if (mval != 0) {
		    if (maskval == 0)
			maskval = mval
		    if (mval == maskval) {
putevent_		if (nev >= maxev)
			    break
			nev = nev + 1
			o_ev[nev] = ev
		    } else
			break
		}
	    }

	    IO_EVI(io) = i

	case INDEX_NoRMorBB, INDEX_RMorBB:
	    # General extraction for indexed data.  Process successive ranges
	    # and range lists until we get at least one event which lies within
	    # the bounding box, within a range, and which passes the event
	    # attribute filter, if one is in use.

	    # If the current range list (mask line) has been exhausted, advance
	    # to the next line which contains both ranges and events.  A range
	    # list is used to specify the bounding box even if we don't have
	    # a nonempty region mask within the BB.

	    if (IO_RLI(io) > RLI_LEN(rl)) {
		repeat {
		    y = IO_V(io,2)
		    if (IO_RLI(io) == RLI_INITIALIZE)
			IO_RLI(io) = RL_FIRST
		    else
			y = y + 1

		    if (y > IO_VE(io,2)) {
			if (nev <= 0) {
			    o_nev = EOF
			    return (EOF)
			} else
			    goto done_
		    }

		    IO_V(io,2) = y
		    evidx = Memi[IO_YOFFVP(io)+y-1]

		    if (evidx > 0) {
			if (IO_RMUSED(io) == YES) {
			    if (IO_LINEIO(io) == YES) {
				if (!pl_linenotempty (pl,IO_V(io,1)))
				    next
			    } else {
				v[1] = IO_VE(io,1);  v[2] = y
				if (!pl_sectnotempty (pl,IO_V(io,1),v,NDIM))
				    next
			    }
			    call pl_glri (pl, IO_V(io,1), Memi[rl],
				IO_MDEPTH(io), IO_VN(io,1), PIX_SRC)
			}
			IO_RLI(io) = RL_FIRST
		    }
		} until (IO_RLI(io) <= RLI_LEN(rl))

		IO_EVI(io) = evidx
		IO_EV1(io) = evidx
		IO_EV2(io) = Memi[IO_YLENVP(io)+y-1] + evidx - 1
	    }

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Compute current range parameters and initialize event pointer.
	    rp = rl + (IO_RLI(io) - 1) * RL_LENELEM
	    x1 = Memi[rp+RL_XOFF]
	    x2 = x1 + Memi[rp+RL_NOFF] - 1
	    maskval = Memi[rp+RL_VOFF]

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    evtop = min (IO_EV2(io), IO_BKLASTEV(io))

	    # Extract events from bucket which lie within the current range
	    # of the current line.  This is the inner loop of indexed event
	    # extraction, ignoring event attribute filtering.

	    do i = IO_EVI(io), evtop {
		ev_p = (ev - 1) * SZ_SHORT / SZ_SHORT + 1
		    x = Mems[ev_p+ev_xoff]
		if (x >= x1) {
		    if (x > x2) {
			IO_RLI(io) = IO_RLI(io) + 1
			break
		    } else if (nev >= maxev)
			break
		    nev = nev + 1
		    o_ev[nev] = ev
		}
		ev = ev + szs_event
	    }

	    IO_EVI(io) = i
	    if (i > IO_EV2(io))
		IO_RLI(io) = RLI_NEXTLINE
	}
done_
	# Apply the event attribute filter if one is defined; repeat
	# the whole process if we don't end up with any events.

	if (nev > 0)
	    if (ex != NULL)
		nev = qpex_evaluate (ex, o_ev, o_ev, nev)
	if (nev <= 0)
	    goto again_
exit_
	o_nev = nev
	if (o_nev <= 0)
	    o_nev = EOF

	return (o_nev)
end



# QPX_GV -- Internal generic code for qpio_getevents.  There is one copy
# of this routine for each event coordinate datatype.  The optimization
# strategy used here assumes that executing qpio_gv is much more expensive
# than building the call in qpio_getevents.  This will normally be the case
# for a large event list or a complex expression, otherwise the operation
# is likely to be fast enough that it doesn't matter anyway.

int procedure qpx_gvi (io, o_ev, maskval, maxev, o_nev)

pointer	io			#I QPIO descriptor
pointer	o_ev[maxev]		#O receives the event struct pointers
int	maskval			#O receives the mask value of the events
int	maxev			#I max events out
int	o_nev			#O same as function value (nev_out|EOF)

int     x1, x2, y1, y2, xs, xe, ys, ye, x, y
pointer pl, rl, rp, bp, ex, ev, ev_p, bbmask, bb_bufp
bool    useindex, lineio, bbused, rmused, nodata
int     bb_xsize, bb_ysize, bb_xblock, bb_yblock, ii, jj
int     v[NDIM], szs_event, mval, nev, evidx, evtop, temp, i
int     ev_xoff, ev_yoff

pointer	plr_open()
bool	pl_linenotempty(), pl_sectnotempty()
int	qpio_rbucket(), qpex_evaluate(), btoi(), plr_getpix()

define	swap {temp=$1;$1=$2;$2=temp}
define	putevent_  91
define	again_     92
define	done_      93
define	exit_      94

begin
	pl = IO_PL(io)		# pixel list (region mask) descriptor
	rl = IO_RL(io)		# range list buffer
	bp = IO_BP(io)		# bucket buffer (type short)
	ex = IO_EX(io)		# QPEX (EAF) descriptor

	# The following is executed when the first i/o is performed on a new
	# region, to select the most efficient type of i/o to be performed,
	# and initialize the i/o parameters for that case.  The type of i/o
	# to be performed depends upon whether or not an index can be used,
	# and whether or not there is a region mask (RM) or bounding box (BB).
	# The presence or absence of an event attribute filter (EAF) is not
	# separated out as a special case, as it is quick and easy to test
	# for the presence of an EAF and apply one it if it exists.

	if (IO_ACTIVE(io) == NO) {
	    # Check for an index.  We have an index if the event list is
	    # indexed, and the index is defined on the Y-coordinate we will
	    # be using for extraction.

	    useindex = (IO_INDEXLEN(io) == IO_NLINES(io) &&
			IO_EVYOFF(io)   == IO_IXYOFF(io) &&
			IO_NOINDEX(io)  == NO)

	    # Initialize the V and VN vectors.
	    do i = 1, NDIM {
		IO_VN(io,i) = IO_VE(io,i) - IO_VS(io,i) + 1
		if (IO_VN(io,i) < 0) {
		    swap (IO_VS(io,i), IO_VE(io,i))
		    IO_VN(io,i) = -IO_VN(io,i)
		}
	    }
	    call amovi (IO_VS(io,1), IO_V(io,1), NDIM)

	    # Determine if full lines are to be accessed, and if a bounding
	    # box (subraster of the image) is defined.

	    lineio = (IO_VS(io,1) == 1 && IO_VE(io,1) == IO_NCOLS(io))
	    bbused = (!lineio || IO_VS(io,2) > 1 || IO_VE(io,2) < IO_NLINES(io))

	    # Determine if region mask data is to be used and if there is any
	    # data to be read.

	    nodata = (IO_NEVENTS(io) <= 0)
	    rmused = false

	    if (pl != NULL)
		if (pl_sectnotempty (pl, IO_VS(io,1), IO_VE(io,1), NDIM))
		    rmused = true
		else
		    nodata = true

	    # Select the optimal type of i/o to be used for extraction.
	    if (nodata) {
		IO_IOTYPE(io) = NoDATA_NoAREA
		useindex = false
		bbused = false

	    } else if (bbused || rmused) {
		if (useindex)
		    IO_IOTYPE(io) = INDEX_RMorBB
		else
		    IO_IOTYPE(io) = NoINDEX_RMorBB

	    } else {
		# If we are reading the entire image (no bounding box) and
		# we are not using a mask, then there is no point in using
		# indexed i/o.

		IO_IOTYPE(io) = NoINDEX_NoRMorBB
		useindex = false
	    }

	    # Initialize the range list data if it will be used.
	    if (useindex) {
		# Dummy range specifying full line segment.
		RLI_LEN(rl)   = RL_FIRST
		RLI_AXLEN(rl) = IO_NCOLS(io)

		rp = rl + ((RL_FIRST - 1) * RL_LENELEM)
		Memi[rp+RL_XOFF] = IO_VS(io,1)
		Memi[rp+RL_NOFF] = IO_VN(io,1)
		Memi[rp+RL_VOFF] = 1

		IO_RLI(io) = RLI_INITIALIZE
	    }

	    # Open the mask for random access if i/o is not indexed and
	    # a region mask is used.

	    bbmask = IO_BBMASK(io)
	    if (bbmask != NULL)
		call plr_close (bbmask)

	    if (IO_IOTYPE(io) == NoINDEX_RMorBB && rmused) {
		bbmask = plr_open (pl, v, 0)	# (v is never referenced)
		call plr_setrect (bbmask, IO_VS(io,1),IO_VS(io,2),
		    IO_VE(io,1),IO_VE(io,2))
		call plr_getlut (bbmask,
		    bb_bufp, bb_xsize, bb_ysize, bb_xblock, bb_yblock)
	    }

	    # Update the QPIO descriptor.
	    IO_LINEIO(io)   = btoi(lineio)
	    IO_RMUSED(io)   = btoi(rmused)
	    IO_BBUSED(io)   = btoi(bbused)
	    IO_BBMASK(io)   = bbmask

	    IO_EVI(io)      = 1
	    IO_BKNO(io)     = 0
	    IO_BKLASTEV(io) = 0

	    IO_ACTIVE(io) = YES
	}

	# Initialize event extraction parameters.
	szs_event = IO_EVENTLEN(io)
	maskval = 0
	nev = 0

	ev_xoff = IO_EVXOFF(io)
	ev_yoff = IO_EVYOFF(io)

	# Extract events using the most efficient type of i/o for the given
	# selection critera (index, mask, BB, EAF, etc.).
again_
	switch (IO_IOTYPE(io)) {
	case NoDATA_NoAREA:
	    # We know in advance that there are no events to be returned,
	    # either because there is no data, or the area of the region
	    # mask within the bounding box is empty.

	    goto exit_

	case NoINDEX_NoRMorBB:
	    # This is the simplest case; no index, region mask, or bounding
	    # box.  Read and output all events in sequence.

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Copy out the event pointers.
	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    nev = min (maxev, IO_BKLASTEV(io) - IO_EVI(io) + 1)

	    do i = 1, nev {
		o_ev[i] = ev
		ev = ev + szs_event
	    }

	    IO_EVI(io) = IO_EVI(io) + nev
	    maskval = 1

	case NoINDEX_RMorBB:
	    # Fully general selection, including any combination of bounding
	    # box, region mask, or EAF, but no index, either because there is
	    # no index for this event list, or the index is for a different Y
	    # attribute than the one being used for extraction.

	    bbused = (IO_BBUSED(io) == YES)
	    x1 = IO_VS(io,1);  x2 = IO_VE(io,1)
	    y1 = IO_VS(io,2);  y2 = IO_VE(io,2)

	    # Refill the event bucket?
	    while (IO_EVI(io) > IO_BKLASTEV(io)) {
		# Get the next bucket.
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

		# Reject buckets that do not contain any events lying
		# within the specified bounding box, if any.

		if (bbused) {
		    ev_p = (IO_MINEVB(io) - 1) * SZ_SHORT / SZ_INT + 1
			xs = Memi[ev_p+ev_xoff]
			ys = Memi[ev_p+ev_yoff]

		    ev_p = (IO_MAXEVB(io) - 1) * SZ_SHORT / SZ_INT + 1
			xe = Memi[ev_p+ev_xoff]
			ye = Memi[ev_p+ev_yoff]

		    if (xs > x2 || xe < x1 || ys > y2 || ye < y1)
			IO_EVI(io) = IO_BKLASTEV(io) + 1
		}
	    }

	    # Copy out any events which pass the region mask and which share
	    # the same mask value.  Note that in this case, to speed mask
	    # value lookup at random mask coordinates, the region mask for
	    # the bounding box is stored as a populated array in the QPIO
	    # descriptor.

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io) - 1) * szs_event
	    bbmask = IO_BBMASK(io)
	    mval = 0

	    do i = IO_EVI(io), IO_BKLASTEV(io) {
		# Get event x,y coordinates in whatever coord system.
		ev = ev + szs_event
		ev_p = (ev - 1) * SZ_SHORT / SZ_INT + 1

		    x = Memi[ev_p+ev_xoff]
		    y = Memi[ev_p+ev_yoff]

		# Reject events lying outside the bounding box.
		if (bbused)
		    if (x < x1 || x > x2 || y < y1 || y > y2)
			next

		# Take a shortcut if no region mask is in effect for this BB.
		if (bbmask == NULL)
		    goto putevent_

		# Get the mask pixel associated with this event.
		ii = (x - 1) / bb_xblock
		jj = (y - 1) / bb_yblock
		mval = Memi[bb_bufp + jj*bb_xsize + ii]
		if (mval < 0)
		    mval = plr_getpix (bbmask, x, y)

		# Accumulate points lying in the first nonzero mask range
		# encountered.

		if (mval != 0) {
		    if (maskval == 0)
			maskval = mval
		    if (mval == maskval) {
putevent_		if (nev >= maxev)
			    break
			nev = nev + 1
			o_ev[nev] = ev
		    } else
			break
		}
	    }

	    IO_EVI(io) = i

	case INDEX_NoRMorBB, INDEX_RMorBB:
	    # General extraction for indexed data.  Process successive ranges
	    # and range lists until we get at least one event which lies within
	    # the bounding box, within a range, and which passes the event
	    # attribute filter, if one is in use.

	    # If the current range list (mask line) has been exhausted, advance
	    # to the next line which contains both ranges and events.  A range
	    # list is used to specify the bounding box even if we don't have
	    # a nonempty region mask within the BB.

	    if (IO_RLI(io) > RLI_LEN(rl)) {
		repeat {
		    y = IO_V(io,2)
		    if (IO_RLI(io) == RLI_INITIALIZE)
			IO_RLI(io) = RL_FIRST
		    else
			y = y + 1

		    if (y > IO_VE(io,2)) {
			if (nev <= 0) {
			    o_nev = EOF
			    return (EOF)
			} else
			    goto done_
		    }

		    IO_V(io,2) = y
		    evidx = Memi[IO_YOFFVP(io)+y-1]

		    if (evidx > 0) {
			if (IO_RMUSED(io) == YES) {
			    if (IO_LINEIO(io) == YES) {
				if (!pl_linenotempty (pl,IO_V(io,1)))
				    next
			    } else {
				v[1] = IO_VE(io,1);  v[2] = y
				if (!pl_sectnotempty (pl,IO_V(io,1),v,NDIM))
				    next
			    }
			    call pl_glri (pl, IO_V(io,1), Memi[rl],
				IO_MDEPTH(io), IO_VN(io,1), PIX_SRC)
			}
			IO_RLI(io) = RL_FIRST
		    }
		} until (IO_RLI(io) <= RLI_LEN(rl))

		IO_EVI(io) = evidx
		IO_EV1(io) = evidx
		IO_EV2(io) = Memi[IO_YLENVP(io)+y-1] + evidx - 1
	    }

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Compute current range parameters and initialize event pointer.
	    rp = rl + (IO_RLI(io) - 1) * RL_LENELEM
	    x1 = Memi[rp+RL_XOFF]
	    x2 = x1 + Memi[rp+RL_NOFF] - 1
	    maskval = Memi[rp+RL_VOFF]

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    evtop = min (IO_EV2(io), IO_BKLASTEV(io))

	    # Extract events from bucket which lie within the current range
	    # of the current line.  This is the inner loop of indexed event
	    # extraction, ignoring event attribute filtering.

	    do i = IO_EVI(io), evtop {
		ev_p = (ev - 1) * SZ_SHORT / SZ_INT + 1
		    x = Memi[ev_p+ev_xoff]
		if (x >= x1) {
		    if (x > x2) {
			IO_RLI(io) = IO_RLI(io) + 1
			break
		    } else if (nev >= maxev)
			break
		    nev = nev + 1
		    o_ev[nev] = ev
		}
		ev = ev + szs_event
	    }

	    IO_EVI(io) = i
	    if (i > IO_EV2(io))
		IO_RLI(io) = RLI_NEXTLINE
	}
done_
	# Apply the event attribute filter if one is defined; repeat
	# the whole process if we don't end up with any events.

	if (nev > 0)
	    if (ex != NULL)
		nev = qpex_evaluate (ex, o_ev, o_ev, nev)
	if (nev <= 0)
	    goto again_
exit_
	o_nev = nev
	if (o_nev <= 0)
	    o_nev = EOF

	return (o_nev)
end



# QPX_GV -- Internal generic code for qpio_getevents.  There is one copy
# of this routine for each event coordinate datatype.  The optimization
# strategy used here assumes that executing qpio_gv is much more expensive
# than building the call in qpio_getevents.  This will normally be the case
# for a large event list or a complex expression, otherwise the operation
# is likely to be fast enough that it doesn't matter anyway.

int procedure qpx_gvl (io, o_ev, maskval, maxev, o_nev)

pointer	io			#I QPIO descriptor
pointer	o_ev[maxev]		#O receives the event struct pointers
int	maskval			#O receives the mask value of the events
int	maxev			#I max events out
int	o_nev			#O same as function value (nev_out|EOF)

int     x1, x2, y1, y2, xs, xe, ys, ye, x, y
pointer pl, rl, rp, bp, ex, ev, ev_p, bbmask, bb_bufp
bool    useindex, lineio, bbused, rmused, nodata
int     bb_xsize, bb_ysize, bb_xblock, bb_yblock, ii, jj
int     v[NDIM], szs_event, mval, nev, evidx, evtop, temp, i
int     ev_xoff, ev_yoff

pointer	plr_open()
bool	pl_linenotempty(), pl_sectnotempty()
int	qpio_rbucket(), qpex_evaluate(), btoi(), plr_getpix()

define	swap {temp=$1;$1=$2;$2=temp}
define	putevent_  91
define	again_     92
define	done_      93
define	exit_      94

begin
	pl = IO_PL(io)		# pixel list (region mask) descriptor
	rl = IO_RL(io)		# range list buffer
	bp = IO_BP(io)		# bucket buffer (type short)
	ex = IO_EX(io)		# QPEX (EAF) descriptor

	# The following is executed when the first i/o is performed on a new
	# region, to select the most efficient type of i/o to be performed,
	# and initialize the i/o parameters for that case.  The type of i/o
	# to be performed depends upon whether or not an index can be used,
	# and whether or not there is a region mask (RM) or bounding box (BB).
	# The presence or absence of an event attribute filter (EAF) is not
	# separated out as a special case, as it is quick and easy to test
	# for the presence of an EAF and apply one it if it exists.

	if (IO_ACTIVE(io) == NO) {
	    # Check for an index.  We have an index if the event list is
	    # indexed, and the index is defined on the Y-coordinate we will
	    # be using for extraction.

	    useindex = (IO_INDEXLEN(io) == IO_NLINES(io) &&
			IO_EVYOFF(io)   == IO_IXYOFF(io) &&
			IO_NOINDEX(io)  == NO)

	    # Initialize the V and VN vectors.
	    do i = 1, NDIM {
		IO_VN(io,i) = IO_VE(io,i) - IO_VS(io,i) + 1
		if (IO_VN(io,i) < 0) {
		    swap (IO_VS(io,i), IO_VE(io,i))
		    IO_VN(io,i) = -IO_VN(io,i)
		}
	    }
	    call amovi (IO_VS(io,1), IO_V(io,1), NDIM)

	    # Determine if full lines are to be accessed, and if a bounding
	    # box (subraster of the image) is defined.

	    lineio = (IO_VS(io,1) == 1 && IO_VE(io,1) == IO_NCOLS(io))
	    bbused = (!lineio || IO_VS(io,2) > 1 || IO_VE(io,2) < IO_NLINES(io))

	    # Determine if region mask data is to be used and if there is any
	    # data to be read.

	    nodata = (IO_NEVENTS(io) <= 0)
	    rmused = false

	    if (pl != NULL)
		if (pl_sectnotempty (pl, IO_VS(io,1), IO_VE(io,1), NDIM))
		    rmused = true
		else
		    nodata = true

	    # Select the optimal type of i/o to be used for extraction.
	    if (nodata) {
		IO_IOTYPE(io) = NoDATA_NoAREA
		useindex = false
		bbused = false

	    } else if (bbused || rmused) {
		if (useindex)
		    IO_IOTYPE(io) = INDEX_RMorBB
		else
		    IO_IOTYPE(io) = NoINDEX_RMorBB

	    } else {
		# If we are reading the entire image (no bounding box) and
		# we are not using a mask, then there is no point in using
		# indexed i/o.

		IO_IOTYPE(io) = NoINDEX_NoRMorBB
		useindex = false
	    }

	    # Initialize the range list data if it will be used.
	    if (useindex) {
		# Dummy range specifying full line segment.
		RLI_LEN(rl)   = RL_FIRST
		RLI_AXLEN(rl) = IO_NCOLS(io)

		rp = rl + ((RL_FIRST - 1) * RL_LENELEM)
		Memi[rp+RL_XOFF] = IO_VS(io,1)
		Memi[rp+RL_NOFF] = IO_VN(io,1)
		Memi[rp+RL_VOFF] = 1

		IO_RLI(io) = RLI_INITIALIZE
	    }

	    # Open the mask for random access if i/o is not indexed and
	    # a region mask is used.

	    bbmask = IO_BBMASK(io)
	    if (bbmask != NULL)
		call plr_close (bbmask)

	    if (IO_IOTYPE(io) == NoINDEX_RMorBB && rmused) {
		bbmask = plr_open (pl, v, 0)	# (v is never referenced)
		call plr_setrect (bbmask, IO_VS(io,1),IO_VS(io,2),
		    IO_VE(io,1),IO_VE(io,2))
		call plr_getlut (bbmask,
		    bb_bufp, bb_xsize, bb_ysize, bb_xblock, bb_yblock)
	    }

	    # Update the QPIO descriptor.
	    IO_LINEIO(io)   = btoi(lineio)
	    IO_RMUSED(io)   = btoi(rmused)
	    IO_BBUSED(io)   = btoi(bbused)
	    IO_BBMASK(io)   = bbmask

	    IO_EVI(io)      = 1
	    IO_BKNO(io)     = 0
	    IO_BKLASTEV(io) = 0

	    IO_ACTIVE(io) = YES
	}

	# Initialize event extraction parameters.
	szs_event = IO_EVENTLEN(io)
	maskval = 0
	nev = 0

	ev_xoff = IO_EVXOFF(io)
	ev_yoff = IO_EVYOFF(io)

	# Extract events using the most efficient type of i/o for the given
	# selection critera (index, mask, BB, EAF, etc.).
again_
	switch (IO_IOTYPE(io)) {
	case NoDATA_NoAREA:
	    # We know in advance that there are no events to be returned,
	    # either because there is no data, or the area of the region
	    # mask within the bounding box is empty.

	    goto exit_

	case NoINDEX_NoRMorBB:
	    # This is the simplest case; no index, region mask, or bounding
	    # box.  Read and output all events in sequence.

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Copy out the event pointers.
	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    nev = min (maxev, IO_BKLASTEV(io) - IO_EVI(io) + 1)

	    do i = 1, nev {
		o_ev[i] = ev
		ev = ev + szs_event
	    }

	    IO_EVI(io) = IO_EVI(io) + nev
	    maskval = 1

	case NoINDEX_RMorBB:
	    # Fully general selection, including any combination of bounding
	    # box, region mask, or EAF, but no index, either because there is
	    # no index for this event list, or the index is for a different Y
	    # attribute than the one being used for extraction.

	    bbused = (IO_BBUSED(io) == YES)
	    x1 = IO_VS(io,1);  x2 = IO_VE(io,1)
	    y1 = IO_VS(io,2);  y2 = IO_VE(io,2)

	    # Refill the event bucket?
	    while (IO_EVI(io) > IO_BKLASTEV(io)) {
		# Get the next bucket.
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

		# Reject buckets that do not contain any events lying
		# within the specified bounding box, if any.

		if (bbused) {
		    ev_p = (IO_MINEVB(io) - 1) * SZ_SHORT / SZ_LONG + 1
			xs = Meml[ev_p+ev_xoff]
			ys = Meml[ev_p+ev_yoff]

		    ev_p = (IO_MAXEVB(io) - 1) * SZ_SHORT / SZ_LONG + 1
			xe = Meml[ev_p+ev_xoff]
			ye = Meml[ev_p+ev_yoff]

		    if (xs > x2 || xe < x1 || ys > y2 || ye < y1)
			IO_EVI(io) = IO_BKLASTEV(io) + 1
		}
	    }

	    # Copy out any events which pass the region mask and which share
	    # the same mask value.  Note that in this case, to speed mask
	    # value lookup at random mask coordinates, the region mask for
	    # the bounding box is stored as a populated array in the QPIO
	    # descriptor.

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io) - 1) * szs_event
	    bbmask = IO_BBMASK(io)
	    mval = 0

	    do i = IO_EVI(io), IO_BKLASTEV(io) {
		# Get event x,y coordinates in whatever coord system.
		ev = ev + szs_event
		ev_p = (ev - 1) * SZ_SHORT / SZ_LONG + 1

		    x = Meml[ev_p+ev_xoff]
		    y = Meml[ev_p+ev_yoff]

		# Reject events lying outside the bounding box.
		if (bbused)
		    if (x < x1 || x > x2 || y < y1 || y > y2)
			next

		# Take a shortcut if no region mask is in effect for this BB.
		if (bbmask == NULL)
		    goto putevent_

		# Get the mask pixel associated with this event.
		ii = (x - 1) / bb_xblock
		jj = (y - 1) / bb_yblock
		mval = Memi[bb_bufp + jj*bb_xsize + ii]
		if (mval < 0)
		    mval = plr_getpix (bbmask, x, y)

		# Accumulate points lying in the first nonzero mask range
		# encountered.

		if (mval != 0) {
		    if (maskval == 0)
			maskval = mval
		    if (mval == maskval) {
putevent_		if (nev >= maxev)
			    break
			nev = nev + 1
			o_ev[nev] = ev
		    } else
			break
		}
	    }

	    IO_EVI(io) = i

	case INDEX_NoRMorBB, INDEX_RMorBB:
	    # General extraction for indexed data.  Process successive ranges
	    # and range lists until we get at least one event which lies within
	    # the bounding box, within a range, and which passes the event
	    # attribute filter, if one is in use.

	    # If the current range list (mask line) has been exhausted, advance
	    # to the next line which contains both ranges and events.  A range
	    # list is used to specify the bounding box even if we don't have
	    # a nonempty region mask within the BB.

	    if (IO_RLI(io) > RLI_LEN(rl)) {
		repeat {
		    y = IO_V(io,2)
		    if (IO_RLI(io) == RLI_INITIALIZE)
			IO_RLI(io) = RL_FIRST
		    else
			y = y + 1

		    if (y > IO_VE(io,2)) {
			if (nev <= 0) {
			    o_nev = EOF
			    return (EOF)
			} else
			    goto done_
		    }

		    IO_V(io,2) = y
		    evidx = Memi[IO_YOFFVP(io)+y-1]

		    if (evidx > 0) {
			if (IO_RMUSED(io) == YES) {
			    if (IO_LINEIO(io) == YES) {
				if (!pl_linenotempty (pl,IO_V(io,1)))
				    next
			    } else {
				v[1] = IO_VE(io,1);  v[2] = y
				if (!pl_sectnotempty (pl,IO_V(io,1),v,NDIM))
				    next
			    }
			    call pl_glri (pl, IO_V(io,1), Memi[rl],
				IO_MDEPTH(io), IO_VN(io,1), PIX_SRC)
			}
			IO_RLI(io) = RL_FIRST
		    }
		} until (IO_RLI(io) <= RLI_LEN(rl))

		IO_EVI(io) = evidx
		IO_EV1(io) = evidx
		IO_EV2(io) = Memi[IO_YLENVP(io)+y-1] + evidx - 1
	    }

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Compute current range parameters and initialize event pointer.
	    rp = rl + (IO_RLI(io) - 1) * RL_LENELEM
	    x1 = Memi[rp+RL_XOFF]
	    x2 = x1 + Memi[rp+RL_NOFF] - 1
	    maskval = Memi[rp+RL_VOFF]

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    evtop = min (IO_EV2(io), IO_BKLASTEV(io))

	    # Extract events from bucket which lie within the current range
	    # of the current line.  This is the inner loop of indexed event
	    # extraction, ignoring event attribute filtering.

	    do i = IO_EVI(io), evtop {
		ev_p = (ev - 1) * SZ_SHORT / SZ_LONG + 1
		    x = Meml[ev_p+ev_xoff]
		if (x >= x1) {
		    if (x > x2) {
			IO_RLI(io) = IO_RLI(io) + 1
			break
		    } else if (nev >= maxev)
			break
		    nev = nev + 1
		    o_ev[nev] = ev
		}
		ev = ev + szs_event
	    }

	    IO_EVI(io) = i
	    if (i > IO_EV2(io))
		IO_RLI(io) = RLI_NEXTLINE
	}
done_
	# Apply the event attribute filter if one is defined; repeat
	# the whole process if we don't end up with any events.

	if (nev > 0)
	    if (ex != NULL)
		nev = qpex_evaluate (ex, o_ev, o_ev, nev)
	if (nev <= 0)
	    goto again_
exit_
	o_nev = nev
	if (o_nev <= 0)
	    o_nev = EOF

	return (o_nev)
end



# QPX_GV -- Internal generic code for qpio_getevents.  There is one copy
# of this routine for each event coordinate datatype.  The optimization
# strategy used here assumes that executing qpio_gv is much more expensive
# than building the call in qpio_getevents.  This will normally be the case
# for a large event list or a complex expression, otherwise the operation
# is likely to be fast enough that it doesn't matter anyway.

int procedure qpx_gvr (io, o_ev, maskval, maxev, o_nev)

pointer	io			#I QPIO descriptor
pointer	o_ev[maxev]		#O receives the event struct pointers
int	maskval			#O receives the mask value of the events
int	maxev			#I max events out
int	o_nev			#O same as function value (nev_out|EOF)

int     x1, x2, y1, y2, xs, xe, ys, ye, x, y
pointer pl, rl, rp, bp, ex, ev, ev_p, bbmask, bb_bufp
bool    useindex, lineio, bbused, rmused, nodata
int     bb_xsize, bb_ysize, bb_xblock, bb_yblock, ii, jj
int     v[NDIM], szs_event, mval, nev, evidx, evtop, temp, i
int     ev_xoff, ev_yoff

pointer	plr_open()
bool	pl_linenotempty(), pl_sectnotempty()
int	qpio_rbucket(), qpex_evaluate(), btoi(), plr_getpix()

define	swap {temp=$1;$1=$2;$2=temp}
define	putevent_  91
define	again_     92
define	done_      93
define	exit_      94

begin
	pl = IO_PL(io)		# pixel list (region mask) descriptor
	rl = IO_RL(io)		# range list buffer
	bp = IO_BP(io)		# bucket buffer (type short)
	ex = IO_EX(io)		# QPEX (EAF) descriptor

	# The following is executed when the first i/o is performed on a new
	# region, to select the most efficient type of i/o to be performed,
	# and initialize the i/o parameters for that case.  The type of i/o
	# to be performed depends upon whether or not an index can be used,
	# and whether or not there is a region mask (RM) or bounding box (BB).
	# The presence or absence of an event attribute filter (EAF) is not
	# separated out as a special case, as it is quick and easy to test
	# for the presence of an EAF and apply one it if it exists.

	if (IO_ACTIVE(io) == NO) {
	    # Check for an index.  We have an index if the event list is
	    # indexed, and the index is defined on the Y-coordinate we will
	    # be using for extraction.

	    useindex = (IO_INDEXLEN(io) == IO_NLINES(io) &&
			IO_EVYOFF(io)   == IO_IXYOFF(io) &&
			IO_NOINDEX(io)  == NO)

	    # Initialize the V and VN vectors.
	    do i = 1, NDIM {
		IO_VN(io,i) = IO_VE(io,i) - IO_VS(io,i) + 1
		if (IO_VN(io,i) < 0) {
		    swap (IO_VS(io,i), IO_VE(io,i))
		    IO_VN(io,i) = -IO_VN(io,i)
		}
	    }
	    call amovi (IO_VS(io,1), IO_V(io,1), NDIM)

	    # Determine if full lines are to be accessed, and if a bounding
	    # box (subraster of the image) is defined.

	    lineio = (IO_VS(io,1) == 1 && IO_VE(io,1) == IO_NCOLS(io))
	    bbused = (!lineio || IO_VS(io,2) > 1 || IO_VE(io,2) < IO_NLINES(io))

	    # Determine if region mask data is to be used and if there is any
	    # data to be read.

	    nodata = (IO_NEVENTS(io) <= 0)
	    rmused = false

	    if (pl != NULL)
		if (pl_sectnotempty (pl, IO_VS(io,1), IO_VE(io,1), NDIM))
		    rmused = true
		else
		    nodata = true

	    # Select the optimal type of i/o to be used for extraction.
	    if (nodata) {
		IO_IOTYPE(io) = NoDATA_NoAREA
		useindex = false
		bbused = false

	    } else if (bbused || rmused) {
		if (useindex)
		    IO_IOTYPE(io) = INDEX_RMorBB
		else
		    IO_IOTYPE(io) = NoINDEX_RMorBB

	    } else {
		# If we are reading the entire image (no bounding box) and
		# we are not using a mask, then there is no point in using
		# indexed i/o.

		IO_IOTYPE(io) = NoINDEX_NoRMorBB
		useindex = false
	    }

	    # Initialize the range list data if it will be used.
	    if (useindex) {
		# Dummy range specifying full line segment.
		RLI_LEN(rl)   = RL_FIRST
		RLI_AXLEN(rl) = IO_NCOLS(io)

		rp = rl + ((RL_FIRST - 1) * RL_LENELEM)
		Memi[rp+RL_XOFF] = IO_VS(io,1)
		Memi[rp+RL_NOFF] = IO_VN(io,1)
		Memi[rp+RL_VOFF] = 1

		IO_RLI(io) = RLI_INITIALIZE
	    }

	    # Open the mask for random access if i/o is not indexed and
	    # a region mask is used.

	    bbmask = IO_BBMASK(io)
	    if (bbmask != NULL)
		call plr_close (bbmask)

	    if (IO_IOTYPE(io) == NoINDEX_RMorBB && rmused) {
		bbmask = plr_open (pl, v, 0)	# (v is never referenced)
		call plr_setrect (bbmask, IO_VS(io,1),IO_VS(io,2),
		    IO_VE(io,1),IO_VE(io,2))
		call plr_getlut (bbmask,
		    bb_bufp, bb_xsize, bb_ysize, bb_xblock, bb_yblock)
	    }

	    # Update the QPIO descriptor.
	    IO_LINEIO(io)   = btoi(lineio)
	    IO_RMUSED(io)   = btoi(rmused)
	    IO_BBUSED(io)   = btoi(bbused)
	    IO_BBMASK(io)   = bbmask

	    IO_EVI(io)      = 1
	    IO_BKNO(io)     = 0
	    IO_BKLASTEV(io) = 0

	    IO_ACTIVE(io) = YES
	}

	# Initialize event extraction parameters.
	szs_event = IO_EVENTLEN(io)
	maskval = 0
	nev = 0

	ev_xoff = IO_EVXOFF(io)
	ev_yoff = IO_EVYOFF(io)

	# Extract events using the most efficient type of i/o for the given
	# selection critera (index, mask, BB, EAF, etc.).
again_
	switch (IO_IOTYPE(io)) {
	case NoDATA_NoAREA:
	    # We know in advance that there are no events to be returned,
	    # either because there is no data, or the area of the region
	    # mask within the bounding box is empty.

	    goto exit_

	case NoINDEX_NoRMorBB:
	    # This is the simplest case; no index, region mask, or bounding
	    # box.  Read and output all events in sequence.

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Copy out the event pointers.
	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    nev = min (maxev, IO_BKLASTEV(io) - IO_EVI(io) + 1)

	    do i = 1, nev {
		o_ev[i] = ev
		ev = ev + szs_event
	    }

	    IO_EVI(io) = IO_EVI(io) + nev
	    maskval = 1

	case NoINDEX_RMorBB:
	    # Fully general selection, including any combination of bounding
	    # box, region mask, or EAF, but no index, either because there is
	    # no index for this event list, or the index is for a different Y
	    # attribute than the one being used for extraction.

	    bbused = (IO_BBUSED(io) == YES)
	    x1 = IO_VS(io,1);  x2 = IO_VE(io,1)
	    y1 = IO_VS(io,2);  y2 = IO_VE(io,2)

	    # Refill the event bucket?
	    while (IO_EVI(io) > IO_BKLASTEV(io)) {
		# Get the next bucket.
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

		# Reject buckets that do not contain any events lying
		# within the specified bounding box, if any.

		if (bbused) {
		    ev_p = (IO_MINEVB(io) - 1) * SZ_SHORT / SZ_REAL + 1
			xs = Memr[ev_p+ev_xoff] + 0.5
			ys = Memr[ev_p+ev_yoff] + 0.5

		    ev_p = (IO_MAXEVB(io) - 1) * SZ_SHORT / SZ_REAL + 1
			xe = Memr[ev_p+ev_xoff] + 0.5
			ye = Memr[ev_p+ev_yoff] + 0.5

		    if (xs > x2 || xe < x1 || ys > y2 || ye < y1)
			IO_EVI(io) = IO_BKLASTEV(io) + 1
		}
	    }

	    # Copy out any events which pass the region mask and which share
	    # the same mask value.  Note that in this case, to speed mask
	    # value lookup at random mask coordinates, the region mask for
	    # the bounding box is stored as a populated array in the QPIO
	    # descriptor.

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io) - 1) * szs_event
	    bbmask = IO_BBMASK(io)
	    mval = 0

	    do i = IO_EVI(io), IO_BKLASTEV(io) {
		# Get event x,y coordinates in whatever coord system.
		ev = ev + szs_event
		ev_p = (ev - 1) * SZ_SHORT / SZ_REAL + 1

		    x = Memr[ev_p+ev_xoff] + 0.5
		    y = Memr[ev_p+ev_yoff] + 0.5

		# Reject events lying outside the bounding box.
		if (bbused)
		    if (x < x1 || x > x2 || y < y1 || y > y2)
			next

		# Take a shortcut if no region mask is in effect for this BB.
		if (bbmask == NULL)
		    goto putevent_

		# Get the mask pixel associated with this event.
		ii = (x - 1) / bb_xblock
		jj = (y - 1) / bb_yblock
		mval = Memi[bb_bufp + jj*bb_xsize + ii]
		if (mval < 0)
		    mval = plr_getpix (bbmask, x, y)

		# Accumulate points lying in the first nonzero mask range
		# encountered.

		if (mval != 0) {
		    if (maskval == 0)
			maskval = mval
		    if (mval == maskval) {
putevent_		if (nev >= maxev)
			    break
			nev = nev + 1
			o_ev[nev] = ev
		    } else
			break
		}
	    }

	    IO_EVI(io) = i

	case INDEX_NoRMorBB, INDEX_RMorBB:
	    # General extraction for indexed data.  Process successive ranges
	    # and range lists until we get at least one event which lies within
	    # the bounding box, within a range, and which passes the event
	    # attribute filter, if one is in use.

	    # If the current range list (mask line) has been exhausted, advance
	    # to the next line which contains both ranges and events.  A range
	    # list is used to specify the bounding box even if we don't have
	    # a nonempty region mask within the BB.

	    if (IO_RLI(io) > RLI_LEN(rl)) {
		repeat {
		    y = IO_V(io,2)
		    if (IO_RLI(io) == RLI_INITIALIZE)
			IO_RLI(io) = RL_FIRST
		    else
			y = y + 1

		    if (y > IO_VE(io,2)) {
			if (nev <= 0) {
			    o_nev = EOF
			    return (EOF)
			} else
			    goto done_
		    }

		    IO_V(io,2) = y
		    evidx = Memi[IO_YOFFVP(io)+y-1]

		    if (evidx > 0) {
			if (IO_RMUSED(io) == YES) {
			    if (IO_LINEIO(io) == YES) {
				if (!pl_linenotempty (pl,IO_V(io,1)))
				    next
			    } else {
				v[1] = IO_VE(io,1);  v[2] = y
				if (!pl_sectnotempty (pl,IO_V(io,1),v,NDIM))
				    next
			    }
			    call pl_glri (pl, IO_V(io,1), Memi[rl],
				IO_MDEPTH(io), IO_VN(io,1), PIX_SRC)
			}
			IO_RLI(io) = RL_FIRST
		    }
		} until (IO_RLI(io) <= RLI_LEN(rl))

		IO_EVI(io) = evidx
		IO_EV1(io) = evidx
		IO_EV2(io) = Memi[IO_YLENVP(io)+y-1] + evidx - 1
	    }

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Compute current range parameters and initialize event pointer.
	    rp = rl + (IO_RLI(io) - 1) * RL_LENELEM
	    x1 = Memi[rp+RL_XOFF]
	    x2 = x1 + Memi[rp+RL_NOFF] - 1
	    maskval = Memi[rp+RL_VOFF]

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    evtop = min (IO_EV2(io), IO_BKLASTEV(io))

	    # Extract events from bucket which lie within the current range
	    # of the current line.  This is the inner loop of indexed event
	    # extraction, ignoring event attribute filtering.

	    do i = IO_EVI(io), evtop {
		ev_p = (ev - 1) * SZ_SHORT / SZ_REAL + 1
		    x = Memr[ev_p+ev_xoff] + 0.5
		if (x >= x1) {
		    if (x > x2) {
			IO_RLI(io) = IO_RLI(io) + 1
			break
		    } else if (nev >= maxev)
			break
		    nev = nev + 1
		    o_ev[nev] = ev
		}
		ev = ev + szs_event
	    }

	    IO_EVI(io) = i
	    if (i > IO_EV2(io))
		IO_RLI(io) = RLI_NEXTLINE
	}
done_
	# Apply the event attribute filter if one is defined; repeat
	# the whole process if we don't end up with any events.

	if (nev > 0)
	    if (ex != NULL)
		nev = qpex_evaluate (ex, o_ev, o_ev, nev)
	if (nev <= 0)
	    goto again_
exit_
	o_nev = nev
	if (o_nev <= 0)
	    o_nev = EOF

	return (o_nev)
end



# QPX_GV -- Internal generic code for qpio_getevents.  There is one copy
# of this routine for each event coordinate datatype.  The optimization
# strategy used here assumes that executing qpio_gv is much more expensive
# than building the call in qpio_getevents.  This will normally be the case
# for a large event list or a complex expression, otherwise the operation
# is likely to be fast enough that it doesn't matter anyway.

int procedure qpx_gvd (io, o_ev, maskval, maxev, o_nev)

pointer	io			#I QPIO descriptor
pointer	o_ev[maxev]		#O receives the event struct pointers
int	maskval			#O receives the mask value of the events
int	maxev			#I max events out
int	o_nev			#O same as function value (nev_out|EOF)

int     x1, x2, y1, y2, xs, xe, ys, ye, x, y
pointer pl, rl, rp, bp, ex, ev, ev_p, bbmask, bb_bufp
bool    useindex, lineio, bbused, rmused, nodata
int     bb_xsize, bb_ysize, bb_xblock, bb_yblock, ii, jj
int     v[NDIM], szs_event, mval, nev, evidx, evtop, temp, i
int     ev_xoff, ev_yoff

pointer	plr_open()
bool	pl_linenotempty(), pl_sectnotempty()
int	qpio_rbucket(), qpex_evaluate(), btoi(), plr_getpix()

define	swap {temp=$1;$1=$2;$2=temp}
define	putevent_  91
define	again_     92
define	done_      93
define	exit_      94

begin
	pl = IO_PL(io)		# pixel list (region mask) descriptor
	rl = IO_RL(io)		# range list buffer
	bp = IO_BP(io)		# bucket buffer (type short)
	ex = IO_EX(io)		# QPEX (EAF) descriptor

	# The following is executed when the first i/o is performed on a new
	# region, to select the most efficient type of i/o to be performed,
	# and initialize the i/o parameters for that case.  The type of i/o
	# to be performed depends upon whether or not an index can be used,
	# and whether or not there is a region mask (RM) or bounding box (BB).
	# The presence or absence of an event attribute filter (EAF) is not
	# separated out as a special case, as it is quick and easy to test
	# for the presence of an EAF and apply one it if it exists.

	if (IO_ACTIVE(io) == NO) {
	    # Check for an index.  We have an index if the event list is
	    # indexed, and the index is defined on the Y-coordinate we will
	    # be using for extraction.

	    useindex = (IO_INDEXLEN(io) == IO_NLINES(io) &&
			IO_EVYOFF(io)   == IO_IXYOFF(io) &&
			IO_NOINDEX(io)  == NO)

	    # Initialize the V and VN vectors.
	    do i = 1, NDIM {
		IO_VN(io,i) = IO_VE(io,i) - IO_VS(io,i) + 1
		if (IO_VN(io,i) < 0) {
		    swap (IO_VS(io,i), IO_VE(io,i))
		    IO_VN(io,i) = -IO_VN(io,i)
		}
	    }
	    call amovi (IO_VS(io,1), IO_V(io,1), NDIM)

	    # Determine if full lines are to be accessed, and if a bounding
	    # box (subraster of the image) is defined.

	    lineio = (IO_VS(io,1) == 1 && IO_VE(io,1) == IO_NCOLS(io))
	    bbused = (!lineio || IO_VS(io,2) > 1 || IO_VE(io,2) < IO_NLINES(io))

	    # Determine if region mask data is to be used and if there is any
	    # data to be read.

	    nodata = (IO_NEVENTS(io) <= 0)
	    rmused = false

	    if (pl != NULL)
		if (pl_sectnotempty (pl, IO_VS(io,1), IO_VE(io,1), NDIM))
		    rmused = true
		else
		    nodata = true

	    # Select the optimal type of i/o to be used for extraction.
	    if (nodata) {
		IO_IOTYPE(io) = NoDATA_NoAREA
		useindex = false
		bbused = false

	    } else if (bbused || rmused) {
		if (useindex)
		    IO_IOTYPE(io) = INDEX_RMorBB
		else
		    IO_IOTYPE(io) = NoINDEX_RMorBB

	    } else {
		# If we are reading the entire image (no bounding box) and
		# we are not using a mask, then there is no point in using
		# indexed i/o.

		IO_IOTYPE(io) = NoINDEX_NoRMorBB
		useindex = false
	    }

	    # Initialize the range list data if it will be used.
	    if (useindex) {
		# Dummy range specifying full line segment.
		RLI_LEN(rl)   = RL_FIRST
		RLI_AXLEN(rl) = IO_NCOLS(io)

		rp = rl + ((RL_FIRST - 1) * RL_LENELEM)
		Memi[rp+RL_XOFF] = IO_VS(io,1)
		Memi[rp+RL_NOFF] = IO_VN(io,1)
		Memi[rp+RL_VOFF] = 1

		IO_RLI(io) = RLI_INITIALIZE
	    }

	    # Open the mask for random access if i/o is not indexed and
	    # a region mask is used.

	    bbmask = IO_BBMASK(io)
	    if (bbmask != NULL)
		call plr_close (bbmask)

	    if (IO_IOTYPE(io) == NoINDEX_RMorBB && rmused) {
		bbmask = plr_open (pl, v, 0)	# (v is never referenced)
		call plr_setrect (bbmask, IO_VS(io,1),IO_VS(io,2),
		    IO_VE(io,1),IO_VE(io,2))
		call plr_getlut (bbmask,
		    bb_bufp, bb_xsize, bb_ysize, bb_xblock, bb_yblock)
	    }

	    # Update the QPIO descriptor.
	    IO_LINEIO(io)   = btoi(lineio)
	    IO_RMUSED(io)   = btoi(rmused)
	    IO_BBUSED(io)   = btoi(bbused)
	    IO_BBMASK(io)   = bbmask

	    IO_EVI(io)      = 1
	    IO_BKNO(io)     = 0
	    IO_BKLASTEV(io) = 0

	    IO_ACTIVE(io) = YES
	}

	# Initialize event extraction parameters.
	szs_event = IO_EVENTLEN(io)
	maskval = 0
	nev = 0

	ev_xoff = IO_EVXOFF(io)
	ev_yoff = IO_EVYOFF(io)

	# Extract events using the most efficient type of i/o for the given
	# selection critera (index, mask, BB, EAF, etc.).
again_
	switch (IO_IOTYPE(io)) {
	case NoDATA_NoAREA:
	    # We know in advance that there are no events to be returned,
	    # either because there is no data, or the area of the region
	    # mask within the bounding box is empty.

	    goto exit_

	case NoINDEX_NoRMorBB:
	    # This is the simplest case; no index, region mask, or bounding
	    # box.  Read and output all events in sequence.

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Copy out the event pointers.
	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    nev = min (maxev, IO_BKLASTEV(io) - IO_EVI(io) + 1)

	    do i = 1, nev {
		o_ev[i] = ev
		ev = ev + szs_event
	    }

	    IO_EVI(io) = IO_EVI(io) + nev
	    maskval = 1

	case NoINDEX_RMorBB:
	    # Fully general selection, including any combination of bounding
	    # box, region mask, or EAF, but no index, either because there is
	    # no index for this event list, or the index is for a different Y
	    # attribute than the one being used for extraction.

	    bbused = (IO_BBUSED(io) == YES)
	    x1 = IO_VS(io,1);  x2 = IO_VE(io,1)
	    y1 = IO_VS(io,2);  y2 = IO_VE(io,2)

	    # Refill the event bucket?
	    while (IO_EVI(io) > IO_BKLASTEV(io)) {
		# Get the next bucket.
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

		# Reject buckets that do not contain any events lying
		# within the specified bounding box, if any.

		if (bbused) {
		    ev_p = (IO_MINEVB(io) - 1) * SZ_SHORT / SZ_DOUBLE + 1
			xs = Memd[ev_p+ev_xoff] + 0.5
			ys = Memd[ev_p+ev_yoff] + 0.5

		    ev_p = (IO_MAXEVB(io) - 1) * SZ_SHORT / SZ_DOUBLE + 1
			xe = Memd[ev_p+ev_xoff] + 0.5
			ye = Memd[ev_p+ev_yoff] + 0.5

		    if (xs > x2 || xe < x1 || ys > y2 || ye < y1)
			IO_EVI(io) = IO_BKLASTEV(io) + 1
		}
	    }

	    # Copy out any events which pass the region mask and which share
	    # the same mask value.  Note that in this case, to speed mask
	    # value lookup at random mask coordinates, the region mask for
	    # the bounding box is stored as a populated array in the QPIO
	    # descriptor.

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io) - 1) * szs_event
	    bbmask = IO_BBMASK(io)
	    mval = 0

	    do i = IO_EVI(io), IO_BKLASTEV(io) {
		# Get event x,y coordinates in whatever coord system.
		ev = ev + szs_event
		ev_p = (ev - 1) * SZ_SHORT / SZ_DOUBLE + 1

		    x = Memd[ev_p+ev_xoff] + 0.5
		    y = Memd[ev_p+ev_yoff] + 0.5

		# Reject events lying outside the bounding box.
		if (bbused)
		    if (x < x1 || x > x2 || y < y1 || y > y2)
			next

		# Take a shortcut if no region mask is in effect for this BB.
		if (bbmask == NULL)
		    goto putevent_

		# Get the mask pixel associated with this event.
		ii = (x - 1) / bb_xblock
		jj = (y - 1) / bb_yblock
		mval = Memi[bb_bufp + jj*bb_xsize + ii]
		if (mval < 0)
		    mval = plr_getpix (bbmask, x, y)

		# Accumulate points lying in the first nonzero mask range
		# encountered.

		if (mval != 0) {
		    if (maskval == 0)
			maskval = mval
		    if (mval == maskval) {
putevent_		if (nev >= maxev)
			    break
			nev = nev + 1
			o_ev[nev] = ev
		    } else
			break
		}
	    }

	    IO_EVI(io) = i

	case INDEX_NoRMorBB, INDEX_RMorBB:
	    # General extraction for indexed data.  Process successive ranges
	    # and range lists until we get at least one event which lies within
	    # the bounding box, within a range, and which passes the event
	    # attribute filter, if one is in use.

	    # If the current range list (mask line) has been exhausted, advance
	    # to the next line which contains both ranges and events.  A range
	    # list is used to specify the bounding box even if we don't have
	    # a nonempty region mask within the BB.

	    if (IO_RLI(io) > RLI_LEN(rl)) {
		repeat {
		    y = IO_V(io,2)
		    if (IO_RLI(io) == RLI_INITIALIZE)
			IO_RLI(io) = RL_FIRST
		    else
			y = y + 1

		    if (y > IO_VE(io,2)) {
			if (nev <= 0) {
			    o_nev = EOF
			    return (EOF)
			} else
			    goto done_
		    }

		    IO_V(io,2) = y
		    evidx = Memi[IO_YOFFVP(io)+y-1]

		    if (evidx > 0) {
			if (IO_RMUSED(io) == YES) {
			    if (IO_LINEIO(io) == YES) {
				if (!pl_linenotempty (pl,IO_V(io,1)))
				    next
			    } else {
				v[1] = IO_VE(io,1);  v[2] = y
				if (!pl_sectnotempty (pl,IO_V(io,1),v,NDIM))
				    next
			    }
			    call pl_glri (pl, IO_V(io,1), Memi[rl],
				IO_MDEPTH(io), IO_VN(io,1), PIX_SRC)
			}
			IO_RLI(io) = RL_FIRST
		    }
		} until (IO_RLI(io) <= RLI_LEN(rl))

		IO_EVI(io) = evidx
		IO_EV1(io) = evidx
		IO_EV2(io) = Memi[IO_YLENVP(io)+y-1] + evidx - 1
	    }

	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    goto exit_

	    # Compute current range parameters and initialize event pointer.
	    rp = rl + (IO_RLI(io) - 1) * RL_LENELEM
	    x1 = Memi[rp+RL_XOFF]
	    x2 = x1 + Memi[rp+RL_NOFF] - 1
	    maskval = Memi[rp+RL_VOFF]

	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    evtop = min (IO_EV2(io), IO_BKLASTEV(io))

	    # Extract events from bucket which lie within the current range
	    # of the current line.  This is the inner loop of indexed event
	    # extraction, ignoring event attribute filtering.

	    do i = IO_EVI(io), evtop {
		ev_p = (ev - 1) * SZ_SHORT / SZ_DOUBLE + 1
		    x = Memd[ev_p+ev_xoff] + 0.5
		if (x >= x1) {
		    if (x > x2) {
			IO_RLI(io) = IO_RLI(io) + 1
			break
		    } else if (nev >= maxev)
			break
		    nev = nev + 1
		    o_ev[nev] = ev
		}
		ev = ev + szs_event
	    }

	    IO_EVI(io) = i
	    if (i > IO_EV2(io))
		IO_RLI(io) = RLI_NEXTLINE
	}
done_
	# Apply the event attribute filter if one is defined; repeat
	# the whole process if we don't end up with any events.

	if (nev > 0)
	    if (ex != NULL)
		nev = qpex_evaluate (ex, o_ev, o_ev, nev)
	if (nev <= 0)
	    goto again_
exit_
	o_nev = nev
	if (o_nev <= 0)
	    o_nev = EOF

	return (o_nev)
end


