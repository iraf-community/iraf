# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"qpoe.h"
include	"qpio.h"

# QPIO_PUTEVENTS -- Append events to a new event list.  No filtering is
# performed.  As events are received they are merely copied into the bucket
# currently being filled, writing each bucket to the output lfile as it fills.
# No sorting is performed, hence if an indexed list is desired, the caller
# must output the events in sort order (normally sorted by Y and then by X
# within each image line).

procedure qpio_putevents (io, i_ev, nevents)

pointer	io			#I QPIO descriptor
pointer	i_ev[ARB]		#I array of event pointers
int	nevents			#I number of events

pointer	qp, bp, ev
int	szs_event, szb_page, nwords, bklen, bksiz, nev, i, j
errchk	qpio_wbucket, qpio_sync, malloc, calloc

begin
	szs_event = IO_EVENTLEN(io)
	bp = IO_BP(io)

	# Fix the event list parameters and write out the event list header
	# when the first write to a new event list occurs.

	if (IO_ACTIVE(io) == NO) {
	    qp = IO_QP(io)
	    szb_page = QP_FMPAGESIZE(qp)

	    IO_FBOFF(io)	= szb_page + 1
	    IO_EVENTLEN(io)	= DD_STRUCTLEN(IO_DD(io))*SZ_STRUCT/SZ_SHORT
	    IO_NEVENTS(io)	= 0

	    # Force the bucket size to an integral number of datafile pages,
	    # and adjust the number of events to fill the bucket, allowing
	    # 2 extra slots at the end for the min/max event structs.

	    bklen = QP_BUCKETLEN(qp) + 2
	    bksiz = bklen * (IO_EVENTLEN(io) * SZ_SHORT * SZB_CHAR)
	    bksiz = (bksiz - 1) / szb_page * szb_page
	    bklen = bksiz / (IO_EVENTLEN(io) * SZ_SHORT * SZB_CHAR)

	    IO_BUCKETLEN(io)	= bklen - 2
	    IO_SZBBUCKET(io)	= bksiz
	    IO_EVMINOFF(io)	= szs_event * (bklen - 2)
	    IO_EVMAXOFF(io)	= szs_event * (bklen - 1)
	    IO_EVI(io)		= 1
	    IO_BKNO(io)		= 1
	    IO_BKFIRSTEV(io)	= 1
	    IO_BKLASTEV(io)	= bklen - 2 

	    if (IO_DEBUG(io) > 1) {
		call eprintf ("%s: assign szbk=%d, bklen=%d+2\n")
		    call pargstr (Memc[IO_PARAM(io)])
		    call pargi (bksiz)
		    call pargi (bklen - 2)
	    }

	    # Allocate the bucket buffer.
	    call malloc (IO_BP(io), bksiz / SZB_CHAR / SZ_SHORT, TY_SHORT)
	    bp = IO_BP(io)

	    # Allocate the MINEVL and MAXEVL event structs, used to keep
	    # track of the min and max event field values for the entire
	    # event list.

	    nwords = IO_EVENTLEN(io)
	    call calloc (IO_MINEVL(io), nwords, TY_SHORT)
	    call calloc (IO_MAXEVL(io), nwords, TY_SHORT)

	    # Write the event list header.
	    call qpio_sync (io)

	    IO_ACTIVE(io) = YES
	}

	# Make sure there is room in the bucket.
	if (IO_EVI(io) > IO_BKLASTEV(io))
	    call qpio_wbucket (io, IO_EVI(io))

	# Output the current batch of events.
	for (j=0;  j < nevents;  j=j+nev) {
	    # Copy out as many events as will fit in the bucket.
	    nev = min (nevents-j, (IO_BKLASTEV(io) - IO_EVI(io) + 1))
	    if (nev <= 0)
		break

	    ev  = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    do i = 1, nev {
		call amovs (Mems[i_ev[i+j]], Mems[ev], szs_event)
		ev = ev + szs_event
	    }

	    # Write out the bucket if it fills.
	    IO_EVI(io) = IO_EVI(io) + nev
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		call qpio_wbucket (io, IO_EVI(io))
	}
end
