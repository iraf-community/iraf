# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<fset.h>
include	"qpoe.h"
include	"qpio.h"

# QPIO_WBUCKET -- Flush any data currently in the bucket to the datafile,
# and set up the buffer to receive data for the bucket BKNO.  The min/max
# event structs are updated whenever a bucket is written to disk. IO_EVI
# is assumed to point to the event following the last event written into
# in the buffer.  Data should always be written sequentially.

procedure qpio_wbucket (io, evi)

pointer	io			#I QPIO descriptor
int	evi			#I evi of next bucket on exit

pointer	min_ev[2], max_ev[2], ev, fp, mp, dd
int	sz_event, offset, dtype, nb, flen, nchars, i, j, k
int	fstati()

begin
	dd = IO_DD(io)

	# Write the current bucket to the datafile if nonempty.
	if (dd != NULL && IO_EVI(io) > IO_BKFIRSTEV(io)) {
	    # Scan through the events in the bucket and update the min/max
	    # event structs for the event list ([1] below) and for the
	    # bucket ([2] below, stored at the end of the bucket).
	    # Use CHAR pointers to facilitate pointer conversions.

	    min_ev[1]	= (IO_MINEVL(io) - 1) * SZ_SHORT + 1
	    min_ev[2]	= (IO_MINEVB(io) - 1) * SZ_SHORT + 1
	    max_ev[1]	= (IO_MAXEVL(io) - 1) * SZ_SHORT + 1
	    max_ev[2]	= (IO_MAXEVB(io) - 1) * SZ_SHORT + 1
	    sz_event	= DD_STRUCTLEN(dd) * SZ_STRUCT

	    do k = 1, 2 {
		ev = (IO_BP(io) - 1) * SZ_SHORT + 1
		# If min/max of bucket or first bucket of event list...
		if (k == 2 || IO_BKNO(io) == 1) {
		    call amovc (Memc[ev], Memc[min_ev[k]], sz_event)
		    call amovc (Memc[ev], Memc[max_ev[k]], sz_event)
		}

		do j = 1, IO_EVI(io) - IO_BKFIRSTEV(io) {
		    do i = 1, DD_NFIELDS(dd) {
			# Get the typed offset and datatype of the field.
			offset = DD_FOFFSET(dd,i)
			dtype  = DD_FTYPE(dd,i)

			# Update the min/max entries for the field.
			switch (dtype) {
			case TY_SHORT:
			    fp = (ev - 1) / SZ_SHORT + 1 + offset
			    mp = (min_ev[k] - 1) / SZ_SHORT + 1 + offset
			    if (Mems[fp] < Mems[mp])
				Mems[mp] = Mems[fp]
			    mp = (max_ev[k] - 1) / SZ_SHORT + 1 + offset
			    if (Mems[fp] > Mems[mp])
				Mems[mp] = Mems[fp]

			case TY_INT, TY_LONG:
			    fp = (ev - 1) / SZ_INT + 1 + offset
			    mp = (min_ev[k] - 1) / SZ_INT + 1 + offset
			    if (Memi[fp] < Memi[mp])
				Memi[mp] = Memi[fp]
			    mp = (max_ev[k] - 1) / SZ_INT + 1 + offset
			    if (Memi[fp] > Memi[mp])
				Memi[mp] = Memi[fp]

			case TY_REAL:
			    fp = (ev - 1) / SZ_REAL + 1 + offset
			    mp = (min_ev[k] - 1) / SZ_REAL + 1 + offset
			    if (Memr[fp] < Memr[mp])
				Memr[mp] = Memr[fp]
			    mp = (max_ev[k] - 1) / SZ_REAL + 1 + offset
			    if (Memr[fp] > Memr[mp])
				Memr[mp] = Memr[fp]

			case TY_DOUBLE:
			    fp = (ev - 1) / SZ_DOUBLE + 1 + offset
			    mp = (min_ev[k] - 1) / SZ_DOUBLE + 1 + offset
			    if (Memd[fp] < Memd[mp])
				Memd[mp] = Memd[fp]
			    mp = (max_ev[k] - 1) / SZ_DOUBLE + 1 + offset
			    if (Memd[fp] > Memd[mp])
				Memd[mp] = Memd[fp]
			}
		    }

		    ev = ev + sz_event
		}
	    }

	    # Zero out any remaining events.
	    while (ev < min_ev[2]) {
		call aclrc (Memc[ev], sz_event)
		ev = ev + sz_event
	    }

	    # Write the bucket.
	    nb = IO_SZBBUCKET(io)
	    offset = (IO_BKNO(io) - 1) * nb + IO_FBOFF(io)
	    call fm_lfawrite (IO_CHAN(io), Mems[IO_BP(io)], nb, offset)
	    call fm_lfawait (IO_CHAN(io), nb)

	    # Update the file size.
	    flen = fstati (IO_FD(io), F_FILESIZE)
	    nchars = (offset + nb) / SZB_CHAR
	    if (nchars > flen)
		call fseti (IO_FD(io), F_FILESIZE, nchars)

	    # Increment the total event count.
	    IO_NEVENTS(io) = max (IO_NEVENTS(io), IO_EVI(io) - 1)
	    S_NELEM(IO_PSYM(io)) = IO_NEVENTS(io)
	    QP_MODIFIED(IO_QP(io)) = YES
	}

	# Set up the buffer for the new bucket.
	IO_BKNO(io) = EVI_TO_BUCKET(io,evi)
	IO_BKFIRSTEV(io) = BUCKET_TO_EVI(io,IO_BKNO(io))
	IO_BKLASTEV(io)  = IO_BKFIRSTEV(io) + IO_BUCKETLEN(io) - 1

	if (IO_DEBUG(io) > 2) {
	    call eprintf ("wbucket: evi=%d, bkno=%d\n")
		call pargi(evi)
		call pargi(IO_BKNO(io))
	}
end
