# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<mach.h>
include	<fset.h>
include	"qpoe.h"
include	"qpio.h"

# QPIO_MKINDEX -- Make an index for the event list associated with the QPIO
# descriptor.  The event list must have been already written out, in sorted
# order according to the given key.  Once an event list is indexed it cannot
# be further extended or otherwise modified.  In QPOE, only short and int
# coordinate fields can be used to construct an index.  The key fields are
# specified as, e.g., "s10,s8" or "(s10,s8)" (Y // X), where the field name
# is the datatype code (s for short, i for int) followed by the decimal byte
# offset of the field in the event struct.  Both fields do not have to be
# the same datatype.

procedure qpio_mkindex (io, key)

pointer	io			#I QPIO descriptor
char	key[ARB]		#I list of key fields

bool	xint, yint, isint
pointer	sp, tokbuf, ip, in, ev, ev_i, ov, lv, oo, bp
int	ox, line, nevents, szs_event, ncols, nlines, nout, x, y, i
int	token, offset, xoff, yoff, len_index, nev, fd, sv_evi, firstev

long	note()
pointer	qp_opentext()
int	qp_gettok(), ctoi(), qpio_rbucket(), pl_p2li()
errchk	qp_opentext, qpio_rbucket, qpio_sync, write, calloc, syserrs
define	nosort_ 91

begin
	call smark (sp)
	call salloc (tokbuf, SZ_TOKBUF, TY_CHAR)
	call malloc (oo, IO_NLINES(io) * 2, TY_SHORT)

	ncols  = IO_NCOLS(io)
	nlines = IO_NLINES(io)
	sv_evi = IO_EVI(io)

	# Key defaults to sort x/y.
	xoff = IO_EVXOFF(io)
	yoff = IO_EVYOFF(io)
	xint = (IO_EVXTYPE(io) == TY_INT)
	yint = (IO_EVYTYPE(io) == TY_INT)

	# Parse key list (macro references are permitted) to get offsets of
	# the X and Y coordinate fields to be used as the index key.

	in = qp_opentext (IO_QP(io), key)

	do i = 1, 2 {
	    repeat {
		token = qp_gettok (in, Memc[tokbuf], SZ_TOKBUF)
	    } until (token == EOF || token == TOK_IDENTIFIER)
	    if (token == EOF)
		break

	    call strlwr (Memc[tokbuf])
	    if (Memc[tokbuf] == 'i')
		isint = true
	    else if (Memc[tokbuf] == 's')
		isint = false
	    else
		call syserrs (SYS_QPXYFNS, key)
	    if (i == 1)
		xint = isint
	    else
		yint = isint

	    ip = tokbuf + 1
	    if (ctoi (Memc, ip, offset) <= 0)
		call syserrs (SYS_QPBADKEY, key)
	    else if (isint)
		offset = offset / (SZ_INT * SZB_CHAR)
	    else
		offset = offset / (SZ_SHORT * SZB_CHAR)

	    if (i == 1)
		yoff = offset
	    else
		xoff = offset

	    while (qp_gettok (in, Memc[tokbuf], SZ_TOKBUF) != EOF)
		if (Memc[tokbuf] == ',')
		    break
	}

	call qp_closetext (in)

	# Sync the event list to ensure that the bucket is flushed.
	call qpio_sync (io)

	fd = IO_FD(io)
	bp = IO_BP(io)
	len_index = nlines
	szs_event = IO_EVENTLEN(io)

	if (IO_DEBUG(io) > 1) {
	    call eprintf ("qpio_mkindex (%xX, `%s')\n")
		call pargi (io)
		call pargstr (key)
	    call eprintf ("nevents=%d, evsize=%d, xkey=%c%d, ykey=%c%d\n")
		call pargi (IO_NEVENTS(io))
		call pargi (szs_event)
		if (xint)
		    call pargc ('i')
		else
		    call pargc ('s')
		call pargi (xoff)
		if (yint)
		    call pargc ('i')
		else
		    call pargc ('s')
		call pargi (yoff)
	}

	# Allocate the offset and length vectors (comprising the index).
	# These are deallocated at qpio_close time.

	call calloc (ov, len_index, TY_INT)
	call calloc (lv, len_index, TY_INT)

	ox = -1
	line = 1
	firstev = 1
	nevents = 0

	# Rewind the list.
	i = qpio_rbucket (io, 1)

	# For each event in the event list...
	for (IO_EVI(io)=1;  IO_EVI(io) <= IO_NEVENTS(io);  ) {
	    # Refill the event bucket?
	    if (IO_EVI(io) > IO_BKLASTEV(io))
		if (qpio_rbucket (io, IO_EVI(io)) == EOF)
		    break

	    # Process all events in the bucket.
	    ev = bp + (IO_EVI(io) - IO_BKFIRSTEV(io)) * szs_event
	    nev = min (IO_NEVENTS(io), IO_BKLASTEV(io)) - IO_EVI(io) + 1
	    nout = 0

	    do i = 1, nev {
		ev_i = (ev - 1) * SZ_SHORT / SZ_INT + 1

		if (xint)
		    x = Memi[ev_i+xoff]
		else
		    x = Mems[ev+xoff]

		if (yint)
		    y = Memi[ev_i+yoff]
		else
		    y = Mems[ev+yoff]

		x = max(1, min(ncols,  x))
		y = max(1, min(nlines, y))

		if (IO_DEBUG(io) > 4) {
		    # Egads!  Dump every photon.
		    call eprintf ("(%04d,%04d) ")
			call pargi (x)
			call pargi (y)
		    nout = nout + 1
		    if (nout >= 6) {
			call eprintf ("\n")
			nout = 0
		    }
		}

		if (y > line) {
		    # Add index entry.
		    if (nevents > 0) {
			Memi[ov+line-1] = firstev
			Memi[lv+line-1] = nevents

			if (IO_DEBUG(io) > 3 && nevents > 0) {
			    if (nout > 0) {
				call eprintf ("\n")
				nout = 0
			    }
			    call eprintf ("%4d: ev=%d, nev=%d\n")
				call pargi (line)
				call pargi (firstev)
				call pargi (nevents)
			}
		    }

		    # Set up the new line.
		    firstev = IO_EVI(io) + i - 1
		    nevents = 1
		    line = y
		    ox = x

		} else if (y == line) {
		    # Add another event to the current line.
		    nevents = nevents + 1
		    if (x < ox)
			goto nosort_
		    else
			ox = x
		} else
		    goto nosort_

		ev = ev + szs_event
	    }

	    IO_EVI(io) = IO_EVI(io) + nev
	    if (nout > 0) {
		call eprintf ("\n")
		nout = 0
	    }
	}

	# Output final index entry.
	if (nevents > 0) {
	    Memi[ov+line-1] = firstev
	    Memi[lv+line-1] = nevents
	}

	# Apply data compression to the index arrays and append to the event
	# list lfile.

	call fseti (fd, F_BUFSIZE, len_index * SZ_INT)
	call seek (fd, EOF)

	IO_YOFFVOFF(io) = note (fd)
	IO_YOFFVLEN(io) = pl_p2li (Memi[ov], 1, Mems[oo], len_index)
	call write (fd, Mems[oo], IO_YOFFVLEN(io) * SZ_SHORT)

	IO_YLENVOFF(io) = note (fd)
	IO_YLENVLEN(io) = pl_p2li (Memi[lv], 1, Mems[oo], len_index)
	call write (fd, Mems[oo], IO_YLENVLEN(io) * SZ_SHORT)

	call flush (fd)
	call fseti (fd, F_BUFSIZE, 0)

	# Update the remaining index related fields of the QPIO descriptor.
	IO_INDEXLEN(io) = len_index
	IO_YOFFVP(io) = ov
	IO_YLENVP(io) = lv

	IO_IXXOFF(io) = xoff
	IO_IXYOFF(io) = yoff

	if (xint)
	    IO_IXXTYPE(io) = TY_INT
	else
	    IO_IXXTYPE(io) = TY_SHORT
	if (yint)
	    IO_IXYTYPE(io) = TY_INT
	else
	    IO_IXYTYPE(io) = TY_SHORT

	if (IO_DEBUG(io) > 1) {
	    call eprintf ("index.offv %d words at offset %d\n")
		call pargi (IO_YOFFVLEN(io))
		call pargi (IO_YOFFVOFF(io))
	    call eprintf ("index.lenv %d words at offset %d\n")
		call pargi (IO_YLENVLEN(io))
		call pargi (IO_YLENVOFF(io))
	}

	# Update the event list header.
	call qpio_sync (io)

	IO_EVI(io) = sv_evi
	call sfree (sp)
	return

nosort_
	# A nonsorted event list has been detected, hence we cannot build
	# an index, but we need not abort since nonindexed event lists are
	# still usable.

	if (nout > 0)
	    call eprintf ("\n")
	iferr (call syserrs (SYS_QPEVNSORT, Memc[IO_PARAM(io)]))
	    call erract (EA_WARN)

	IO_INDEXLEN(io) = 0
	call mfree (ov, TY_INT)
	call mfree (lv, TY_INT)

	IO_EVI(io) = sv_evi
	call sfree (sp)
end
