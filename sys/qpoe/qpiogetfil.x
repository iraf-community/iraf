# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"qpoe.h"
include	"qpio.h"

# QPIO_GETFILTER -- Get the current filtering parameters governing event
# extraction via QPIO.  These are the QPIO parameters (region mask, blocking
# factor, coordinate system, etc.) plus the event attribute filter.  We leave
# it up to QPEX to reconstruct the EAF to allow for any dynamic edits which
# may have taken place, e.g., via qpio_setfilter.

int procedure qpio_getfilter (io, outstr, maxch)

pointer	io			#I QPIO descriptor
char	outstr[maxch]		#O where to put the filter text
int	maxch			#I max chars out

int	op
pointer	sp, buf, bp
int	gstrcpy(), qpex_getfilter()
define	ovfl_ 91

begin
	call smark (sp)
	call salloc (buf, SZ_TEXTBUF, TY_CHAR)

	op = 1

	# Report on QPIO parameters first.

	# Parameter name.
	call sprintf (Memc[buf], SZ_TEXTBUF, "param=%s,")
	    call pargstr (Memc[IO_PARAM(io)])
	op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	if (op > maxch)
	    goto ovfl_

	# Coordinate system.
	call sprintf (Memc[buf], SZ_TEXTBUF, "key=(%c%d,%c%d),")
	    if (IO_EVXTYPE(io) == TY_INT) {
		call pargi ('i')
		call pargi (IO_EVXOFF(io) * SZ_INT * SZB_CHAR)
	    } else {
		call pargi ('s')
		call pargi (IO_EVXOFF(io) * SZ_SHORT * SZB_CHAR)
	    }
	    if (IO_EVYTYPE(io) == TY_INT) {
		call pargi ('i')
		call pargi (IO_EVYOFF(io) * SZ_INT * SZB_CHAR)
	    } else {
		call pargi ('s')
		call pargi (IO_EVYOFF(io) * SZ_SHORT * SZB_CHAR)
	    }
	op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	if (op > maxch)
	    goto ovfl_

	# Blocking factor for generating pixels.
	call sprintf (Memc[buf], SZ_TEXTBUF, "block=%dx%d, ")
	    call pargi (IO_XBLOCK(io))
	    call pargi (IO_YBLOCK(io))
	op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	if (op > maxch)
	    goto ovfl_

	# Region mask, if any.
	if (Memc[IO_MASK(io)] != EOS) {
	    call sprintf (Memc[buf], SZ_TEXTBUF, "mask=%s,")
		call pargstr (Memc[IO_MASK(io)])
	    op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	    if (op > maxch)
		goto ovfl_
	}

	# Debug level, if debug messages enabled.
	if (IO_DEBUG(io) > 0) {
	    call sprintf (Memc[buf], SZ_TEXTBUF, "debug=%d,")
		call pargi (IO_DEBUG(io))
	    op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	    if (op > maxch)
		goto ovfl_
	}

	# Noindex flag, if enabled.
	if (IO_NOINDEX(io) > 0) {
	    call sprintf (Memc[buf], SZ_TEXTBUF, "noindex=%b,")
		call pargi (IO_NOINDEX(io))
	    op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	    if (op > maxch)
		goto ovfl_
	}

	# Event attribute filter.
	if (IO_EX(io) != NULL) {
	    bp = buf + gstrcpy ("filter=(", Memc[buf], SZ_TEXTBUF)
	    bp = bp + qpex_getfilter (IO_EX(io), Memc[bp], SZ_TEXTBUF-8)
	    Memc[bp] = ')';  bp = bp + 1
	    Memc[bp] = EOS
	    op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	    if (op > maxch)
		goto ovfl_
	} else if (op > 1) {
	    # Clobber trailing comma.
	    op = op - 1
	    outstr[op] = EOS
	}

	call sfree (sp)
	return (op - 1)
ovfl_
	call sfree (sp)
	outstr[maxch+1] = EOS
	return (maxch)
end
