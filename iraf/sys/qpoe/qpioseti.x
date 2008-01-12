# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpioset.h>
include	<syserr.h>
include	<plset.h>
include	"qpio.h"

# QPIO_SETI -- Set a QPIO interface integer valued parameter.  This procedure
# represents the lowest level interface by which an applications program can
# control QPIO.

procedure qpio_seti (io, param, value)

pointer	io			#I QPIO descriptor
int	param			#I parameter code
int	value			#I new parameter value

int	naxes, axlen[PL_MAXDIM], sv_active
errchk	pl_close, syserr, realloc

begin
	# Almost everything here cancels any active i/o.
	sv_active = IO_ACTIVE(io)
	IO_ACTIVE(io) = NO

	# Set the named parameter.
	switch (param) {
	case QPIO_BLOCKFACTOR:
	    IO_XBLOCK(io) = value
	    IO_YBLOCK(io) = value
	case QPIO_XBLOCKFACTOR:
	    IO_XBLOCK(io) = value
	case QPIO_YBLOCKFACTOR:
	    IO_YBLOCK(io) = value
	case QPIO_EVXOFF:
	    IO_EVXOFF(io) = value
	case QPIO_EVYOFF:
	    IO_EVYOFF(io) = value
	case QPIO_EVXTYPE:
	    IO_EVXTYPE(io) = value
	case QPIO_EVYTYPE:
	    IO_EVYTYPE(io) = value
	case QPIO_NOINDEX:
	    IO_NOINDEX(io) = value
	case QPIO_NODEFFILT:
	    IO_NODEFFILT(io) = value
	case QPIO_NODEFMASK:
	    IO_NODEFMASK(io) = value
	case QPIO_OPTBUFSIZE:
	    IO_OPTBUFSIZE(io) = value

	case QPIO_BUCKETLEN:
	    # Set the bucket length (new event lists only).
	    if (IO_MODE(io) != READ_ONLY)
		IO_BUCKETLEN(io) = value

	case QPIO_DEBUG:
	    # Set the debug level; don't modify IO_ACTIVE.
	    IO_ACTIVE(io) = sv_active
	    IO_DEBUG(io) = value

	case QPIO_EX:
	    # Set the event attribute filter.
	    if (IO_EX(io) != NULL && IO_EXCLOSE(io) == YES)
		call qpex_close (IO_EX(io))
	    IO_EX(io) = value
	    IO_EXCLOSE(io) = NO

	case QPIO_PL:
	    # Set the PLIO region mask.
	    if (IO_PL(io) != NULL && IO_PLCLOSE(io) == YES)
		call pl_close (IO_PL(io))

	    IO_PL(io) = value
	    IO_PLCLOSE(io) = NO
	    call pl_gsize (IO_PL(io), naxes, axlen, IO_MDEPTH(io))
	    if (axlen[1] != IO_NCOLS(io) || axlen[2] != IO_NLINES(io))
		call syserr (SYS_QPPLSIZE)

	    # Allocate a range list buffer if i/o is indexed.
	    if (IO_INDEXLEN(io) > 0)
		call realloc (IO_RL(io), RL_MAXLEN(IO_PL(io)), TY_INT)

	    # Update the mask name, such as it is...
	    if (IO_MASK(io) != NULL) {
		call sprintf (Memc[IO_MASK(io)], SZ_FNAME, "%xX")
		    call pargi (value)
	    }
	}
end
