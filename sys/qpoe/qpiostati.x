# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpioset.h>
include	"qpio.h"

# QPIO_STATI -- Stat a QPIO interface integer valued parameter.

int procedure qpio_stati (io, param)

pointer	io			#I QPIO descriptor
int	param			#I parameter code

bool	fp_equalr()

begin
	switch (param) {
	case QPIO_BLOCKFACTOR:
	    if (fp_equalr (IO_XBLOCK(io), IO_YBLOCK(io)))
		return (IO_XBLOCK(io))
	    else
		return (ERR)
	case QPIO_XBLOCKFACTOR:
	    return (IO_XBLOCK(io))
	case QPIO_YBLOCKFACTOR:
	    return (IO_YBLOCK(io))
	case QPIO_BUCKETLEN:
	    return (IO_BUCKETLEN(io))
	case QPIO_DEBUG:
	    return (IO_DEBUG(io))
	case QPIO_EVXOFF:
	    return (IO_EVXOFF(io))
	case QPIO_EVYOFF:
	    return (IO_EVYOFF(io))
	case QPIO_EVXTYPE:
	    return (IO_EVXTYPE(io))
	case QPIO_EVYTYPE:
	    return (IO_EVYTYPE(io))
	case QPIO_EX:
	    return (IO_EX(io))
	case QPIO_NODEFFILT:
	    return (IO_NODEFFILT(io))
	case QPIO_NODEFMASK:
	    return (IO_NODEFMASK(io))
	case QPIO_NOINDEX:
	    return (IO_NOINDEX(io))
	case QPIO_OPTBUFSIZE:
	    return (IO_OPTBUFSIZE(io))
	case QPIO_PL:
	    return (IO_PL(io))

	case QPIO_EVENTLEN:			# length of event struct, shorts
	    return (IO_EVENTLEN(io))
	case QPIO_FD:				# FIO fd of event list lfile
	    return (IO_FD(io))
	case QPIO_INDEXLEN:			# index length (0=noindex)
	    return (IO_INDEXLEN(io))
	case QPIO_IXXOFF:			# offset of X in index
	    return (IO_IXXOFF(io))
	case QPIO_IXYOFF:			# offset of Y in index
	    return (IO_IXYOFF(io))
	case QPIO_IXXTYPE:			# datatype of X in index
	    return (IO_IXXTYPE(io))
	case QPIO_IXYTYPE:			# datatype of Y in index
	    return (IO_IXYTYPE(io))
	case QPIO_LF:				# FMIO lfile number
	    return (IO_LF(io))
	case QPIO_MASKP:			# PLIO descriptor
	    return (IO_MASK(io))
	case QPIO_MAXEVP:			# pointer to short
	    return (IO_MAXEVL(io))
	case QPIO_MINEVP:			# pointer to short
	    return (IO_MINEVL(io))
	case QPIO_NCOLS:
	    return (IO_NCOLS(io))
	case QPIO_NLINES:
	    return (IO_NLINES(io))
	case QPIO_PARAMP:			# pointer to char
	    return (IO_PARAM(io))
	case QPIO_QP:
	    return (IO_QP(io))			# QPOE descriptor
	}

	return (ERR)
end
