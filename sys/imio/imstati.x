# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imset.h>
include	<imio.h>

# IMSTATI -- Get an IMIO option of type integer.

int procedure imstati (im, option)

pointer	im			#I image descriptor
int	option			#I imset option being queried

begin
	switch (option) {
	case IM_ADVICE:
	    return (IM_VADVICE(im))
	case IM_BUFSIZE:
	    return (IM_VBUFSIZE(im))
	case IM_BUFFRAC:
	    return (IM_VBUFFRAC(im))
	case IM_BUFMAX:
	    return (IM_VBUFMAX(im))
	case IM_NBUFS:
	    return (IM_VNBUFS(im))
	case IM_COMPRESS:
	    return (IM_VCOMPRESS(im))
	case IM_NBNDRYPIX:
	    return (IM_VNBNDRYPIX(im))
	case IM_TYBNDRY:
	    return (IM_VTYBNDRY(im))
	case IM_FLAGBADPIX:
	    return (IM_VFLAGBADPIX(im))
	case IM_PIXFD:
	    return (IM_PFD(im))
	case IM_CLOSEFD:
	    return (IM_VCLOSEFD(im))
	case IM_WHEADER:
	    return (IM_UPDATE(im))
	case IM_PLDES:
	    return (IM_PL(im))
	case IM_RLIO:
	    if (and (IM_PLFLAGS(im), PL_RLIO) != 0)
		return (YES)
	    else
		return (NO)
	default:
	    call imerr (IM_NAME(im), SYS_IMSTATUNKPAR)
	}
end
