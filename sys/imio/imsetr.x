# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imset.h>
include	<imio.h>
include	<fset.h>

# IMSETR -- Set an IMIO parameter of type real.  We are called by IMSETI hence
# integer parameters and switches may be set too.

procedure imsetr (im, param, value)

pointer	im			# image descriptor
int	param			# parameter to be set
real	value			# integer value of parameter

int	i
pointer	ibdes
errchk	calloc

begin
	switch (param) {
	case IM_ADVICE:
	    IM_VADVICE(im) = value
	case IM_BUFSIZE:
	    IM_VBUFSIZE(im) = value
	case IM_COMPRESS:
	    IM_VCOMPRESS(im) = value
	case IM_NBNDRYPIX:
	    IM_VNBNDRYPIX(im) = max (0, nint(value))
	case IM_TYBNDRY:
	    IM_VTYBNDRY(im) = value
	case IM_BNDRYPIXVAL:
	    IM_OOBPIX(im) = value
	case IM_FLAGBADPIX:
	    IM_VFLAGBADPIX(im) = value
	case IM_PIXFD:
	    IM_PFD(im) = value
	case IM_WHEADER:
	    IM_UPDATE(im) = value

	case IM_PLDES:
	    IM_PL(im) = value
	case IM_RLIO:
	    # Enable/disable range list i/o (for image masks).
	    if (int (value) == YES)
		IM_PLFLAGS(im) = or (IM_PLFLAGS(im), PL_RLIO)
	    else
		IM_PLFLAGS(im) = and (IM_PLFLAGS(im), not(PL_RLIO))

	case IM_NBUFS:
	    # Free any existing input buffers.
	    ibdes = IM_IBDES(im)
	    if (ibdes != NULL)
		for (i=0;  i < IM_VNBUFS(im);  i=i+1)
		    call mfree (BD_BUFPTR(ibdes + LEN_BDES * i), TY_CHAR)

	    # Change size of buffer pool.
	    IM_VNBUFS(im) = nint (value)

	    # Reinit input buffer descriptors.  The actual input buffers will be
	    # reallocated upon demand.

	    if (ibdes != NULL) {
		call mfree (IM_IBDES(im), TY_STRUCT)
		call calloc (IM_IBDES(im), LEN_BDES * IM_VNBUFS(im), TY_STRUCT)
		IM_NGET(im) = 0
	    }

	case IM_CANCEL:
	    # Free any pixel data buffers associated with an image.
	    call imrmbufs (im)

	case IM_CLOSEFD:
	    # Set F_CLOSEFD on the pixel file.
	    IM_VCLOSEFD(im) = nint(value)
	    if (IM_PFD(im) != NULL)
		call fseti (IM_PFD(im), F_CLOSEFD, nint(value))

	default:
	    call imerr (IM_NAME(im), SYS_IMSETUNKPAR)
	}
end
