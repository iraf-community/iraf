# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imio.h>

# IMRMBUFS -- Free any pixel data buffers currently allocated to an image.

procedure imrmbufs (im)

pointer	im			# image descriptor

int	i
pointer	ibdes, obdes

begin
	ibdes = IM_IBDES(im)
	obdes = IM_OBDES(im)

	if (ibdes != NULL) {
	    for (i=0;  i < IM_VNBUFS(im);  i=i+1)
		call mfree (BD_BUFPTR(ibdes + LEN_BDES * i), TY_CHAR)
	    call mfree (ibdes, TY_STRUCT)
	}

	if (obdes != NULL) {
	    call mfree (BD_BUFPTR(obdes), TY_CHAR)
	    call mfree (obdes, TY_STRUCT)
	}

	IM_IBDES(im) = NULL
	IM_OBDES(im) = NULL
end
