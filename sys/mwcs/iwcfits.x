# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imwcs.h"

# IW_CFITS -- Close (free) an IMWCS descriptor allocated previously by
# IW_RFITS.

procedure iw_cfits (iw)

pointer	iw			#I pointer to IMWCS descriptor

begin
	if (IW_CBUF(iw) != NULL)
	    call mfree (IW_CBUF(iw), TY_STRUCT)
	if (IW_SBUF(iw) != NULL)
	    call mfree (IW_SBUF(iw), TY_CHAR)
	call mfree (iw, TY_STRUCT)
end
