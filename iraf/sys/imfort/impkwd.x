# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMPKWD -- Set the value of the named header keyword as a double.

procedure impkwd (im, keyw, dval, ier)

size_t	sz_val
pointer	im			# imfort image descriptor
%       character*(*) keyw
double	dval
int	ier

pointer	sp, kp
int	errcode()

begin
	call smark (sp)
	sz_val = SZ_KEYWORD
	call salloc (kp, sz_val, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	iferr (call imputd (im, Memc[kp], dval)) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    ier = OK
	    IM_UPDATE(im) = YES
	}

	call sfree (sp)
end
