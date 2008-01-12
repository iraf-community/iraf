# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMPKWR -- Set the value of the named header keyword as a real.

procedure impkwr (im, keyw, rval, ier)

pointer	im			# imfort image descriptor
%       character*(*) keyw
real	rval
int	ier

pointer	sp, kp
int	errcode()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	iferr (call imputr (im, Memc[kp], rval)) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    ier = OK
	    IM_UPDATE(im) = YES
	}

	call sfree (sp)
end
