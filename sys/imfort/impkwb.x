# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMPKWB -- Set the value of the named header keyword as a boolean.

procedure impkwb (im, keyw, bval, ier)

pointer	im			# imfort image descriptor
%       character*(*) keyw
bool	bval
int	ier

pointer	sp, kp
int	errcode()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	iferr (call imputb (im, Memc[kp], bval)) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    ier = OK
	    IM_UPDATE(im) = YES
	}

	call sfree (sp)
end
