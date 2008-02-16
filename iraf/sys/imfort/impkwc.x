# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMPKWC -- Set the value of the named header keyword as a character string.

procedure impkwc (im, keyw, sval, ier)

pointer	im			# imfort image descriptor
%       character*(*) keyw
%	character*(*) sval
int	ier

pointer	sp, kp, vp
int	errcode()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)
	call salloc (vp, SZ_VALSTR, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	call f77upk (sval, Memc[vp], SZ_VALSTR)
	iferr (call impstr (im, Memc[kp], Memc[vp])) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    ier = OK
	    IM_UPDATE(im) = YES
	}

	call sfree (sp)
end
