# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMGKWC -- Return the value of the named header keyword as a character
# string.

procedure imgkwc (im, keyw, sval, ier)

size_t	sz_val
pointer	im			# imfort image descriptor
%       character*(*) keyw
%	character*(*) sval
int	ier

pointer	sp, kp, vp
int	errcode()

begin
	call smark (sp)
	sz_val = SZ_KEYWORD
	call salloc (kp, sz_val, TY_CHAR)
	sz_val = SZ_VALSTR
	call salloc (vp, sz_val, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	iferr (call imgstr (im, Memc[kp], Memc[vp], SZ_VALSTR)) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    call f77pak (Memc[vp], sval, ARB)
	    ier = OK
	}

	call sfree (sp)
end
