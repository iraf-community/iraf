# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMGKWI -- Return the value of the named header keyword as an integer.

procedure imgkwi (im, keyw, ival, ier)

pointer	im			# imfort image descriptor
%       character*(*) keyw
int	ival
int	ier

pointer	sp, kp
int	imgeti(), errcode()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	iferr (ival = imgeti (im, Memc[kp])) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else
	    ier = OK

	call sfree (sp)
end
