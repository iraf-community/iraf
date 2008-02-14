# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMACCK -- Test if the named keyword exists.  IER=0 is returned if the
# parameter exists.

procedure imacck (im, key, ier)

pointer	im			# image descriptor
%	character*(*) key
int	ier

pointer	sp, kp
int	imaccf()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)

	call f77upk (key, Memc[kp], SZ_KEYWORD)
	if (imaccf (im, Memc[kp]) == YES)
	    ier = OK
	else {
	    ier = IE_NEXKW
	    call im_seterrop (ier, Memc[kp])
	}

	call sfree (sp)
end
