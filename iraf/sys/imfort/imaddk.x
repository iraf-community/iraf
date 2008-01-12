# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMADDK -- Add a new keyword to the image header.

procedure imaddk (im, keyw, dtype, comm, ier)

pointer	im			# imfort image descriptor
%       character*(*) keyw
int	dtype
%	character*(*) comm
int	ier

pointer	sp, kp, cp
int	errcode()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)
	call salloc (cp, SZ_LINE, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	call f77upk (comm, Memc[cp], SZ_LINE)

	iferr (call imaddf (im, Memc[kp], dtype, Memc[cp])) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    ier = OK
	    IM_UPDATE(im) = YES
	}

	call sfree (sp)
end
