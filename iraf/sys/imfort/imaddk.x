# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMADDK -- Add a new keyword to the image header.

procedure imaddk (im, keyw, dtype, comm, ier)

size_t	sz_val
pointer	im			# imfort image descriptor
%       character*(*) keyw
int	dtype
%	character*(*) comm
int	ier

pointer	sp, kp, cp
int	errcode()

begin
	call smark (sp)
	sz_val = SZ_KEYWORD
	call salloc (kp, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (cp, sz_val, TY_CHAR)

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
