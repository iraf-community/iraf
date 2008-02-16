# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMAKWC -- Add a new keyword of type string.

procedure imakwc (im, keyw, sval, comm, ier)

size_t	sz_val
pointer	im			# imfort image descriptor
%       character*(*) keyw
%	character*(*) sval
%	character*(*) comm
int	ier

pointer	sp, kp, vp, cp
int	errcode()

begin
	call smark (sp)
	sz_val = SZ_KEYWORD
	call salloc (kp, sz_val, TY_CHAR)
	sz_val = SZ_VALSTR
	call salloc (vp, sz_val, TY_CHAR)
	call salloc (cp, sz_val, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	call f77upk (sval, Memc[vp], SZ_VALSTR)
	call f77upk (comm, Memc[cp], SZ_VALSTR)

	iferr (call imastr (im, Memc[kp], Memc[vp], Memc[cp])) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    ier = OK
	    IM_UPDATE(im) = YES
	}

	call sfree (sp)
end
