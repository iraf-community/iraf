# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMAKWR -- Add a new keyword of type real.

procedure imakwr (im, keyw, rval, comm, ier)

size_t	sz_val
pointer	im			# imfort image descriptor
%       character*(*) keyw
real	rval
%       character*(*) comm
int	ier

pointer	sp, kp, cp
int	errcode()

begin
	call smark (sp)
	sz_val = SZ_KEYWORD
	call salloc (kp, sz_val, TY_CHAR)
	sz_val = SZ_VALSTR
	call salloc (cp, sz_val, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	call f77upk (comm, Memc[cp], SZ_VALSTR)

	iferr (call imaddr (im, Memc[kp], rval, Memc[cp])) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    ier = OK
	    IM_UPDATE(im) = YES
	}

	call sfree (sp)
end
