# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMAKWB -- Add a new keyword of type bool.

procedure imakwb (im, keyw, bval, comm, ier)

pointer	im			# imfort image descriptor
%       character*(*) keyw
bool	bval
%       character*(*) comm
int	ier

pointer	sp, kp, cp
int	errcode()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)
	call salloc (cp, SZ_VALSTR, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	call f77upk (comm, Memc[cp], SZ_VALSTR)

	iferr (call imaddb (im, Memc[kp], bval, Memc[cp])) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    ier = OK
	    IM_UPDATE(im) = YES
	}

	call sfree (sp)
end
