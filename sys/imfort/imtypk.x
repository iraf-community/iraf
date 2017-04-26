# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMTYPK -- Get the datatype and comment string for a keyword.

procedure imtypk (im, keyw, dtype, comm, ier)

pointer	im			# image descriptor
%	character*(*) keyw
int	dtype			# receives datatype code
%	character*(*) comm
int	ier

pointer	sp, kp, cp
int	errcode()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)
	call salloc (cp, SZ_VALSTR, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	iferr (call imgatr (im, Memc[kp], dtype, Memc[cp], len(comm))) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else {
	    call f77pak (Memc[cp], comm, len(comm))
	    ier = OK
	}

	call sfree (sp)
end
