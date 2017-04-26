# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMGKWB -- Return the value of the named header keyword as a boolean.

procedure imgkwb (im, keyw, bval, ier)

pointer	im			# imfort image descriptor
%       character*(*) keyw
bool	bval
int	ier

pointer	sp, kp
bool	imgetb()
int	errcode()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	iferr (bval = imgetb (im, Memc[kp])) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else
	    ier = OK

	call sfree (sp)
end
