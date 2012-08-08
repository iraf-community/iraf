# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

# IMGKWD -- Return the value of the named header keyword as a double.

procedure imgkwd (im, keyw, dval, ier)

pointer	im			# imfort image descriptor
%       character*(*) keyw
double	dval
int	ier

pointer	sp, kp
double	imgetd()
int	errcode()

begin
	call smark (sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)

	call f77upk (keyw, Memc[kp], SZ_KEYWORD)
	iferr (dval = imgetd (im, Memc[kp])) {
	    ier = errcode()
	    call im_seterrop (ier, Memc[kp])
	} else
	    ier = OK

	call sfree (sp)
end
