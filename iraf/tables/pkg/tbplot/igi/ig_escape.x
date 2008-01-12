include "igi.h"

procedure ig_escape (igs)

pointer	igs		# Parameters structure

pointer	tokvals		# Token value structure

begin
	tokvals = TOKEN_VALUE(igs)

	if (Memc[LOP_VALP(tokvals)+1] == EOS)
	    return

	call gdeactivate (GIO_GP(igs), 0)
	call clcmdw (Memc[LOP_VALP(tokvals)+1])
	call greactivate (GIO_GP(igs), 0)
	call lcmdcat (igs, YES)
#	call cmdcat (igs, NO)
end
