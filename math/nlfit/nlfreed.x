include "nlfitdefd.h"

# NLFREE -- Deallocate all assigned space

procedure nlfreed (nl)

pointer	nl		# pointer to non-linear fitting structure

errchk	mfree

begin
	if (nl == NULL)
	    return
	if (NL_PARAM(nl) != NULL)
	    call mfree (NL_PARAM(nl), TY_DOUBLE)
	if (NL_OPARAM(nl) != NULL)
	    call mfree (NL_OPARAM(nl), TY_DOUBLE)
	if (NL_DPARAM(nl) != NULL)
	    call mfree (NL_DPARAM(nl), TY_DOUBLE)
	if (NL_DELPARAM(nl) != NULL)
	    call mfree (NL_DELPARAM(nl), TY_DOUBLE)
	if (NL_PLIST(nl) != NULL)
	    call mfree (NL_PLIST(nl), TY_INT)
	if (NL_ALPHA(nl) != NULL)
	    call mfree (NL_ALPHA(nl), TY_DOUBLE)
	if (NL_CHOFAC(nl) != NULL)
	    call mfree (NL_CHOFAC(nl), TY_DOUBLE)
	if (NL_COVAR(nl) != NULL)
	    call mfree (NL_COVAR(nl), TY_DOUBLE)
	if (NL_BETA(nl) != NULL)
	    call mfree (NL_BETA(nl), TY_DOUBLE)
	if (NL_TRY(nl) != NULL)
	    call mfree (NL_TRY(nl), TY_DOUBLE)
	if (NL_DERIV(nl) != NULL)
	    call mfree (NL_DERIV(nl), TY_DOUBLE)
	call mfree (nl, TY_STRUCT)
end
