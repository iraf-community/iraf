include	"inlfitdef.h"


# IN_NLINIT -- Initialize (reinitialize) NLFIT descriptor. The new
# NLFIT descriptor is returned as a procedure argument.

procedure in_nlinitr (in, nl)

pointer	in			# INLFIT descriptor
pointer	nl			# NLFIT descriptor

errchk	nlinit(), nlfree()

begin
#	# Debug.
#	call eprintf ("in_nlinit: in=%d, nl=%d\n")
#	    call pargi (in)
#	    call pargi (nl)

	# Free old NLFIT structure if any.
	if (nl != NULL)
	    call nlfreer (nl)

	# Initialize new NLFIT structure.
	call nlinitr (nl, IN_FUNC (in), IN_DFUNC (in), Memr[IN_PARAM (in)],
	    Memr[IN_DPARAM (in)], IN_NPARAMS (in), Memi[IN_PLIST (in)],
	    IN_NFPARAMS (in), IN_TOLR (in), IN_MAXITER (in))
end
