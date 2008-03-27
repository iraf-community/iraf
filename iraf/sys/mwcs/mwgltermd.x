# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_GLTERMD -- Get the current Lterm, double precision version.

procedure mw_gltermd (mw, ltm, ltv, ndim)

pointer	mw			#I pointer to MWCS descriptor
double	ltm[ndim,ndim]		#O linear transformation matrix
double	ltv[ndim]		#O translation vector
int	ndim			#I dimensionality of system

size_t	sz_val
int	i
errchk	syserrs

begin
	# The dimensionality of the data must match that of the current Lterm.
	if (ndim != MI_NDIM(mw))
	    call syserrs (SYS_MWNDIM, "mw_gltermd")

	# Copy out the data.  Default to a unitary transformation if the
	# Lterm has not been initialized.

	sz_val = ndim*ndim
	if (MI_LTM(mw) == NULL) {
	    call aclrd (ltm, sz_val)
	    do i = 1, ndim
		ltm[i,i] = 1.0D0
	} else {
	    call amovd (D(mw,MI_LTM(mw)), ltm, sz_val)
	}

	sz_val = ndim
	if (MI_LTV(mw) == NULL) {
	    call aclrd (ltv, sz_val)
	} else {
	    call amovd (D(mw,MI_LTV(mw)), ltv, sz_val)
	}
end
