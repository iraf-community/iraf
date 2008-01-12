# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_GLTERMD -- Get the current Lterm, double precision version.

procedure mw_gltermd (mw, ltm, ltv, ndim)

pointer	mw			#I pointer to MWCS descriptor
double	ltm[ndim,ndim]		#O linear transformation matrix
double	ltv[ndim]		#O translation vector
int	ndim			#I dimensionality of system

int	i
errchk	syserrs

begin
	# The dimensionality of the data must match that of the current Lterm.
	if (ndim != MI_NDIM(mw))
	    call syserrs (SYS_MWNDIM, "mw_gltermd")

	# Copy out the data.  Default to a unitary transformation if the
	# Lterm has not been initialized.

	if (MI_LTM(mw) == NULL) {
	    call aclrd (ltm, ndim*ndim)
	    do i = 1, ndim
		ltm[i,i] = 1.0D0
	} else
	    call amovd (D(mw,MI_LTM(mw)), ltm, ndim*ndim)

	if (MI_LTV(mw) == NULL)
	    call aclrd (ltv, ndim)
	else
	    call amovd (D(mw,MI_LTV(mw)), ltv, ndim)
end
