# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_SLTERMD -- Set the Lterm, double precision version.  Since all floating
# data is stored as double internally, we merely copy the data in.

procedure mw_sltermd (mw, ltm, ltv, ndim)

pointer	mw			#I pointer to MWCS descriptor
double	ltm[ndim,ndim]		#I linear transformation matrix
double	ltv[ndim]		#I translation vector
int	ndim			#I dimensionality of system

size_t	sz_val
long	mw_allocd()
errchk	syserrs, mw_allocd

begin
	# The dimensionality of the data must match that of the current Lterm.
	if (ndim != MI_NDIM(mw))
	    call syserrs (SYS_MWNDIM, "mw_sltermd")

	# Copy in the data.  Cobber the old data if the Lterm has been set,
	# otherwise allocate space in the global data area.

	sz_val = ndim*ndim
	if (MI_LTM(mw) == NULL)
	    MI_LTM(mw) = mw_allocd (mw, sz_val)
	call amovd (ltm, D(mw,MI_LTM(mw)), sz_val)

	sz_val = ndim
	if (MI_LTV(mw) == NULL)
	    MI_LTV(mw) = mw_allocd (mw, sz_val)
	call amovd (ltv, D(mw,MI_LTV(mw)), sz_val)
end
