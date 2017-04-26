# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_GWTERMD -- Get the linear part of the Wterm, i.e., the physical and world
# coordinates of the reference point and the CD matrix.  It is the Wterm of
# the current default WCS which is read.

procedure mw_gwtermd (mw, r, w, cd, ndim)

pointer	mw			#I pointer to MWCS descriptor
double	r[ndim]			#O physical coordinates of reference point
double	w[ndim]			#O world coordinates of reference point
double	cd[ndim,ndim]		#O CD matrix
int	ndim			#I dimension of Wterm

pointer	wp
errchk	syserrs
string	s_name "mw_gwtermd"

begin
	# Get the current WCS.
	wp = MI_WCS(mw)
	if (wp == NULL)
	    call syserrs (SYS_MWNOWCS, s_name)

	# Verify the dimension.
	if (WCS_NDIM(wp) != ndim)
	    call syserrs (SYS_MWNDIM, s_name)

	# Copy out the data.  Return the unitary transformation if the
	# Wterm has not been set.

	if (WCS_R(wp) == NULL)
	    call aclrd (r, ndim)
	else
	    call amovd (D(mw,WCS_R(wp)), r, ndim)

	if (WCS_W(wp) == NULL)
	    call aclrd (w, ndim)
	else
	    call amovd (D(mw,WCS_W(wp)), w, ndim)

	if (WCS_CD(wp) == NULL)
	    call mw_mkidmd (cd, ndim)
	else
	    call amovd (D(mw,WCS_CD(wp)), cd, ndim*ndim)
end
