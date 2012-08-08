# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_SWTERMD -- Set the linear part of the Wterm, i.e., the physical and world
# coordinates of the reference point and the CD matrix.  It is the Wterm of
# the current default WCS which is affected.

procedure mw_swtermd (mw, r, w, cd, ndim)

pointer	mw			#I pointer to MWCS descriptor
double	r[ndim]			#I physical coordinates of reference point
double	w[ndim]			#I world coordinates of reference point
double	cd[ndim,ndim]		#I CD matrix
int	ndim			#I dimension of Wterm

pointer	wp
pointer	mw_allocd()
errchk	mw_allocd, syserrs
string	s_name "mw_swtermd"

begin
	# Get the current WCS.
	wp = MI_WCS(mw)
	if (wp == NULL)
	    call syserrs (SYS_MWNOWCS, s_name)

	# Verify the dimension.
	if (WCS_NDIM(wp) != ndim)
	    call syserrs (SYS_MWNDIM, s_name)

	# Copy in the data.  Cobber the old data if the Wterm has been set,
	# otherwise allocate space in the global data area.

	if (WCS_R(wp) == NULL)
	    WCS_R(wp) = mw_allocd (mw, ndim)
	call amovd (r, D(mw,WCS_R(wp)), ndim)

	if (WCS_W(wp) == NULL)
	    WCS_W(wp) = mw_allocd (mw, ndim)
	call amovd (w, D(mw,WCS_W(wp)), ndim)

	if (WCS_CD(wp) == NULL)
	    WCS_CD(wp) = mw_allocd (mw, ndim*ndim)
	call amovd (cd, D(mw,WCS_CD(wp)), ndim*ndim)
end
