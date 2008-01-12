# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_SWTERMR -- Set the linear part of the Wterm, i.e., the physical and world
# coordinates of the reference point and the CD matrix.  It is the Wterm of
# the current default WCS which is affected.

procedure mw_swtermr (mw, r, w, cd, ndim)

pointer	mw			#I pointer to MWCS descriptor
real	r[ndim]			#I physical coordinates of reference point
real	w[ndim]			#I world coordinates of reference point
real	cd[ndim,ndim]		#I CD matrix
int	ndim			#I dimension of Wterm

pointer	wp
int	nelem
pointer	mw_allocd()
errchk	mw_allocd, syserrs
string	s_name "mw_swtermr"

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
	call achtrd (r, D(mw,WCS_R(wp)), ndim)

	if (WCS_W(wp) == NULL)
	    WCS_W(wp) = mw_allocd (mw, ndim)
	call achtrd (w, D(mw,WCS_W(wp)), ndim)

	nelem = ndim * ndim
	if (WCS_CD(wp) == NULL)
	    WCS_CD(wp) = mw_allocd (mw, nelem)
	call achtrd (cd, D(mw,WCS_CD(wp)), nelem)
end
