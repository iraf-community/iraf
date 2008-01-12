# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_NEWSYSTEM -- Add a new world coordinate system with the given name
# and dimensionality to the MWCS.  Make the new system the current system,
# since a number of WCS initialization calls will (should) surely follow.

procedure mw_newsystem (mw, system, ndim)

pointer	mw			#I pointer to MWCS descriptor
char	system[ARB]		#I system name
int	ndim			#I system dimensionality

pointer	wp
int	mw_refstr()
pointer	mw_findsys()
errchk	syserrs, mw_refstr

begin
	# Check that the system does not already exist.
	if (mw_findsys (mw, system) != NULL)
	    call syserrs (SYS_MWWCSREDEF, system)

	# Add the new system.
	if (MI_NWCS(mw) + 1 > MAX_WCS)
	    call syserrs (SYS_MWMAXWCS, system)
	MI_NWCS(mw) = MI_NWCS(mw) + 1
	wp = MI_WCSP(mw,MI_NWCS(mw))

	# Initialize the WCS.
	WCS_NDIM(wp) = ndim

	# Make the new WCS the default WCS.
	MI_WCS(mw) = wp

	# The system name is stored as a global (axis=0) attribute of the WCS.
	call mw_swattrs (mw, 0, "system", system)
	WCS_SYSTEM(wp) = mw_refstr (mw, system)
end
