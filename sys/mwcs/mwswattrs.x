# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_SWATTRS -- Set the string value of the named WCS attribute for axis N.
# The attribute is created if not already defined.  If axis N=0 is specified,
# the attribute pertains to the entire WCS, not just one axis.

procedure mw_swattrs (mw, axis, attribute, valstr)

pointer	mw			#I pointer to MWCS descriptor
int	axis			#I axis to which attribute belongs
char	attribute[ARB]		#I attribute name
char	valstr[ARB]		#I attribute value

pointer	wp, ap
int	atno, i
bool	streq()
int	mw_refstr()
errchk	syserrs, mw_refstr

begin
	# Get current WCS.
	wp = MI_WCS(mw)
	if (wp == NULL)
	    call syserrs (SYS_MWNOWCS, "mw_swattrs")

	# Lookup the named attribute and replace the pointer to the value
	# string if found.  Otherwise, add a new attribute.

	atno = 0
	do i = 1, WCS_NWATTR(wp) {
	    ap = WCS_WATTR(wp,i)
	    if (AT_AXIS(ap) == axis)
		if (streq (S(mw,AT_NAME(ap)), attribute)) {
		    atno = i
		    break
		}
	}

	# Add a new attribute?
	if (atno == 0) {
	    atno = WCS_NWATTR(wp) + 1
	    if (atno > MAX_WATTR)
		call syserrs (SYS_MWATOVFL, attribute)
	    else {
		WCS_NWATTR(wp) = atno
		ap = WCS_WATTR(wp,atno)
		AT_AXIS(ap) = axis
		AT_NAME(ap) = mw_refstr (mw, attribute)
	    }
	}

	# Store the value string.
	AT_VALUE(ap) = mw_refstr (mw, valstr)
end
