# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_GWATTRS -- Get the string value of the named WCS attribute for axis N.
# If the attribute name is a number N, attribute number N is returned instead,
# allowing the attributes to be listed without knowing their names.

procedure mw_gwattrs (mw, axis, attribute, valstr, maxch)

pointer	mw			#I pointer to MWCS descriptor
int	axis			#I axis to which attribute belongs
char	attribute[SZ_ATNAME]	#U attribute name
char	valstr[ARB]		#O attribute value
int	maxch			#I max chars to output value string

pointer	wp, ap
int	item, atno, i

int	ctoi()
bool	streq()
errchk	syserrs

begin
	# Get current WCS.
	wp = MI_WCS(mw)
	if (wp == NULL)
	    call syserrs (SYS_MWNOWCS, "mw_gwattrs")

	# Get attribute number if number was given.
	i = 1
	if (ctoi (attribute, i, atno) == 0)
	    atno = 0

	# Lookup the named or numbered attribute and output the value
	# string if found.

	item = 0
	do i = 1, WCS_NWATTR(wp) {
	    ap = WCS_WATTR(wp,i)
	    if (AT_AXIS(ap) == axis) {
		item = item + 1
		if (atno > 0) {
		    if (atno == item) {
			call strcpy (S(mw,AT_NAME(ap)), attribute, SZ_ATNAME)
			call strcpy (S(mw,AT_VALUE(ap)), valstr, maxch)
			return
		    }
		} else if (streq (S(mw,AT_NAME(ap)), attribute)) {
		    call strcpy (S(mw,AT_VALUE(ap)), valstr, maxch)
		    return
		}
	    }
	}

	call syserrs (SYS_MWWATTRNF, attribute)
end
