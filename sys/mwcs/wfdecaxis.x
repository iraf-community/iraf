# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"


# WF_DECAXIS -- Determine which of the 2 axes for the current function is
# the DEC axis.

procedure wf_decaxis (fc, ira, idec)

pointer	fc			#I pointer to function call descriptor
int	ira, idec		#O CTRAN relative RA, DEC axis numbers

pointer	ct, mw
int	ax[2], i
char	axtype[4]
bool	streq()

begin
	ct = FC_CT(fc)
	mw = CT_MW(ct)

	# This function requires exactly 2 axes.
	if (FC_NAXES(fc) != 2)
	    call error (1, "A projection WCS requires 2 axes")

	# Map FC axis (1 or 2) to CTRAN axis to physical axis.
	do i = 1, 2
	    ax[i] = CT_AXIS(ct,FC_AXIS(fc,i))

	# Determine which is the DEC/LAT axis, and hence the axis order.
	ira = 0
	idec = 0
	do i = 1, 2
	    ifnoerr (call mw_gwattrs (mw, ax[i], "axtype", axtype, 4)) {
		call strlwr (axtype)
		if (streq (axtype, "ra") || streq (axtype[2], "lon")) {
		    ira  = i
		    idec = 3 - i
		    break
		} else if (streq (axtype, "dec") || streq (axtype[2], "lat")) {
		    ira  = 3 - i
		    idec = i
		    break
		} 
	    }

	if (idec == 0)
	    call error (2,
	    "DEC/xLAT axis must be specified for a projection WCS")
end
