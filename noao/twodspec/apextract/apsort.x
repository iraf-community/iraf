include	"apertures.h"

# Sort flags:
define	INC	1	# Sort by aperture position in increasing order
define	DEC	2	# Sort by position in decreasing order

# AP_SORT -- Sort the apertures.

procedure ap_sort (current, aps, naps, flag)

int	current			# Current aperture
pointer	aps[ARB]		# Aperture data
int	naps			# Number of apertures
int	flag			# Sort flag

int	i, j, apaxis
pointer	ap

begin
	if (naps < 1)
	    return

	switch (flag) {
	case INC:
	    apaxis = AP_AXIS (aps[1])
	    for (i = 1; i <= naps - 1; i = i + 1) {
	        for (j = i + 1; j <= naps; j = j + 1) {
		    if (AP_CEN(aps[i], apaxis) > AP_CEN(aps[j], apaxis)) {
		        ap = aps[i]
		        aps[i] = aps[j]
		        aps[j] = ap
		        if (current == i)
			    current = j
		        else if (current == j)
			    current = i
		    }
	        }
	    }
	case DEC:
	    apaxis = AP_AXIS (aps[1])
	    for (i = 1; i <= naps - 1; i = i + 1) {
	        for (j = i + 1; j <= naps; j = j + 1) {
		    if (AP_CEN(aps[i], apaxis) < AP_CEN(aps[j], apaxis)) {
		        ap = aps[i]
		        aps[i] = aps[j]
		        aps[j] = ap
		        if (current == i)
			    current = j
		        else if (current == j)
			    current = i
		    }
	        }
	    }
	}
end
