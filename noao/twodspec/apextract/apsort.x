include	"apertures.h"

# Sort flags:
define	ID	1	# Sort by aperture ID
define	POS	2	# Sort by aperture position

# AP_SORT -- Sort the apertures.  This routine is not
# called often or with a large number of apertures so we use the brute
# force method.  Return the new aperture index corresponding to the
# current aperture.

procedure ap_sort (current, aps, naps, flag)

int	current			# Current aperture
pointer	aps[ARB]		# Aperture data
int	naps			# Number of apertures
int	flag			# Sort flag

int	i, j, apaxis
pointer	ap

begin
	switch (flag) {
	case ID:
	    for (i = 1; i <= naps - 1; i = i + 1) {
	        for (j = i + 1; j <= naps; j = j + 1) {
	            if (AP_ID(aps[i]) > AP_ID(aps[j])) {
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
	case POS:
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
	}
end
