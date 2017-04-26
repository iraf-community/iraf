include	<mach.h>
include	"apertures.h"

# AP_NEAREST -- Find the index of the aperture nearest cursor position x.

define	DELTA	0.01		# Tolerance for equidistant apertures

procedure ap_nearest (index, line, aps, naps, x)

int	index			# Index of aperture nearest x
int	line			# Dispersion line
pointer	aps[ARB]		# Aperture pointers
int	naps			# Number of apertures
real	x			# Point nearest aperture

int	i, j, apaxis
char	ch
real	d, delta
pointer	ap

int	fscan(), nscan()
real	ap_cveval()

begin
	if (naps == 0)
	    return

	index = 0
	delta = MAX_REAL

	for (i = 1; i <= naps; i = i + 1) {
	    ap = aps[i]
	    apaxis = AP_AXIS(ap)
	    d = abs (AP_CEN(ap, apaxis)+ap_cveval(AP_CV(ap),real(line))-x)
	    if (d < delta - DELTA) {
		j = 1
		index = i
		delta = d
	    } else if (d < delta + DELTA)
		j = j + 1
	}

	# If there is more than one aperture equally near ask the user.
	if (j > 1) {
	    call printf ("Apertures")
	    for (i = 1; i <= naps; i = i + 1) {
	        ap = aps[i]
	        apaxis = AP_AXIS(ap)
	        d = abs (AP_CEN(ap, apaxis)+ap_cveval(AP_CV(ap),real(line))-x)
	        if (d < delta + DELTA) {
		    call printf (" %d")
			call pargi (AP_ID (ap))
		}
	    }
	    call printf (" are equally near the cursor.\n")
10	    call printf ("Choose an aperture (%d): ")
		call pargi (AP_ID (aps[index]))
	    call flush (STDOUT)
	    if (fscan (STDIN) != EOF) {
		call scanc (ch)
		if (ch == '\n')
		    return

		call reset_scan()
		call gargi (j)
		if (nscan() == 0)
		    goto 10
		for (i=1; (i<=naps)&&(AP_ID(aps[i])!=j); i=i+1)
		    ;
		if (i > naps)
		    goto 10
		index = i
	    }
	}
end
