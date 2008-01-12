include "igi.h"

#  IG_MINMAX -- Print the data range ignoring INDEF

#  8/20/91 Removed ^Ls. ZGL
## 7/10/92 Added Z data and vector sizes.  ZGL

procedure ig_minmax (igs)

pointer	igs		# igi parameters structure

pointer	igps		# Plot parameters structure
real	mindat, maxdat
int	npts

begin
	call lcmdcat (igs, YES)
	call cmdcat (igs, NO)
	igps = PLOT_PARMS(igs)

	if (MG_XDATAP(igps) == NULL &&
	    MG_YDATAP(igps) == NULL &&
	    MG_ZDATAP(igps) == NULL) {
	    call eprintf ("Data arrays are NOT filled ")

	} else
	    call gdeactivate (GIO_GP(igs), 0)
	    call printf ("           Min          Max\n")

	    if (MG_XDATAP(igps) == NULL && MG_YDATAP(igps) != NULL) {
		# No X data;  use pixel numbers
		mindat = 1
		maxdat = MG_YNPTS(igps)
		npts   = 0

	    } else {
		# Find the range of the X vector
		npts = MG_XNPTS(igps)
		call iglimr (Memr[MG_XDATAP(igps)], npts, 
		    mindat, maxdat)
	    }

	    call printf ("X %12g %12g")
		call pargr (mindat)
		call pargr (maxdat)

	    if (npts > 0) {
	        call printf ("  (%d points)\n")
		    call pargi (npts)

	    } else
	        call printf ("\n")

	    if (MG_YDATAP(igps) == NULL) {
		call eprintf ("No Y data\n")

	    } else {
		# Find the range of the Y vector
		call iglimr (Memr[MG_YDATAP(igps)], MG_YNPTS(igps), 
		    mindat, maxdat)
		call printf ("Y %12g %12g  (%d points)\n")
		    call pargr (mindat)
		    call pargr (maxdat)
		    call pargi (MG_YNPTS(igps))
	    }

	    if (MG_ZDATAP(igps) == NULL) {
		call eprintf ("No Z image data\n")

	    } else {
		# Find the range of the Z vector
		call iglimr (Memr[MG_ZDATAP(igps)], MG_ZNPTS(igps), 
		    mindat, maxdat)
		call printf ("Z %12g %12g  (%d x %d)\n")
		    call pargr (mindat)
		    call pargr (maxdat)
		    call pargi (MG_ZNPTSX(igps))
		    call pargi (MG_ZNPTSY(igps))
	    }

	    call flush (STDOUT)
	    call greactivate (GIO_GP(igs), 0)
end


procedure iglimr (data, npts, mindat, maxdat)

# IGLIMR -- Compute the limits (minimum and maximum values) of a 
#           vector ignoring INDEFs.

real	data[ARB]
int	npts
real	mindat, maxdat

real	value
int	i

begin
	mindat = data[1]
	maxdat = data[1]

	do i = 1, npts {
	    value = data[i]
	    if (IS_INDEF(value))
		next
	    else if (value < mindat)
		mindat = value
	    else if (value > maxdat)
		maxdat = value
	}
end
