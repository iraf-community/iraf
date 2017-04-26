include	<pkg/inlfit.h>


# IN_LIMIT -- Compute the independent variable limits for all variables,
# and store them in the INLFIT structure.

procedure in_limitr (in, x, npts, nvars)

pointer	in			# INLFIT descriptor
real	x[ARB]			# Independent values (npts * nvars)
int	npts			# number of points
int	nvars			# number of variables

int	i, j
real	aux, xmin, xmax
pointer	minptr, maxptr

pointer	in_getp()

begin
#	# Debug
#	call eprintf ("in_limit: in=%d, npts=%d, nvars=%d\n")
#	    call pargi (in)
#	    call pargi (npts)
#	    call pargi (nvars)

	# Get minimum and maximum buffer pointers
	minptr = in_getp (in, INLXMIN)
	maxptr = in_getp (in, INLXMAX)

	# Loop over variables
	do i = 1, nvars {

	    # Set initial values
	    xmin = x[i]
	    xmax = x[i]

	    # Search for maximum and minimum values
	    do j = 1, npts {
		aux = x[(j - 1) * nvars + i]
		if (xmin > aux)
		    xmin = aux
		else if (xmax < aux)
		    xmax = aux
	    }

	    # Enter values into the structure
	    Memr[minptr + i - 1] = xmin
	    Memr[maxptr + i - 1] = xmax
	}
end
