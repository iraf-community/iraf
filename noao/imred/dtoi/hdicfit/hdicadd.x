include	"hdicfit.h"

# HDIC_ADDPOINT -- Add a density, exposure, weights point into the sample.

procedure hdic_addpoint (ic,
	nden, nexp, nwts, den, y, wts, userwts, x, wd, sdev, npts)

pointer	ic		# Pointer to ic data structure
real	nden		# New density value to be added
real	nexp		# New exposure value to be added
double	nwts		# New weight value to be added
pointer	den		# Pointer to existing density array
pointer	y		# Pointer to existing exposure array
pointer	wts		# Pointer to existing wts array
pointer	userwts		# Pointer to existing userwts array
pointer	x		# Pointer to existing array of ind variables
pointer	wd		# Pointer to flag array for deletion reasons
pointer	sdev		# Pointer to standard deviation array
int	npts		# Number of points, incremented on output

begin
	npts = npts + 1

	call realloc (den,     npts, TY_DOUBLE)
	call realloc (y,       npts, TY_DOUBLE)
	call realloc (wts,     npts, TY_DOUBLE)
	call realloc (userwts, npts, TY_DOUBLE)
	call realloc (x,       npts, TY_DOUBLE)
	call realloc (wd,      npts, TY_INT)
	call realloc (sdev,    npts, TY_DOUBLE)

	Memd[den+npts-1] = double (nden)
	Memd[y  +npts-1] = double (nexp)
	Memd[wts+npts-1] = double (nwts)
	Memd[userwts+npts-1] = double (nwts)
	Memi[wd +npts-1] = NDELETE
	Memd[sdev+npts-1] = ADDED_PT

	# Sort the data and then update the reference vector.
	call hdic_sort (Memd[den], Memd[y], Memd[wts], Memd[userwts], 
	    Memi[wd], Memd[sdev],  npts)
	call hdic_init (Memd[den], npts, Memd[den+npts-1])

	IC_NEWX(ic) = YES
	IC_NEWY(ic) = YES
	IC_NEWWTS(ic) = YES
end
