include	<smw.h>
include	"identify.h"

# ID_PEAK -- Find the peak value above continuum.

double procedure id_peak (id, pix)

pointer	id			# ID pointer
double	pix			# Pixel position
double	peak			# Peak value

int	c, l, u

begin
	if (IS_INDEFD(pix))
	    return (INDEFD)

	c = nint (pix)
	l = max (1, nint (pix - ID_FWIDTH(id)))
	u = min (ID_NPTS(id), nint (pix + ID_FWIDTH(id)))
	peak = IMDATA(id,c) - (IMDATA(id,l) + IMDATA(id,u)) / 2.

	return (peak)
end


# ID_PEAKS -- Find peaks in the data.  This just calls find_peaks but does
# the logical to physical pixel conversion.

int procedure id_peaks (id, data, x, npoints, contrast, separation, edge, nmax,
    threshold, debug)

pointer	id		#I Identify pointer
real	data[npoints]	#I Input data array
real	x[npoints]	#O Output peak position array
int	npoints		#I Number of data points
real	contrast	#I Maximum contrast between strongest and weakest
int	separation	#I Minimum separation between peaks
int	edge		#I Minimum distance from the edge
int	nmax		#I Maximum number of peaks to be returned
real	threshold	#I Minimum threshold level for peaks
bool	debug		#I Print diagnostic information?

int	i, n, np1, find_peaks()
double	smw_c1trand()
errchk	find_peaks

begin
	# Find the peaks in logical coordinates.
	n = find_peaks (data, x, npoints, contrast, separation, edge,
	    nmax, threshold, debug)

	# Convert to physical coordinates.
	np1 = NP1(ID_SH(id)) - 1
	do i = 1, n
	    x[i] = smw_c1trand (ID_LP(id), double (x[i]+np1))

	return (n)
end


# ID_UPEAKS -- Find uniformly distributed peaks in the data.  This just calls
# find_upeaks but does the logical to physical pixel conversion.

int procedure id_upeaks (id, data, x, npoints, contrast, separation, edge,
	nmax, nbins, threshold, debug)

pointer	id		#I Identify pointer
real	data[npoints]	#I Input data array
real	x[npoints]	#O Output peak position array
int	npoints		#I Number of data points
real	contrast	#I Maximum contrast between strongest and weakest
int	separation	#I Minimum separation between peaks
int	edge		#I Minimum distance from the edge
int	nmax		#I Maximum number of peaks to be returned
int	nbins		#I Number of bins across the data array
real	threshold	#I Minimum threshold level for peaks
bool	debug		#I Print diagnostic information?

int	i, n, np1, find_upeaks()
double	smw_c1trand()
errchk	find_upeaks

begin
	# Find the peaks in logical coordinates.
	n = find_upeaks (data, x, npoints, contrast, separation, edge,
	    nmax, nbins, threshold, debug)

	# Convert to physical coordinates.
	np1 = NP1(ID_SH(id)) - 1
	do i = 1, n
	    x[i] = smw_c1trand (ID_LP(id), double (x[i]+np1))

	return (n)
end
