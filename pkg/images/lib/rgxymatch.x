include <mwset.h>

# RG_RXYL -- Compute the grid of logical coordinates.

procedure rg_rxyl (xl, yl, nx, ny, x1, x2, y1, y2)

double	xl[ARB]			#O array of output x coordinates
double	yl[ARB]			#O array of output y coordinates
int	nx			#I the size of the grid in x
int	ny			#I the size of the grid in y
double	x1			#I the lower limit of the grid in x
double	x2			#I the upper limit of the grid in x
double	y1			#I the lower limit of the grid in y
double	y2			#I the upper limit of the grid in y

double	xstep, ystep, x, y
int	i, j, npts

begin
	if (nx == 1)
	    xstep = 0.0d0
	else
	    xstep = (x2 - x1) / (nx - 1)
	if (ny == 1)
	    ystep = 0.0d0
	else
	    ystep = (y2 - y1) / (ny - 1)
	npts = 0

	y = y1
	do j = 1, ny {
	    x = x1
	    do i = 1, nx {
		npts = npts + 1
		xl[npts] = x
		yl[npts] = y
		x = x + xstep
	    }
	    y = y + ystep
	}
end


# RG_XYTOXY -- Compute the world coordinate list give the wcs descriptor
# and the logical coordinates.

pointer procedure rg_xytoxy (mw, xl, yl, xw, yw, npts, inwcs, outwcs, ax1, ax2)

pointer	mw			#I the wcs descriptor
double	xl[ARB]			#I the input logical x coordinate
double	yl[ARB]			#I the input logical y coordinate
double	xw[ARB]			#O the output world x coordinate
double	yw[ARB]			#O the output world y coordinate
int	npts			#I the number of coordinates.
char	inwcs[ARB]		#I the input wcs
char	outwcs[ARB]		#I the output wcs
int	ax1			#I the logical x axis
int	ax2			#I the logical y axis

int	i, axbits
pointer	ct
double	mw_c1trand()
int	mw_stati()
pointer	mw_sctran()
errchk	mw_sctran()

begin
	# Compile the transformation.
	if (mw == NULL) {
	    ct = NULL
	} else if (mw_stati (mw, MW_NDIM) >= 2) {
	    axbits = 2 ** (ax1 - 1) + 2 ** (ax2 - 1)
	    iferr (ct = mw_sctran (mw, inwcs, outwcs, axbits))
	        ct = NULL
	} else {
	    axbits = 2 ** (ax1 - 1)
	    iferr (ct = mw_sctran (mw, inwcs, outwcs, axbits))
	        ct = NULL
	}

	# Compute the world coordinates.
	if (ct == NULL) {
	    call amovd (xl, xw, npts)
	    call amovd (yl, yw, npts)
	} else if (mw_stati (mw, MW_NDIM) == 2) {
	    do i = 1, npts
		call mw_c2trand (ct, xl[i], yl[i], xw[i], yw[i])
	} else {
	    do i = 1, npts {
		xw[i] = mw_c1trand (ct, xl[i])
		yw[i] = yl[i]
	    }
	}

	return (ct)
end

