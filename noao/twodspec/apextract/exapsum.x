include	"apertures.h"
include	"extract.h"


# EX_APSUM -- Sum image data within aperture.  Subtract background if needed.

procedure ex_apsum (data, npts, low, center, high, asi, ic, bkgd, sum, bkg)

real	data[ARB]		# Pixel data to be summed
int	npts			# Number of data points
real	low, center, high	# Aperture
pointer	asi			# Image interpolator pointer
pointer	ic			# ICFIT pointer
int	bkgd			# Background flag
real	sum			# Output sum
real	bkg			# Output background

int	i, col1, col2, nfit
real	f, a, b, c, d
pointer	cv, sp, str, x, y, w

int	ctor()
real	ic_getr(), cveval(), asieval(), asigrl()

begin
	sum = 0.
	bkg = 0.

	if ((high < low) || (high < 0.5) || (low >= npts+0.5))
	    return

	# Fit image interpolation function and integrate.
	if (bkgd != NONE && ic != NULL) {
	    a = min (low, ic_getr (ic, "xmin") + center)
	    b = max (high, ic_getr (ic, "xmax") + center)
	} else {
	    a = low
	    b = high
	}

	col1 = max (1, nint (a) - 1)
	col2 = min (npts, nint (b) + 1)
	nfit = col2 - col1 + 1
	iferr (call asifit (asi, data[col1], nfit))
	    return

	a = max (real (col1), low) - col1 + 1
	b = min (real (col2), high) - col1 + 1
	sum = asigrl (asi, a, b)
	f = b - a

	# Determine background in desired.
	if (bkgd == NONE || ic == NULL)
	    return

	switch (bkgd) {
	case AVERAGE:
	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)
	    call ic_gstr (ic, "sample", Memc[str], SZ_LINE)
	    for (i=str; Memc[i]!=EOS; i=i+1)
		if (Memc[i] == ':')
		    Memc[i] = '$'

	    d = 0.
	    for (i=1; Memc[str+i-1]!=EOS; i=i+1) {
	        if (ctor (Memc[str], i, a) > 0) {
		    i = i - 1
		    if (Memc[str+i] == '$') {
			i = i + 2
			if (ctor (Memc[str], i, b) > 0) {
			    i = i - 1
			    if (a > b) {
				c = a
				a = b
				b = c
			    }
			    a = max (real (col1), a+center) - col1 + 1
			    b = min (real (col2), b+center) - col1 + 1
			    if (b - a != 0.) {
			        bkg = bkg + asigrl (asi, a, b)
				d = d + b - a
			    }
			}
		    }
		}
	    }
	    if (d > 0.) {
	        bkg = f * bkg / d
	        sum = sum - bkg
	    }

	    call sfree (sp)
	    
	case FIT:
	    call smark (sp)
	    call salloc (x, nfit, TY_REAL)
	    call salloc (y, nfit, TY_REAL)
	    call salloc (w, nfit, TY_REAL)

	    a = col1 + center - int (center)
	    do i = 1, nfit-1 {
	        Memr[x+i-1] = a - center
		Memr[y+i-1] = asieval (asi, a-col1+1)
		Memr[w+i-1] = 1.
		a = a + 1.
	    }

	    iferr {
	        call ic_fit (ic, cv, Memr[x], Memr[y], Memr[w], nfit-1,
	            YES, YES, YES, YES)

		# Numerically integrate background.
		a = max (real (col1), low) - center
		b = min (real (col2), high) - center
		c = (b - a)
		i = 2 * c
		if (i > 1)
		    c = c / i
		bkg = cveval (cv, a) / 2
		for (d=a+c; d<(b-.01); d=d+c)
		    bkg = bkg + cveval (cv, d)
		bkg = bkg + 0.5 * cveval (cv, b)
		bkg = c * bkg
		sum = sum - bkg

	        call cvfree (cv)
	    } then
	        ;

	    call sfree (sp)
	}
end
