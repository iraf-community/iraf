include	"apertures.h"
include	"extract.h"

# EX_APSTRIP -- Get strip of image data within aperture with possible
# background subtraction.

procedure ex_apstrip (data, npts, low, center, high, asi, ic, bkgd, strip, bkg,
	nstrip)

real	data[ARB]		# Pixel data
int	npts			# Number of data points
real	low, center, high	# Aperture
pointer	asi			# Image interpolation pointer
pointer	ic			# ICFIT pointer
int	bkgd			# Background subtraction flag
real	strip[nstrip]		# Output strip
real	bkg[nstrip]		# Output background
int	nstrip			# Number of pixels in strip

int	i, col1, col2, ncols, nfit
real	a, b, c, d, avg
pointer	cv, sp, str, x, y, w

int	ctor()
real	ic_getr(), cveval(), asieval(), asigrl()

begin
	call aclrr (strip, nstrip)
	call aclrr (bkg, nstrip)

	if ((high < low) || (high < 0.5) || (low >= npts+0.5))
	    return

	# Set the number of output points.
	col1 = nint (low)
	col2 = nint (high)
	ncols = min (nstrip, col2 - col1 + 1)

	# Fit image interpolation function including boundary points.
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

	a = low - col1 + 1
    	do i = 1, ncols {
	    if ((a >= 1) && (a <= nfit))
		strip[i] = asieval (asi, a)
	    a = a + 1.
	}

	# Determine background if desired.
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

	    avg = 0.
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
			        avg = avg + asigrl (asi, a, b)
				d = d + b - a
			    }
			}
		    }
		}
	    }
	    if (d > 0.) {
	        avg = avg / d

	        a = low
    	        do i = 1, ncols {
		    if ((a >= col1) && (a <= col2)) {
		        strip[i] = strip[i] - avg
		        bkg[i] = avg
		    }
		    a = a + 1.
		}
	    }

	    call sfree (sp)
	    
	case FIT:
	    call smark (sp)
	    call salloc (x, nfit, TY_REAL)
	    call salloc (y, nfit, TY_REAL)
	    call salloc (w, nfit, TY_REAL)

	    a = col1 + low - int (low)
	    do i = 1, nfit-1 {
	        Memr[x+i-1] = a - center
		Memr[y+i-1] = asieval (asi, a-col1+1)
		Memr[w+i-1] = 1.
		a = a + 1.
	    }

	    iferr {
	        call ic_fit (ic, cv, Memr[x], Memr[y], Memr[w], nfit-1,
	            YES, YES, YES, YES)

	        a = low
	        do i = 1, ncols {
    	            if ((a >= col1) && (a <= col2)) {
			bkg[i] = cveval (cv, a - center)
			strip[i] = strip[i] - bkg[i]
		    }
		    a = a + 1
	        }

	        call cvfree (cv)
	    } then
	        ;

	    call sfree (sp)
	}
end
