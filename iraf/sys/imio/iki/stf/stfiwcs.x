include	<imhdr.h>
include	"stf.h"

# STF_INITWCS -- Check for an unitialized WCS and set up a unitary pixel
# WCS in this case.

procedure stf_initwcs (im)

pointer	im			#I image descriptor

real	v
int	ndim, i, j
bool	have_wcs, wcsok
char	pname[SZ_KEYWORD]
bool	fp_equalr()
real	imgetr()

begin
	ndim = IM_NDIM(im)
	have_wcs = false
	wcsok = false

	# Scan the header to determine if we have any WCS information (assume
	# there is a WCS if any CDi_j cards are found) and if it has been
	# initialized (at least one matrix element is nonzero).  Note that
	# we are checking only to see if the WCS has been initialized, not
	# if it is a valid WCS.

	do j = 1, ndim {
	    do i = 1, ndim {
		call sprintf (pname, SZ_KEYWORD, "CD%d_%d")
		    call pargi (i)
		    call pargi (j)
		ifnoerr (v = imgetr (im, pname)) {
		    have_wcs = true
		    if (!fp_equalr (v, 0.0)) {
			wcsok = true
			break
		    }
		}
	    }
	    if (wcsok)
		break
	}

	# If we found some WCS information and the CD matrix is zero, init
	# the WCS.

	if (have_wcs && !wcsok)
	    do i = 1, ndim {
		call sprintf (pname, SZ_KEYWORD, "CTYPE%d")
		    call pargi (i)
		call imastr (im, pname, "PIXEL")

		call sprintf (pname,  SZ_KEYWORD, "CD%d_%d")
		    call pargi (i)
		    call pargi (i)
		call imaddr (im, pname, 1.0)
	    }
end
