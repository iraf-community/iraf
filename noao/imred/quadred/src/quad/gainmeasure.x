include <imhdr.h>
include "quadgeom.h"

define  OPT_REFLECT     4

# GAINMEASURE -- Calculate the gain (e/ADU) and RON of a CCD using the CTIO
# (Bruce Attwood) algorithm.
# Input are a pair of high signal level exposures (Flat1, Flat2) and a pair of
# zero exposures (Zero1, Zero2). We then calculate:
#
#    epadu = <Flat1> + <Flat2> - (<Zero1> + <Zero2>) / var{Diff_F} - Var{Diff_Z}
#    RON   = RMS {Diff_Z} * epadu / sqrt(2)
#
# Where:
#
#    diff_Z = Zero1 - Zero2
#    diff_F = Flat1 - Flat2
#
# The statistics must be calculated for regions free of bad pixels and other
# defects, and with reasonably uniform illumination. 

procedure t_gainmeasure ()

pointer	flat1, flat2		#TI High level images
pointer	zero1, zero2		#TI Zero level images
char	section[SZ_LINE]	#TI Section for calculation

char	buffer[SZ_LINE]
int	npix, x1, x2, y1, y2, amp
pointer	sp, f1, f2, z1, z2, fd, zd, qg
real	f1bar,   f2bar,   z1bar,   z2bar,   fdbar,   zdbar
real	f1sigma, f2sigma, z1sigma, z2sigma, fdsigma, zdsigma
real	div, epadu, ron
bool	headers

pointer	immap(), imgs2r()
bool	clgetb(), quadsect()
int	hdmaccf()

begin
        # Open instrument file
        call clgstr    ("instrument",  buffer,  SZ_FNAME)
        call hdmopen   (buffer)

	# Map input images
	call clgstr ("flat1",   buffer, SZ_LINE)
	flat1 = immap (buffer,  READ_ONLY, 0)

	call clgstr ("flat2",   buffer, SZ_LINE)
	flat2 = immap (buffer,  READ_ONLY, 0)

	call clgstr ("zero1",   buffer, SZ_LINE)
	zero1 = immap (buffer,  READ_ONLY, 0)

	call clgstr ("zero2",   buffer, SZ_LINE)
	zero2 = immap (buffer,  READ_ONLY, 0)

        # Get section over which measurement is to be made.
        call clgstr ("section", section, SZ_LINE)

	# See if headers are to be printed
	headers = clgetb ("print_headers")

        # Set-up quadgeom structure. We blithely assume all images are the same.
        call quadalloc (qg)

	if (hdmaccf (flat1, "HDR_REV") == NO) {
	    call quadgeom  (flat1, qg, "", "")
	} else {
	    call qghdr2 (flat1, qg)
	}
#       call quaddump  (qg)

	if (headers) {
	    call printf ("#")
	    do amp = 1, QG_NAMPS (qg) {
		call printf ("%9wAmp%2s%5w")
		    call pargstr (Memc[QG_AMPID (qg, amp)])
	    }
	    call printf ("\n")

	    call printf ("#")
	    do amp = 1, QG_NAMPS (qg) {
		call printf ("%5wGain%4wRON%3w")
	    }
	    call printf ("\n")
	    call printf ("#")
	    do amp = 1, QG_NAMPS (qg) {
		call printf ("%3w(e-/ADU)%2w(e-)%2w")
	    }
	    call printf ("\n")
	}

	call printf ("%1w")
        do amp = 1, QG_NAMPS (qg) {
 
            if (quadsect (qg, section, OPT_REFLECT, amp, x1, x2, y1, y2)) {

		npix = (abs(y2 - y1) + 1) * (abs(x2 - x1) + 1)

		# Allocate working arrays
		call smark  (sp)
		call salloc (fd, npix, TY_REAL)
		call salloc (zd, npix, TY_REAL)

		# Read data 
		f1 = imgs2r (flat1, x1, x2, y1, y2)
		f2 = imgs2r (flat2, x1, x2, y1, y2)
		z1 = imgs2r (zero1, x1, x2, y1, y2)
		z2 = imgs2r (zero2, x1, x2, y1, y2)

		# Calculate differences
		call asubr (Memr[f1], Memr[f2], Memr[fd], npix)
		call asubr (Memr[z1], Memr[z2], Memr[zd], npix)

		# Calculate means and standard deviations
		call aavgr (Memr[f1], npix, f1bar, f1sigma)
		call aavgr (Memr[f2], npix, f2bar, f2sigma)
		call aavgr (Memr[z1], npix, z1bar, z1sigma)
		call aavgr (Memr[z2], npix, z2bar, z2sigma)
		call aavgr (Memr[fd], npix, fdbar, fdsigma)
		call aavgr (Memr[zd], npix, zdbar, zdsigma)

#		call eprintf ("f1bar=%g f1sigma=%g\n")
#		    call pargr (f1bar)
#		    call pargr (f1sigma)
#		call eprintf ("f2bar=%g f2sigma=%g\n")
#		    call pargr (f2bar)
#		    call pargr (f2sigma)
#		call eprintf ("z1bar=%g z1sigma=%g\n")
#		    call pargr (z1bar)
#		    call pargr (z1sigma)
#		call eprintf ("z2bar=%g z2sigma=%g\n")
#		    call pargr (z2bar)
#		    call pargr (z2sigma)
#		call eprintf ("fdbar=%g fdsigma=%g\n")
#		    call pargr (fdbar)
#		    call pargr (fdsigma)
#		call eprintf ("zdbar=%g zdsigma=%g\n")
#		    call pargr (zdbar)
#		    call pargr (zdsigma)

		div  = fdsigma**2 - zdsigma**2
		if (div > 0.0) {
		    epadu = ((f1bar + f2bar) - (z1bar + z2bar)) / div
		    ron   = epadu * zdsigma / 1.41421356
		} else {
		    epadu = INDEF
		    ron   = INDEF
		}

		# Print results
		call printf ("%3w%6.2f%2w%6.2f%2w")
		    call pargr (epadu)
		    call pargr (ron)

	    # Free working arrays
	    call sfree (sp)

	    }
	}

	call printf ("\n")

	# Tidy up
	call imunmap (flat1)
	call imunmap (flat2)
	call imunmap (zero1)
	call imunmap (zero2)
end
