include	"apertures.h"

# AP_RESIZE -- Resize apertures.

procedure ap_resize (image, line, nsum, aps, naps, apedit)

char	image[SZ_FNAME]		# Image name
int	line			# Image dispersion line
int	nsum			# Number of dispersion lines to sum
int	apedit			# Called from apedit?

pointer	aps[ARB]		# Aperture pointers
int	naps			# Number of apertures

real	llimit, ulimit		# Maximum aperture limits
real	ylevel			# Fraction of intensity for resize
bool	peak			# Is ylevel a fraction of the peak?
bool	bkg			# Subtract background?
real	grow			# Expand limits by this factor
bool	avglimits		# Average limits?

real	center, low, high
int	i, na, npts, apaxis
pointer	sp, str, im, imdata, title

bool	clgetb(), ap_answer(), apgetb()
real	apgetr(), ap_cveval()
errchk	ap_getdata

begin
	# Check if apertures are defined.
	na = 0
	do i = 1, naps
	    if (AP_SELECT(aps[i]) == YES)
		na = na + 1
	if (na == 0)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	if (apedit == NO) {
	    call sprintf (Memc[str], SZ_LINE, "Resize apertures for %s?")
	        call pargstr (image)
	    if (!ap_answer ("ansresize", Memc[str])) {
	        call sfree (sp)
	        return
	    }

	    if (clgetb ("verbose"))
	        call printf ("Resizing apertures ...\n")
	}

	# Map the image and get the image data.
	call ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)

	# Resize the apertures.
	llimit = apgetr ("llimit")
	ulimit = apgetr ("ulimit")
	ylevel = apgetr ("ylevel")
	bkg = apgetb ("bkg")
	peak = apgetb ("peak")
	grow = apgetr ("r_grow")
	avglimits = apgetb ("avglimits")

	if (IS_INDEF(llimit))
	    llimit = -npts
	if (IS_INDEF(ulimit))
	    ulimit = npts
	
	high = max (llimit, ulimit)
	llimit = min (llimit, ulimit)
	ulimit = high

	if (IS_INDEF (ylevel)) {
	    do i = 1, naps {
		if (AP_SELECT(aps[i]) == YES) {
		    AP_LOW(aps[i], apaxis) = llimit
		    AP_HIGH(aps[i], apaxis) = ulimit
		}
	    }
	    avglimits = true
	} else {
	    do i = 1, naps {
		if (AP_SELECT(aps[i]) == YES) {
		    low = llimit
		    high = ulimit
		    center = AP_CEN(aps[i], apaxis) +
			ap_cveval (AP_CV(aps[i]), real (line))
		    call ap_ylevel (Memr[imdata], npts, ylevel, peak, bkg, grow,
			center, low, high)
		    AP_LOW(aps[i], apaxis) = min (low, high)
		    AP_HIGH(aps[i], apaxis) = max (low, high)
		}
	    }

	    if (avglimits) {
		low = 0.
		high = 0.
		do i = 1, naps {
		    if (AP_SELECT(aps[i]) == YES) {
			low = low + AP_LOW(aps[i], apaxis)
			high = high + AP_HIGH(aps[i], apaxis)
		    }
		}
		low = low / na
		high = high / na
		do i = 1, naps {
		    if (AP_SELECT(aps[i]) == YES) {
			AP_LOW(aps[i], apaxis) = low
			AP_HIGH(aps[i], apaxis) = high
		    }
		}
	    }
	}

	# Log the operation, write the apertures to the database,
	# unmap the image and free memory.
	if (na == 1 || avglimits) {
	    call sprintf (Memc[str], SZ_LINE,
        	"APRESIZE  - %d apertures resized for %s (%.2f, %.2f)")
	        call pargi (na)
	        call pargstr (image)
		call pargr (AP_LOW(aps[1], apaxis))
		call pargr (AP_HIGH(aps[1], apaxis))
	} else {
	    call sprintf (Memc[str], SZ_LINE,
	        "RESIZE - %d apertures resized for %s")
	        call pargi (na)
	        call pargstr (image)
	}
	if (apedit == NO)
	    call ap_log (Memc[str], YES, YES, NO)
	else
	    call ap_log (Memc[str], YES, NO, NO)

	call appstr ("ansdbwrite1", "yes")

	call mfree (imdata, TY_REAL)
	call mfree (title, TY_CHAR)
	call imunmap (im)
	call sfree (sp)
end
