include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/find.h"
include "../lib/fitpsf.h"
include "../lib/radprof.h"


# AP_VTHRESHOLD -- Verify the full detection threshold.

real procedure ap_vthreshold (ap)

pointer	ap		# pointer to the apphot structure

real	skysigma, threshold
int	scan(), nscan()
real	apstatr()

begin
	skysigma = apstatr (ap, SKYSIGMA)

	# Confirm the threshold parameter.
	call printf (
	"Detection threshold in sigma (%g) (CR or value): ")
	    call pargr (apstatr (ap, THRESHOLD))
	call flush (STDOUT)
	if (scan() == EOF)
	    threshold = apstatr (ap, THRESHOLD)
	else {
	    call gargr (threshold)
	    if (nscan () != 1)
	        threshold = apstatr (ap, THRESHOLD)
	}

	call printf ("\tNew detection threshold: %g sigma %g counts\n")
	    call pargr (threshold)
	call apsetr (ap, THRESHOLD, threshold)
	if (IS_INDEFR(skysigma))
	    call pargr (INDEFR)
	else
	    call pargr (threshold * skysigma)

	return (threshold)
end

# AP_VPFSTRING -- Verify the psf fitting function.

procedure ap_vpfstring (ap, str, maxch)

pointer	ap		# pointer to the apphot structure
char	str[ARB]	# output string
int	maxch		# maximum number of characters

int	pfunc
int	scan(), nscan(), strdic()

begin
	# Print the old string value.
	call apstats (ap, PSFSTRING, str, maxch)
	call printf ("Fitting function (%s) (CR or value): ")
	    call pargstr (str)
	call flush (STDOUT)

	# Get the new value.
	if (scan() != EOF) {
	    call gargwrd (str, maxch)
	    pfunc = strdic (str, str, maxch, PSFFUNCS)
	    if (nscan () == 1 && pfunc > 0) {
		call apseti (ap, PSFUNCTION, pfunc)
		call apsets (ap, PSFSTRING, str)
	    }
	}

	# Print the new value.
	call apstats (ap, PSFSTRING, str, SZ_LINE)
	call printf ("\tNew fitting function: %s\n")
	    call pargstr (str)
end


# AP_VPSFAPERT -- Verify the point spread function fitting aperture.

real procedure ap_vpsfapert (ap)

pointer	ap		# pointer to the apphot strucuture

real	scale, psfapert
int	scan(), nscan()
real	apstatr()

begin
	scale = apstatr (ap, SCALE)

	# Print the old value.
	call printf ("Fitting box width in scale units (%g) (CR or value): ")
	    call pargr (2.0 * apstatr (ap, PSFAPERT))
	call flush (STDOUT)

	# Get the new value.
	if (scan() == EOF)
	    psfapert = 2.0 * apstatr (ap, PSFAPERT)
	else {
	    call gargr (psfapert)
	    if (nscan () != 1)
	        psfapert = 2.0 * apstatr (ap, PSFAPERT)
	}

	# Print the new  value.
	call printf ("\tNew fitting box width: %g scale units  %g pixels\n")
	    call pargr (psfapert)
	    call pargr (scale 	* psfapert)
	call apsetr (ap, PSFAPERT, psfapert / 2.0)

	return (psfapert / 2.0)
end


# AP_VRPRADIUS -- Verify the radial profile fitting radius.

real procedure ap_vrpradius (ap)

pointer	ap		# pointer to the apphot strucuture

real	scale, radius
int	scan(), nscan()
real	apstatr()

begin
	scale = apstatr (ap, SCALE)

	# Print the old value.
	call printf ("Fitting radius in scale units (%g) (CR or value): ")
	    call pargr (apstatr (ap, RPRADIUS))
	call flush (STDOUT)

	# Get the new value.
	if (scan() == EOF)
	    radius = apstatr (ap, RPRADIUS)
	else {
	    call gargr (radius)
	    if (nscan () != 1)
	        radius = apstatr (ap, RPRADIUS)
	}

	# Print the new  value.
	call printf ("\tNew fitting radius: %g scale units  %g pixels\n")
	    call pargr (radius)
	    call pargr (scale 	* radius)
	call apsetr (ap, RPRADIUS, radius)

	return (radius)
end


# AP_VSTEP -- Verify the profile step size.

real procedure ap_vstep (ap)

pointer	ap		# pointer to the apphot strucuture

real	scale, step
int	scan(), nscan()
real	apstatr()

begin
	scale = apstatr (ap, SCALE)

	# Print the old value.
	call printf ("Step size in scale units (%g) (CR or value): ")
	    call pargr (apstatr (ap, RPSTEP))
	call flush (STDOUT)

	# Get the new value.
	if (scan() == EOF)
	    step = apstatr (ap, RPSTEP)
	else {
	    call gargr (step)
	    if (nscan () != 1)
	        step = apstatr (ap, RPSTEP)
	}

	# Print the new  value.
	call printf ("\tNew step size: %g scale units  %g pixels\n")
	    call pargr (step)
	    call pargr (scale * step)
	call apsetr (ap, RPSTEP, step)

	return (step)
end
