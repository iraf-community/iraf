include "daoedit.h"

# DP_CBANNER -- Pritnt the confirm banner.

procedure dp_cbanner ()

begin
	call printf ("\nVERIFY THE NEW VALUES\n")
end


# DP_CFWHMPSF -- Confirm the new value of fwhmpsf.

procedure dp_cfwhmpsf()

real	fwhmpsf, scale, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	fwhmpsf = clgetr ("datapars.fwhmpsf")

	# Confirm the fwhmpsf.
	call printf ("FWHM of features (%g scale units) (CR or value): ")
	    call pargr (fwhmpsf)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan() == 1)
	        fwhmpsf = rval
	}
	call printf ("\tNew FWHM of features: %g scale units  %g pixels\n")
	    call pargr (fwhmpsf)
	    call pargr (scale * fwhmpsf)

	# Store the new fwhmpsf.
	call clputr ("datapars.fwhmpsf", fwhmpsf)
end


# DP_CSIGMA -- Confirm the sigma parameters.

procedure dp_csigma()

real	sigma, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current value.
	sigma = clgetr ("datapars.sigma")

	# Confirm the sky sigma.
	call printf (
	    "Standard deviation of background (%g counts) (CR or value): ")
	    call pargr (sigma)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        sigma = rval
	}
	call printf ("\tNew standard deviation of background: %g counts\n")
	    call pargr (sigma)

	# Store the new sky sigma.
	call clputr ("datapars.sigma", sigma)
end


# DP_CDMIN -- Confirm the good data minimum value.

procedure dp_cdmin ()

real	datamin, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current value.
	datamin = clgetr ("datapars.datamin")

	# Confirm the new minimum good data value.
	call printf (
	"Minimum good data value (%g counts) (CR or value): ")
	    call pargr (datamin)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        datamin = rval
	}
	call printf ("\tNew good data minimum: %g counts\n")
	    call pargr (datamin)

	# Store the new good data minimum.
	call clputr ("datapars.datamin", datamin)
end


# DP_CDMAX -- Confirm the good data maximum value.

procedure dp_cdmax ()

real	datamax, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current value.
	datamax = clgetr ("datapars.datamax")

	# Confirm the new maximum good data value.
	call printf (
	"Maximum good data value (%g counts) (CR or value): ")
	    call pargr (datamax)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        datamax = rval
	}
	call printf ("\tNew good data maximum: %g counts\n")
	    call pargr (datamax)

	# Store the new maximum good data value.
	call clputr ("datapars.datamax", datamax)
end


# DP_CCBOX -- Confirm the cbox parameter.

procedure dp_ccbox()

real	scale, cbox, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	cbox = clgetr ("centerpars.cbox")

	# Confirm the centering box value.
	call printf ("Centering box width (%g scale units) (CR or value): ")
	    call pargr (cbox)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        cbox = rval
	}
	call printf ("\tNew centering box width: %g scale units  %g pixels\n")
	    call pargr (cbox)
	    call pargr (scale * cbox)

	# Store the new centering box.
	call clputr ("centerpars.cbox", cbox)
end


# DP_CRCLEAN -- Confirm the cleaning radius.

procedure dp_crclean()

real	scale, rclean, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	rclean = clgetr ("centerpars.rclean")

	# Confirm the cleaning radius..
	call printf ("Cleaning radius (%g scale units) (CR or value): ")
	    call pargr (rclean)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        rclean = rval
	}
	call printf ("\tNew cleaning radius: %g scale units  %g pixels\n")
	    call pargr (rclean)
	    call pargr (scale * rclean)

	# Store the new cleaning radius.
	call clputr ("centerpars.rclean", rclean)
end


# DP_CRCLIP -- Confirm the clipping radius.

procedure dp_crclip()

real	scale, rclip, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	rclip = clgetr ("centerpars.rclip")

	# Confirm the cleaning radius..
	call printf ("Clipping radius (%g scale units) (CR or value): ")
	    call pargr (rclip)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        rclip = rval
	}
	call printf ("\tNew clipping radius: %g scale units  %g pixels\n")
	    call pargr (rclip)
	    call pargr (scale * rclip)

	# Store the new clipping radius.
	call clputr ("centerpars.rclip", rclip)
end


# DP_CANNULUS -- Confirm the inner radius of the sky annulus.

procedure dp_cannulus()

real	scale, annulus, rval
int	scan(), nscan()
real	clgetr()

begin 
	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	annulus = clgetr ("fitskypars.annulus")

	# Confirm the sky annulus.
	call printf (
	    "Inner radius of sky annulus (%g scale units) (CR or value): ")
            call pargr (annulus)
	call flush (STDOUT)
	if (scan () != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        annulus = rval
	}
	call printf (
	    "\tNew inner radius of sky annulus: %g scale units %g pixels\n")
	    call pargr (annulus)
	    call pargr (scale * annulus)

	# Store the new sky annulus.
	call clputr ("fitskypars.annulus", annulus)
end


# DP_CDANNULUS -- Confirm the annulus width parameter.

procedure dp_cdannulus()

real	scale, dannulus, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	dannulus = clgetr ("fitskypars.dannulus")

	# Confirm the sky annulus width.
	call printf (
	    "Width of the sky annulus (%g scale units) (CR or value): ")
	    call pargr (dannulus)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        dannulus = rval
	}
	call printf (
	    "\tNew width of the sky annulus: %g scale units %g pixels\n")
	    call pargr (dannulus)
	    call pargr (scale * dannulus)

	# Save the new sky annulus width.
	call clputr ("fitskypars.dannulus", dannulus)
end


# DP_CRGROW -- Confirm the region growing radius.

procedure dp_crgrow()

real	scale, rgrow, rval
int	scan(), nscan()
real	clgetr()

begin
	# Mark the region growing radius.
	scale = 1.0 / clgetr ("datapars.scale")
	rgrow = clgetr ("fitskypars.rgrow")

	# Confirm the new region growing radius.
	call printf (
	    "Region growing radius (%g scale units) (CR or value): ")
	    call pargr (rgrow)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        rgrow = rval
	}
	call printf (
	    "\tNew region growing radius: %g scale units %g pixels\n")
	    call pargr (rgrow)
	    call pargr (scale * rgrow)

	# Save the new region growing radius.
	call clputr ("fitskypars.rgrow", rgrow)
end


# DP_CAPER -- Confirm the aperture string.

procedure dp_caper()

int	i, naperts
pointer	sp, apstr, newapstr, aperts
real	scale
int	scan(), nscan(), dp_gaperts()
real	clgetr()

begin
	call smark (sp)
	call salloc (apstr, SZ_LINE, TY_CHAR)
	call salloc (newapstr, SZ_LINE, TY_CHAR)
	call salloc (aperts, MAX_NAPERTS, TY_REAL)

	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	call clgstr ("photpars.apertures", Memc[apstr], SZ_LINE)

	# Confirm the aperture string.
	call printf (
	"File/list of aperture radii (%s scale units) (CR or value): ")
	    call pargstr (Memc[apstr])
	call flush (STDOUT)

	# Get the new apertures.
	if (scan() != EOF) {
	    call gargwrd (Memc[newapstr], SZ_LINE)
	    if (nscan () == 1)
		call strcpy (Memc[newapstr], Memc[apstr], SZ_LINE)
	}

	# Print the new apertures.
	naperts = dp_gaperts (Memc[apstr], Memr[aperts], MAX_NAPERTS)
	do i = 1, naperts {
	    call printf ("\tAperture radius %d: %g scale units %g pixels\n")
		call pargi (i)
		call pargr (Memr[aperts+i-1])
		call pargr (scale * Memr[aperts+i-1])
	}

	# Save the new aperture string.
	call clpstr ("photpars.apertures", Memc[apstr])

	call sfree (sp)
end


# DP_CPSFRAD -- Confirm the psf radius.

procedure dp_cpsfrad()

real	scale, psfrad, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	psfrad = clgetr ("daopars.psfrad")

	# Confirm the new PSF radius.
	call printf ("PSF radius (%g scale units) (CR or value): ")
	    call pargr (psfrad)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        psfrad = rval
	}
	call printf ("\tNew PSF radius: %g scale units  %g pixels\n")
	    call pargr (psfrad)
	    call pargr (scale * psfrad)

	# Store the new PSF radius.
	call clputr ("daopars.psfrad", psfrad)
end


# DP_CFITRAD -- Confirm the fitting radius.

procedure dp_cfitrad ()

real	scale, fitrad, rval
int	scan(), nscan()
real	clgetr()

begin
	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	fitrad = clgetr ("daopars.fitrad")

	# Confirm the new fitting radius.
	call printf ("Fitting radius (%g scale units) (CR or value): ")
	    call pargr (fitrad)
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargr (rval)
	    if (nscan () == 1)
	        fitrad = rval
	}
	call printf ("\tNew fitting radius: %g scale units  %g pixels\n")
	    call pargr (fitrad)
	    call pargr (scale * fitrad)

	# Store the new fitting radius.
	call clputr ("daopars.fitrad", fitrad)
end
