include	<error.h>
include	<imhdr.h>
include	<mach.h>
include	"disptable.h"
 
# Output formats.
define	FORMATS	"|multispec|onedspec|sum|average|maximum|"
define	MULTISPEC	1	# Multispec output format
define	ONEDSPEC	2	# Individual spectra
define	SUM		3	# Combine by summing
define	AVERAGE		4	# Combine by averaging
define	MAXIMUM		5	# Combine by maximum
 
# T_MSDISPCOR -- Dispersion correct multispec spectra.
# The input multispec spectra are corrected to a linear (or logarithmically
# linear) wavelength scale.  The dispersion function is defined either
# from reference spectra specified by the image header parameters
# REFSPEC1 and REFSPEC2 or from linear wavelength if the spectra have
# been previously dispersion corrected.  The former requires that the
# specified reference spectra have dispersion function in the database
# generated by IDENTIFY.  The latter requires that the "rebin" flag
# be set.
# 
# There are three types of output formats.  There is a linearized version
# of the multispec format with the linear wavelength parameters stored in
# the APNUM keywords.  There is a one dimensional format where each aperture
# is output as a separate spectrum.  And there is one dimensional format
# where the apertures are combined by summing, averaging, or taking the
# maximum value of any overlapping data.
# 
# The linear wavelength scale may be specfied by any combination of
# starting and ending wavelength, wavelength interval, and number of
# pixels.  Suitable defaults may be used from the limits of the
# dispersion solutions.  The wavelength scale may be set globally for all
# spectra, globally by aperture by using a wavelength table for the
# defaults, and individually.  The user may confirm and change the
# wavelength scale.
# 
# The spectra are interpolated using one of the standard image
# interpolation types and the flux may be conserved by integrating the
# interpolation function.
 
procedure t_msdispcor ()
 
pointer	odrin			# List of input spectra
pointer	odrout			# List of output spectra
pointer	db			# Dispersion solution database
bool	log			# Log scale?
bool	flux			# Conserve flux?
 
int	i, format, naps, clgwrd(), odr_getim(), odr_len()
pointer	sp, input, output, str, table
bool	clgetb()
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (db, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Task parameters
	call odr_open1 ("input", "", odrin)
	call odr_open1 ("output", "", odrout)
	call clgstr ("database", Memc[db], SZ_FNAME)
	format = clgwrd ("format", Memc[str], SZ_LINE, FORMATS)
	log = clgetb ("log")
	flux = clgetb ("flux")
 
	# Initialize the database cacheing and wavelength table.
	call ms_open()
	call dc_table (table, naps)
	switch (format) {
	case MULTISPEC, ONEDSPEC:
	    if (clgetb ("global"))
	        call ms_global (odrin, Memc[db], log, table, naps)
	default:
	    if (clgetb ("global"))
	        call ms_global1 (odrin, Memc[db], log, table, naps)
	}
 
	# Loop through each input image.  Do the dispersion correction
	# in place if no output spectrum list is given or if the input
	# and output spectra names are the same.
 
	while (odr_getim (odrin, Memc[input], SZ_FNAME) != EOF) {
	    if (odr_len (odrout) > 0) {
		if (odr_getim (odrout, Memc[output], SZ_FNAME) == EOF)
		    break
	    } else
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
 
	    switch (format) {
	    case MULTISPEC:
		call ms_multispec (Memc[input], Memc[output], Memc[db],
		    log, flux, table, naps)
	    case ONEDSPEC:
		call ms_onedspec (Memc[input], Memc[output], Memc[db],
		    log, flux, table, naps)
	    default:
		call ms_combine (Memc[input], Memc[output], Memc[db],
		    format, log, flux, table, naps)
	    }
	}
 
	# Finish up.
	do i = 0, naps
	    call mfree (Memi[table+i], TY_STRUCT)
	call mfree (table, TY_INT)
	call odr_close (odrin)
	call odr_close (odrout)
	call ms_close()
	call sfree (sp)
end
 
 
# MS_COMBINE -- Combine multispec apertures into a ONEDSPEC format spectrum.
# The combine options are summing, averaging, or taking the maximum of the
# overlap.
 
procedure ms_combine (input, output, db, combine, log, flux, table, naps)
 
char	input[ARB]		# Input multispec spectrum
char	output[ARB]		# Output root name
char	db[ARB]			# Database
int	combine			# Combining option
bool	log			# Log wavelength parameters?
bool	flux			# Conserve flux?
pointer	table			# Wavelength table
int	naps			# Number of apertures
 
int	i, j, ap, nw
real	w1, w2, dw
double	ms_eval()
pointer	sp, temp, str, in, out, indata, outdata, spec, count
pointer	ms_gspec(), immap(), imgl2r(), impl1r()
bool	rebin, clgetb(), streq()
errchk	ms_gspec
extern  ms_eval
 
include	"msdispcor.com"
 
begin
	# Get the multispec spectrum.
	in = ms_gspec (input, db, ap, i)
	if (in == NULL)
	    return
 
	# Skip spectra not satisfying the rebin option.
	rebin = clgetb ("rebin")
	if ((rebin && i == -1) || (!rebin && i != -1)) {
	    call imunmap (in)
	    return
	}
 
	# Get the linear wavelength parameters.  Check for errors.
	call ms_wavelengths1 (in, output, log, table, naps, w1, w2, dw, nw)
	if ((dw*(w2-w1) <= 0.) || (nw < 1)) {
	    call eprintf ("Error in wavelength scale")
	    call imunmap (in)
	    return
	}
 
	if (clgetb ("listonly")) {
	    call imunmap (in)
	    return
	}
 
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Use a temporary image if the output has the same name as the input.
	if (streq (input, output)) {
	    call mktemp ("temp", Memc[temp], SZ_FNAME)
	    iferr (out = immap (Memc[temp], NEW_COPY, in)) {
		call imunmap (in)
		call sfree (sp)
		call erract (EA_WARN)
		return
	    }
	} else {
	    iferr (out = immap (output, NEW_COPY, in)) {
		call imunmap (in)
		call sfree (sp)
		call erract (EA_WARN)
		return
	    }
	}
 
	# Set output spectrum to be one dimensional.  Clear the output
	# array for combining.  Go through each aperture and linearize it.
	# Then add the aperture to the composite output spectrum.
 
	IM_NDIM(out) = 1
	IM_LEN(out,1) = nw
	outdata = impl1r (out)
	call aclrr (Memr[outdata], IM_LEN(out,1))
	call calloc (spec, nw, TY_REAL) 
	call calloc (count, nw, TY_INT) 
 
	do i = 1, IM_LEN(in, 2) {
	    line = i
	    indata = imgl2r (in, i)
	    call dispcor (Memr[indata], IM_LEN(in,1), Memr[spec],
		w1, dw, nw, log, flux, ms_eval)
	    switch (combine) {
	    case SUM, AVERAGE:
		do j = 1, nw {
		    if (Memr[spec+j-1] != 0.) {
		        Memr[outdata+j-1] = Memr[outdata+j-1] +
			    Memr[spec+j-1]
		        Memi[count+j-1] = Memi[count+j-1] + 1
		    }
		}
	    case MAXIMUM:
		do j = 1, nw {
		    if (Memr[spec+j-1] != 0.) {
			if (Memi[count+j-1] == 0) {
		            Memr[outdata+j-1] = Memr[spec+j-1]
		            Memi[count+j-1] = 1
			} else {
		            Memr[outdata+j-1] = max (Memr[outdata+j-1],
			        Memr[spec+j-1])
			}
		    }
		}
	    }
	}
 
	do i = 1, nw
	    if (Memi[count+i-1] > 1)
		Memr[outdata+i-1] = Memr[outdata+i-1] / Memi[count+i-1]
	call mfree (spec, TY_REAL)
	call mfree (count, TY_REAL)
 
	# Set the image header to ONEDSPEC format.
	do i = 1, IM_LEN(in,2) {
	    call sprintf (Memc[str], SZ_LINE, "APNUM%d")
		call pargi (i)
	    call imdelf (out, Memc[str])
	}
	call imastr (out, "APFORMAT", "onedspec")
	call imaddr (out, "CRPIX1", 1.)
	call imaddr (out, "CRVAL1", w1)
	call imaddr (out, "CDELT1", dw)
	call imaddr (out, "CD1_1", dw)
	call imaddr (out, "W0", w1)
	call imaddr (out, "WPC", dw)
	call imaddi (out, "NP1", 0)
	call imaddi (out, "NP2", IM_LEN(out,1))
	if (log)
	    call imaddi (out, "DC-FLAG", 1)
	else
	    call imaddi (out, "DC-FLAG", 0)
 
	# Finish up.  Replace input by output if needed.
	call imunmap (in)
	call imunmap (out)
	if (streq (input, output)) {
	    call imdelete (input)
	    call imrename (Memc[temp], output)
	}
 
	call sfree (sp)
end
 
 
# MS_MULTISPEC -- Linearize multispec apertures into an MULTISPEC format
# spectrum.  The number of pixels in each image line is the maximum
# required to contain the longest spectrum.
 
procedure ms_multispec (input, output, db, log, flux, table, naps)
 
char	input[ARB]		# Input multispec spectrum
char	output[ARB]		# Output root name
char	db[ARB]			# Database
bool	log			# Log wavelength parameters?
bool	flux			# Conserve flux?
pointer	table			# Wavelength table
int	naps			# Number of apertures
 
int	i, j, nc, nl, nwmax, imgeti(), ahivi()
double	ms_eval()
pointer	sp, temp, str, str1, in, out, indata, outdata, ap, w1, w2, dw, nw
pointer	ms_gspec(), immap(), imgl2r(), impl2r()
bool	rebin, clgetb(), streq()
errchk	ms_gspec
extern  ms_eval
 
include	"msdispcor.com"
 
begin
	# Get the input multispec spectrum.
	in = ms_gspec (input, db, j, i)
	if (in == NULL)
	    return
	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
 
	# Skip spectra not satisfying the rebin option.
	rebin = clgetb ("rebin")
	if ((rebin && i == -1) || (!rebin && i != -1)) {
	    call imunmap (in)
	    return
	}
 
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (ap, nl, TY_INT)
	call salloc (w1, nl, TY_REAL)
	call salloc (w2, nl, TY_REAL)
	call salloc (dw, nl, TY_REAL)
	call salloc (nw, nl, TY_INT)
 
	# Determine the wavelength parameters for each aperture.
	# The options are to have all apertures have the same dispersion
	# or have each aperture have independent dispersion.  The global
	# parameters have already been calculated if needed.
 
	do i = 1, nl {
	    call sprintf (Memc[str], SZ_LINE, "APNUM%d")
		call pargi (i)
	    Memi[ap+i-1] = imgeti (in, Memc[str])
	}
 
	if (clgetb ("samedisp")) {
	    call ms_wavelengths1 (in, output, log, table, naps, Memr[w1],
		Memr[w2], Memr[dw], Memi[nw])
	    if ((Memr[dw]*(Memr[w2]-Memr[w1]) <= 0.) || (Memi[nw] < 1)) {
	        call eprintf ("Error in wavelength scale")
	        call imunmap (in)
		call sfree (sp)
	        return
	    }
	    call amovkr (Memr[w1], Memr[w1], nl)
	    call amovkr (Memr[w2], Memr[w2], nl)
	    call amovkr (Memr[dw], Memr[dw], nl)
	    call amovki (Memi[nw], Memi[nw], nl)
	} else {
	    do i = 1, nl {
	        call ms_wavelengths (in, output, log, table, naps,
		    i, Memi[ap+i-1], Memr[w1+i-1], Memr[w2+i-1], Memr[dw+i-1],
		    Memi[nw+i-1])
	    }
	}
	nwmax = ahivi (Memi[nw], nl)
 
	if (clgetb ("listonly")) {
	    call imunmap (in)
	    call sfree (sp)
	    return
	}
 
	# Use a temporary image if the output has the same name as the input.
	if (streq (input, output)) {
	    call mktemp ("temp", Memc[temp], SZ_LINE)
	    iferr (out = immap (Memc[temp], NEW_COPY, in)) {
		call imunmap (in)
		call sfree (sp)
		call erract (EA_WARN)
		return
	    }
	} else {
	    iferr (out = immap (output, NEW_COPY, in)) {
		call imunmap (in)
		call sfree (sp)
		call erract (EA_WARN)
		return
	    }
	}
	IM_LEN(out,1) = nwmax
 
	# Linearize each aperture and enter parameters in header.
	do i = 1, nl {
	    indata = imgl2r (in, i)
	    outdata = impl2r (out, i)
	    call aclrr (Memr[outdata], IM_LEN(out,1))
 
	    line = i
	    call dispcor (Memr[indata], nc, Memr[outdata],
		Memr[w1+i-1], Memr[dw+i-1], Memi[nw+i-1], log, flux, ms_eval)
 
	    call imastr (out, "APFORMAT", "multispec")
	    call sprintf (Memc[str], SZ_LINE, "APNUM%d")
		call pargi (i)
	    call sprintf (Memc[str1], SZ_LINE, "%d %d %8g %8g %d")
		call pargi (Memi[ap+i-1])
		call pargi (Memi[ap+i-1])
		call pargr (Memr[w1+i-1])
		call pargr (Memr[dw+i-1])
		call pargi (Memi[nw+i-1])
	    call imastr (out, Memc[str], Memc[str1])
	}
 
	if (clgetb ("samedisp")) {
	    call imaddr (out, "CRPIX1", 1.)
	    call imaddr (out, "CRVAL1", Memr[w1])
	    call imaddr (out, "CDELT1", Memr[dw])
	    call imaddr (out, "CD1_1", Memr[dw])
	    call imaddr (out, "W0", Memr[w1])
	    call imaddr (out, "WPC", Memr[dw])
	    call imaddi (out, "NP1", 0)
	    call imaddi (out, "NP2", Memi[nw])
	}
	if (log)
	    call imaddi (out, "DC-FLAG", 1)
	else
	    call imaddi (out, "DC-FLAG", 0)
 
	# Finish up.  Replace input by output if needed.
	call imunmap (in)
	call imunmap (out)
	if (streq (input, output)) {
	    call imdelete (input)
	    call imrename (Memc[temp], output)
	}
 
	call sfree (sp)
end
 

# MS_ONEDSPEC -- Linearize each aperture into a separate ONEDSPEC spectrum.
 
procedure ms_onedspec (input, output, db, log, flux, table, naps)
 
char	input[ARB]		# Input multispec spectrum
char	output[ARB]		# Output root name
char	db[ARB]			# Database
bool	log			# Log wavelength parameters?
bool	flux			# Conserve flux?
pointer	table			# Wavelength table
int	naps			# Number of apertures
 
int	i, j, nc, nl, imgeti()
double	ms_eval()
pointer	sp, temp, str, in, out, indata, outdata, ap, w1, w2, dw, nw
pointer	ms_gspec(), immap(), imgl2r(), impl1r()
bool	rebin, clgetb()
errchk	ms_gspec
extern  ms_eval
 
include	"msdispcor.com"
 
begin
	# Get the multispec spectrum.
	in = ms_gspec (input, db, j, i)
	if (in == NULL)
	    return
	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
 
	# Skip spectra not satisfying the rebin option.
	rebin = clgetb ("rebin")
	if ((rebin && i == -1) || (!rebin && i != -1)) {
	    call imunmap (in)
	    return
	}
 
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (ap, nl, TY_INT)
	call salloc (w1, nl, TY_REAL)
	call salloc (w2, nl, TY_REAL)
	call salloc (dw, nl, TY_REAL)
	call salloc (nw, nl, TY_INT)
 
	# Determine the wavelength parameters for each aperture.
	# The options are to have all apertures have the same dispersion
	# or have each aperture have independent dispersion.  The global
	# parameters have already been calculated if needed.
 
	do i = 1, nl {
	    call sprintf (Memc[str], SZ_LINE, "APNUM%d")
		call pargi (i)
	    Memi[ap+i-1] = imgeti (in, Memc[str])
	}
 
	if (clgetb ("samedisp")) {
	    call sprintf (Memc[temp], SZ_FNAME, "%s.*")
		call pargstr (output)
 
	    call ms_wavelengths1 (in, Memc[temp], log, table, naps, Memr[w1],
		Memr[w2], Memr[dw], Memi[nw])
	    if ((Memr[dw]*(Memr[w2]-Memr[w1]) <= 0.) || (Memi[nw] < 1)) {
	        call eprintf ("Error in wavelength scale")
	        call imunmap (in)
		call sfree (sp)
	        return
	    }
	    call amovkr (Memr[w1], Memr[w1], nl)
	    call amovkr (Memr[w2], Memr[w2], nl)
	    call amovkr (Memr[dw], Memr[dw], nl)
	    call amovki (Memi[nw], Memi[nw], nl)
	} else {
	    do i = 1, nl {
	        call sprintf (Memc[temp], SZ_FNAME, "%s.%04d")
		    call pargstr (output)
		    call pargi (Memi[ap+i-1])
 
	        call ms_wavelengths (in, Memc[temp], log, table, naps,
		    i, Memi[ap+i-1], Memr[w1+i-1], Memr[w2+i-1], Memr[dw+i-1],
		    Memi[nw+i-1])
	    }
	}
 
	if (clgetb ("listonly")) {
	    call imunmap (in)
	    call sfree (sp)
	    return
	}
 
	# For each aperture create a ONEDSPEC spectrum and linearize the
	# aperture.
 
	do i = 1, nl {
	    call sprintf (Memc[temp], SZ_FNAME, "%s.%04d")
		call pargstr (output)
		call pargi (Memi[ap+i-1])
	    iferr (out = immap (Memc[temp], NEW_COPY, in)) {
		 call erract (EA_WARN)
		 next
	    }
 
	    IM_NDIM(out) = 1
	    IM_LEN(out,1) = Memi[nw+i-1]
	    indata = imgl2r (in,i)
	    outdata = impl1r (out)
	    call aclrr (Memr[outdata], IM_LEN(out,1))
 
	    line = i
	    call dispcor (Memr[indata], nc, Memr[outdata],
		 Memr[w1+i-1], Memr[dw+i-1], Memr[nw+i-1], log, flux, ms_eval)
 
	    do j = 1, nl {
	        call sprintf (Memc[str], SZ_LINE, "APNUM%d")
		    call pargi (j)
	        call imdelf (out, Memc[str])
	    }
	    call imastr (out, "APFORMAT", "onedspec")
	    call imaddi (out, "BEAM-NUM", Memi[ap+i-1])
	    call imaddr (out, "CRPIX1", 1.)
	    call imaddr (out, "CRVAL1", Memr[w1+i-1])
	    call imaddr (out, "CDELT1", Memr[dw+i-1])
	    call imaddr (out, "CD1_1", Memr[dw+i-1])
	    call imaddr (out, "W0", Memr[w1+i-1])
	    call imaddr (out, "WPC", Memr[dw+i-1])
	    call imaddi (out, "NP1", 0)
	    call imaddi (out, "NP2", IM_LEN(out,1))
	    if (log)
		call imaddi (out, "DC-FLAG", 1)
	    else
		call imaddi (out, "DC-FLAG", 0)
	    call imunmap (out)
	}
 
	# Finish up.
	call imunmap (in)
	call sfree (sp)
end
 
 
# MS_GLOBAL1 -- Set global wavelength parameters using the minimum and
# maximum wavelengths and and the minimum dispersion over all apertures.
# This is used for the COMBINE option format.
 
procedure ms_global1 (odrin, db, log, table, naps)
 
pointer	odrin			# Input list
char	db[ARB]			# Database
bool	log			# Logarithmic scale?
pointer	table			# Wavelength table
int	naps			# Number of apertures
 
int	i, ap, nw, nwmax, odr_getim()
real	w1, w2, dw, wmin, wmax, dwmin
double	ms_eval()
pointer	sp, input, str, in, tbl, ms_gspec()
errchk	ms_gspec
 
include	"msdispcor.com"
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	wmin = MAX_REAL
	wmax = -MAX_REAL
	dwmin = MAX_REAL
 
	# Go through all the reference spectra and determine the
	# minimum and maximum wavelengths and minimum dispersion.
 
	while (odr_getim (odrin, Memc[input], SZ_FNAME) != EOF) {
	    in = ms_gspec (Memc[input], db, ap, i)
	    if (in == NULL)
		next
 
	    nw = IM_LEN(in,1)
	    do i = 1, IM_LEN(in,2) {
		line = i
		w1 = ms_eval (double (1.))
		w2 = ms_eval (double (nw))
		dw = abs ((w2 - w1) / (nw - 1))
		wmin = min (wmin, w1, w2)
		wmax = max (wmax, w1, w2)
		dwmin = min (dwmin, dw)
	    }
	    call imunmap (in)
	}
	call odr_rew (odrin)
 
	nwmax = (wmax - wmin) / dwmin + 1
	if (log) {
	    wmin = log10 (wmin)
	    wmax = log10 (wmax)
	}
 
	call dc_getentry (false, 1, table, naps, i)
	tbl = Memi[table+i]
	call dc_defaults (wmin, wmax, nwmax,
	    TBL_W1(tbl), TBL_W2(tbl), TBL_DW(tbl), TBL_NW(tbl))
 
	call sfree (sp)
end
 
 
# MS_GLOBAL -- Set global wavelength parameters.  This is done for each
# aperture separately.  The wavelength table may be used to specify separate
# fixed parameters for each aperture.
 
procedure ms_global (odrin, db, log, table, naps)
 
pointer	odrin			# Input list
char	db[ARB]			# Database
bool	log			# Logarithmic scale?
pointer	table			# Wavelength table
int	naps			# Number of apertures
 
int	i, j, ap, nw, odr_getim(), imgeti()
real	w1, w2
double	ms_eval()
pointer	sp, input, str, in, tbl, ms_gspec()
errchk	ms_gspec, imgeti
 
include	"msdispcor.com"
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Go through all the reference spectra and determine the
	# minimum and maximum wavelengths and maximum number of pixels.
	# Do this by aperture.  If there is no entry in the wavelength
	# table add it.
 
	while (odr_getim (odrin, Memc[input], SZ_FNAME) != EOF) {
	    in = ms_gspec (Memc[input], db, ap, i)
	    if (in == NULL)
		next
 
	    nw = IM_LEN(in,1)
	    do j = 1, IM_LEN(in, 2) {
		call sprintf (Memc[str], SZ_LINE, "APNUM%d")
		    call pargi (j)
		ap = imgeti (in, Memc[str])
		line = j
 
		call dc_getentry (false, ap, table, naps, i)
		tbl = Memi[table+i]
 
		w1 = ms_eval (double (1.))
		w2 = ms_eval (double (nw))
		TBL_WMIN(tbl) = min (TBL_WMIN(tbl), w1, w2)
		TBL_WMAX(tbl) = max (TBL_WMAX(tbl), w1, w2)
		TBL_NWMAX(tbl) = max (TBL_NWMAX(tbl), nw)
	    }
	    call imunmap (in)
	}
	call odr_rew (odrin)
 
	do i = 1, naps {
	    tbl = Memi[table+i]
	    if (log) {
		TBL_WMIN(tbl) = log10 (TBL_WMIN(tbl))
		TBL_WMAX(tbl) = log10 (TBL_WMAX(tbl))
	    }
	    call dc_defaults (TBL_WMIN(tbl), TBL_WMAX(tbl), TBL_NWMAX(tbl),
		TBL_W1(tbl), TBL_W2(tbl), TBL_DW(tbl), TBL_NW(tbl))
	}
 
	call sfree (sp)
end
 

# MS_WAVELENGTHS1 -- Set output wavelength parameters for a spectrum.
# Fill in any INDEF values using the limits of the dispersion function
# over all apertures and the minimum dispersion over all apertures.  The
# user may then confirm and change the wavelength parameters if desired.
 
procedure ms_wavelengths1 (in, output, log, table, naps, w1, w2, dw, nw)
 
pointer	in		# IMIO pointer
char	output[ARB]	# Output image name
bool	log		# Logarithm wavelength parameters?
pointer	table		# Wavelength table
int	naps		# Number of apertures
real	w1, w2, dw	# Image wavelength parameters
int	nw		# Image wavelength parameter
 
int	i, n, clgeti(), clgwrd()
real	a, b, c, y1, y2, dy, clgetr()
pointer	sp, str, tbl
double	ms_eval()
bool	clgetb()
 
include	"msdispcor.com"
 
begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Get aperture parameters.
	call dc_getentry (false, 1, table, naps, i)
	tbl = Memi[table+i]
 
	w1 = TBL_W1(tbl)
	w2 = TBL_W2(tbl)
	dw = TBL_DW(tbl)
	nw = TBL_NW(tbl)
 
	# If there are undefined wavelength scale parameters get
	# defaults based on the reference spectrum.
 
	if (IS_INDEF(w1)||IS_INDEF(w2)||IS_INDEF(dw)||IS_INDEFI(nw)) {
	    a = MAX_REAL
	    b = -MAX_REAL
	    c = MAX_REAL
 
	    n = IM_LEN(in,1)
	    do i = 1, IM_LEN(in, 2) {
		line = i
		y1 = ms_eval (double (1.))
		y2 = ms_eval (double (n))
		dy = abs ((y2 - y1) / (n - 1))
		a = min (a, y1, y2)
		b = max (b, y1, y2)
		c = min (c, dy)
	    }
	    n = (b - a) / c + 1
	    if (log) {
		a = log10 (a)
		b = log10 (b)
	    }
	}
 
	call dc_defaults (a, b, n, w1, w2, dw, nw)
 
	# Print the wavelength scale and allow the user to confirm and
	# change the wavelength scale.  A test is done to check which
	# parameters the user changes and give them priority in filling
	# in the remaining parameters.
 
	call printf ("%s: w1 = %g, w2 = %g, dw = %g, nw = %d, log = %b\n")
	    call pargstr (output)
	    call pargr (w1)
	    call pargr (w2)
	    call pargr (dw)
	    call pargi (nw)
	    call pargb (log)
 
	if (TBL_CONFIRM(tbl) == YES) {
	    repeat {
		i = clgwrd ("dispcor1.change", Memc[str],SZ_LINE, "|yes|no|NO|")
		switch (i) {
		case 2:
		    break
		case 3:
		    TBL_CONFIRM(tbl) = NO
		    break
		}
		call clputr ("dispcor1.w1", w1)
		call clputr ("dispcor1.w2", w2)
		call clputr ("dispcor1.dw", dw)
		call clputi ("dispcor1.nw", nw)
		a = w1
		b = w2
		c = dw
		n = nw
		w1 = clgetr ("dispcor1.w1")
		w2 = clgetr ("dispcor1.w2")
		dw = clgetr ("dispcor1.dw")
		nw = clgeti ("dispcor1.nw")
 
		# If no INDEF's set unchanged parameters to INDEF.
		i = 0
		if (IS_INDEF(w1))
		    i = i + 1
		if (IS_INDEF(w2))
		    i = i + 1
		if (IS_INDEF(dw))
		    i = i + 1
		if (IS_INDEFI(nw))
		    i = i + 1
		if (i == 0) {
		    if (w1 == a)
		        w1 = INDEF
		    if (w2 == b)
		        w2 = INDEF
		    if (dw == c)
		        dw = INDEF
		    if (nw == n)
		        nw = INDEFI
		}
 
	        call dc_defaults (a, b, n, w1, w2, dw, nw)
 
		call printf (
		    "%s: w1 = %g, w2 = %g, dw = %g, nw = %d\n")
		    call pargstr (output)
		    call pargr (w1)
		    call pargr (w2)
		    call pargr (dw)
		    call pargi (nw)
 
		if (clgetb ("global")) {
		    TBL_W1(tbl) = w1
		    TBL_W2(tbl) = w2
		    TBL_DW(tbl) = dw
		    TBL_NW(tbl) = nw
		}
	    }
	}
	call flush (STDOUT)
 
	call sfree (sp)
end
 
 
# MS_WAVELENGTHS -- Set output wavelength parameters for a spectrum for
# each aperture.  The fixed parameters are given in the wavelength table.
# If there is no entry in the table for an aperture used the global
# default (entry 1).  Fill in INDEF values using the limits and number
# of pixels for the aperture.  The user may then confirm and change the
# wavelength parameters if desired.
 
procedure ms_wavelengths (in, output, log, table, naps, l, ap,
	w1, w2, dw, nw)
 
pointer	in		# IMIO pointer
char	output[ARB]	# Output image name
bool	log		# Logarithm wavelength parameters?
pointer	table		# Wavelength table
int	naps		# Number of apertures
int	l		# Line
int	ap		# Aperture
real	w1, w2, dw	# Image wavelength parameters
int	nw		# Image wavelength parameter
 
int	i, n, clgeti(), clgwrd()
real	a, b, c, clgetr()
pointer	sp, str, tbl
double	ms_eval()
bool	clgetb()
 
include	"msdispcor.com"
 
begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Get aperture parameters.
	call dc_getentry (false, ap, table, naps, i)
	tbl = Memi[table+i]
 
	w1 = TBL_W1(tbl)
	w2 = TBL_W2(tbl)
	dw = TBL_DW(tbl)
	nw = TBL_NW(tbl)
 
	# If there are undefined wavelength scale parameters get
	# defaults based on the reference spectrum.
 
	if (IS_INDEF(w1)||IS_INDEF(w2)||IS_INDEF(dw)||IS_INDEFI(nw)) {
	    line = l
	    n = IM_LEN(in,1)
	    a = ms_eval (double (1.))
	    b = ms_eval (double (n))
	    if (log) {
		a = log10 (a)
		b = log10 (b)
	    }
	}
 
	call dc_defaults (a, b, n, w1, w2, dw, nw)
 
	# Print the wavelength scale and allow the user to confirm and
	# change the wavelength scale.  A test is done to check which
	# parameters the user changes and give them priority in filling
	# in the remaining parameters.
 
	call printf ("%s: ap = %d, w1 = %g, w2 = %g, dw = %g, nw = %d\n")
	    call pargstr (output)
	    call pargi (ap)
	    call pargr (w1)
	    call pargr (w2)
	    call pargr (dw)
	    call pargi (nw)
 
	if (TBL_CONFIRM(tbl) == YES) {
	    repeat {
		i = clgwrd ("dispcor1.change", Memc[str],SZ_LINE, "|yes|no|NO|")
		switch (i) {
		case 2:
		    break
		case 3:
		    TBL_CONFIRM(tbl) = NO
		    break
		}
		call clputr ("dispcor1.w1", w1)
		call clputr ("dispcor1.w2", w2)
		call clputr ("dispcor1.dw", dw)
		call clputi ("dispcor1.nw", nw)
		a = w1
		b = w2
		c = dw
		n = nw
		w1 = clgetr ("dispcor1.w1")
		w2 = clgetr ("dispcor1.w2")
		dw = clgetr ("dispcor1.dw")
		nw = clgeti ("dispcor1.nw")
 
		# If no INDEF's set unchanged parameters to INDEF.
		i = 0
		if (IS_INDEF(w1))
		    i = i + 1
		if (IS_INDEF(w2))
		    i = i + 1
		if (IS_INDEF(dw))
		    i = i + 1
		if (IS_INDEFI(nw))
		    i = i + 1
		if (i == 0) {
		    if (w1 == a)
		        w1 = INDEF
		    if (w2 == b)
		        w2 = INDEF
		    if (dw == c)
		        dw = INDEF
		    if (nw == n)
		        nw = INDEFI
		}
 
	        call dc_defaults (a, b, n, w1, w2, dw, nw)
 
		call printf (
		    "%s: ap = %d, w1 = %g, w2 = %g, dw = %g, nw = %d\n")
		    call pargstr (output)
		    call pargi (ap)
		    call pargr (w1)
		    call pargr (w2)
		    call pargr (dw)
		    call pargi (nw)
 
		if (clgetb ("global")) {
		    TBL_W1(tbl) = w1
		    TBL_W2(tbl) = w2
		    TBL_DW(tbl) = dw
		    TBL_NW(tbl) = nw
		}
	    }
	}
	call flush (STDOUT)
 
	call sfree (sp)
end

