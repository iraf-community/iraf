include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/noisedef.h"
include "../lib/noise.h"
include "../lib/displaydef.h"

# AP_DEFSETUP -- Initialize the global apphot package parameters to their 
# default values.

procedure ap_defsetup (ap, fwhmpsf)

pointer	ap			# pointer to the apphot package
real	fwhmpsf			# the FWHM of the stellar images

begin
	# Initalize the file names.
	AP_IMNAME(ap) = EOS
	AP_IMROOT(ap) = EOS
	AP_CLNAME(ap) = EOS
	AP_CLROOT(ap) = EOS
	AP_OUTNAME(ap) = EOS
	AP_PLOTFILE(ap) = EOS
	AP_OUTNAME(ap) = EOS

	AP_WCSIN(ap) = WCS_LOGICAL
	AP_WCSOUT(ap) = WCS_LOGICAL
	AP_MW(ap) = NULL
	AP_CTIN(ap) = NULL
	AP_CTOUT(ap) = NULL

	# Initialize the cursor positions.
	AP_CWX(ap) = INDEFR
	AP_CWY(ap) = INDEFR
	AP_WX(ap) = INDEFR
	AP_WY(ap) = INDEFR

	# Set up the data characteristics.
	AP_SCALE(ap) = DEF_SCALE
	AP_FWHMPSF(ap) = fwhmpsf
	AP_POSITIVE(ap) = DEF_POSITIVE
	AP_DATAMIN(ap) = DEF_DATAMIN
	AP_DATAMAX(ap) = DEF_DATAMAX

	# Set up the image header keywords.
	AP_EXPOSURE(ap) = EOS
	AP_ITIME(ap) = DEF_ITIME
	AP_FILTER(ap) = EOS
	call strcpy (DEF_FILTERID, AP_FILTERID(ap), SZ_FNAME)
	AP_AIRMASS(ap) = EOS
	AP_XAIRMASS(ap) = DEF_XAIRMASS
	AP_OBSTIME(ap) = EOS
	call strcpy (DEF_OTIME, AP_OTIME(ap), SZ_FNAME)

	# Set buffer parameters.
	AP_SEQUENTIAL(ap) = NULL
	AP_IMBUF(ap) = NULL
	AP_HWIDTH(ap) = 0
end


# AP_NOISESETUP -- Procedure to intialize noise model parameters.

procedure ap_noisesetup (ap, noise)

pointer	ap	# pointer to apphot structure
int	noise	# noise model

pointer	nse

begin
	call malloc (AP_NOISE(ap), LEN_APNOISE, TY_STRUCT)
	nse = AP_NOISE(ap)
	AP_NOISEFUNCTION(nse) = noise
	switch (noise) {
	case AP_NCONSTANT:
	    call strcpy ("constant", AP_NSTRING(nse), SZ_FNAME)
	case AP_NPOISSON:
	    call strcpy ("poisson", AP_NSTRING(nse), SZ_FNAME)
	default:
	    call strcpy ("poisson", AP_NSTRING(nse), SZ_FNAME)
	}
	AP_READNOISE(nse) = DEF_READNOISE
	AP_SKYSIGMA(nse) = DEF_SKYSIGMA
	AP_EPADU(nse) = DEF_EPADU
	AP_GAIN(nse) = EOS
	AP_CCDREAD(nse) = EOS
end


# AP_DISPSETUP -- Procedure to setup the display parameters.

procedure ap_dispsetup (ap)

pointer	ap		# pointer to apphot structure

pointer	dsp

begin
	call malloc (AP_PDISPLAY(ap), LEN_DISPLAYSTRUCT, TY_STRUCT)
	dsp = AP_PDISPLAY(ap)
	AP_MKSKY(dsp) = DEF_MKSKY
	AP_MKCENTER(dsp) = DEF_MKCENTER
	AP_MKAPERT(dsp) = DEF_MKAPERT
	AP_RADPLOTS(dsp) = DEF_RADPLOTS
	AP_MKDETECTIONS(dsp) = DEF_MKDETECTIONS
end
