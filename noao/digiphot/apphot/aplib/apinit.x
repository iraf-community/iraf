include "../lib/apphotdef.h"
include "../lib/noisedef.h"
include "../lib/noise.h"
include "../lib/displaydef.h"

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
	AP_THRESHOLD(nse) = DEF_THRESHOLD
	AP_CTHRESHOLD(nse) = DEF_CTHRESHOLD
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
