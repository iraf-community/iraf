include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/allstardef.h"

# DP_INIT - Procedure to initialize the daophot structure.

procedure dp_init (dp)

pointer	dp		# pointer to the daophot structure

begin
	# Set the daophot structure.
	call calloc (dp, LEN_DPSTRUCT, TY_STRUCT)

	# Initalize the output type parameters.
	DP_TEXT(dp) = YES
	DP_VERBOSE(dp) = YES

	# Initialize the wcs parameters.
	DP_MW(dp) = NULL
	DP_WCSIN(dp) = WCS_LOGICAL
	DP_WCSOUT(dp) = WCS_LOGICAL
	DP_WCSPSF(dp) = WCS_LOGICAL
	DP_CTIN(dp) = NULL
	DP_CTOUT(dp) = NULL
	DP_CTPSF(dp) = NULL

	# Initialize the data depedent parameters.
	DP_SCALE(dp) = DEF_SCALE
	DP_SFWHMPSF(dp) = DEF_FWHMPSF
	DP_FWHMPSF(dp) = DEF_FWHMPSF / DEF_SCALE
	DP_MINGDATA(dp) = INDEFR
	DP_MAXGDATA(dp) = INDEFR

	# Initialize the noise parameters
	DP_CCDGAIN(dp) = EOS
	DP_PHOTADU(dp) = INDEFR
	DP_CCDREAD(dp) = EOS
	DP_READNOISE(dp) = INDEFR

	# Initialize the observing parameters.
	DP_EXPTIME(dp) = EOS
	DP_ITIME(dp) = INDEFR
	DP_AIRMASS(dp) = EOS
	DP_XAIRMASS(dp) = INDEFR
	DP_FILTER(dp) = EOS
	DP_IFILTER(dp) = EOS
	DP_OBSTIME(dp) = EOS
	DP_OTIME(dp) = EOS

	# Initialize the psf fitting parameters.
	call strcpy (",gauss,", DP_FUNCLIST(dp), SZ_FNAME)
	call strcpy ("gauss", DP_FUNCTION(dp), SZ_FNAME)
	DP_VARORDER(dp) = -1
	DP_FEXPAND(dp) = NO
	DP_SATURATED(dp) = NO
	DP_RPSFRAD(dp) = DEF_PSFRAD / DEF_SCALE
	DP_SPSFRAD (dp) = DEF_PSFRAD / DEF_SCALE
	DP_PSFRAD (dp) = DEF_PSFRAD

	# Initialize the fitting parameters.
	DP_SFITRAD(dp) = DEF_FITRAD / DEF_SCALE
	DP_FITRAD(dp) = DEF_FITRAD
	DP_SANNULUS(dp) = DEF_ANNULUS / DEF_SCALE
	DP_ANNULUS(dp) = DEF_ANNULUS 
	DP_SDANNULUS(dp) = DEF_DANNULUS / DEF_SCALE
	DP_DANNULUS(dp) = DEF_DANNULUS 
	DP_MAXITER(dp) = DEF_MAXITER
	DP_MAXGROUP(dp) = DEF_MAXGROUP
	DP_MAXNSTAR(dp) = DEF_MAXNSTAR
	DP_RECENTER(dp) = YES
	DP_FITSKY(dp) = NO
	DP_GROUPSKY(dp) = YES

	# Initialize the file names.
	DP_INIMAGE(dp) = EOS
	DP_COORDS(dp) = EOS
	DP_INPHOTFILE(dp) = EOS
	DP_PSFIMAGE(dp) = EOS
	DP_OUTPHOTFILE(dp) = EOS
	DP_OUTIMAGE(dp) = EOS
	DP_OUTREJFILE(dp) = EOS

	# Initialize the substructure pointers.
	DP_PSF(dp) = NULL
	DP_PSFFIT(dp) = NULL
	DP_GROUP(dp) = NULL
	DP_PEAK(dp) = NULL
	DP_NSTAR(dp) = NULL
	DP_SUBSTAR(dp) = NULL
	DP_ADDSTAR(dp) = NULL
	DP_ALLSTAR(dp) = NULL
	DP_APSEL(dp) = NULL
end


# DP_FITSETUP -- Setup the current PSF parameters given that the fitting
# parameters have been correctly read in.

procedure dp_fitsetup (dp)

pointer	dp		# pointer to daophot structure

pointer	psffit
bool	streq()

begin
	# Set up the psf fit structure.
	call malloc (DP_PSFFIT(dp), LEN_PSFFIT, TY_STRUCT)
	psffit = DP_PSFFIT(dp)
	call calloc (DP_PSFPARS(psffit), MAX_NFCTNPARS, TY_REAL)

	# Define the psf function. The if user entered string is "auto"
	# or a list then intialize the psf function to "gauss" or the
	# first function in the list respectively.

	if (streq (DP_FUNCTION(dp), "gauss")) {
	    DP_PSFUNCTION(psffit) = FCTN_GAUSS
	} else if (streq (DP_FUNCTION(dp), "moffat25")) {
	    DP_PSFUNCTION(psffit) = FCTN_MOFFAT25
	} else if (streq (DP_FUNCTION(dp), "penny1")) {
	    DP_PSFUNCTION(psffit) = FCTN_PENNY1
	} else if (streq (DP_FUNCTION(dp), "moffat15")) {
	    DP_PSFUNCTION(psffit) = FCTN_MOFFAT15
	} else if (streq (DP_FUNCTION(dp), "penny2")) {
	    DP_PSFUNCTION(psffit) = FCTN_PENNY2
	} else if (streq (DP_FUNCTION(dp), "lorentz")) {
	    DP_PSFUNCTION(psffit) = FCTN_LORENTZ
	} else if (streq (DP_FUNCTION(dp), "auto")) {
	    DP_PSFUNCTION(psffit) = FCTN_GAUSS
	} else {
	    call error (0, "Unknown analytic PSF function")
	}

	switch (DP_VARORDER(dp)) {
	case -1:
	    DP_NVLTABLE(psffit) = 0
	case 0:
	    DP_NVLTABLE(psffit) = 1
	case 1:
	    DP_NVLTABLE(psffit) = 3
	case 2:
	    DP_NVLTABLE(psffit) = 6
	}
	if (DP_FEXPAND(dp) == NO)
	    DP_NFEXTABLE(psffit) = 0
	else
	    DP_NFEXTABLE(psffit) = 5

	# Set the initial values of the function parameters.
	switch (DP_PSFUNCTION(psffit)) {
	case FCTN_GAUSS:
	    DP_PSFNPARS(psffit) = 2
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dp) / 2.0
	case FCTN_MOFFAT25:
	    DP_PSFNPARS(psffit) = 3
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.0
	    Memr[DP_PSFPARS(psffit)+3] = 2.5
	case FCTN_PENNY1:
	    DP_PSFNPARS(psffit) = 4
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.75
	    Memr[DP_PSFPARS(psffit)+3] = 0.0
	case FCTN_MOFFAT15:
	    DP_PSFNPARS(psffit) = 3
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.0
	    Memr[DP_PSFPARS(psffit)+3] = 1.5
	case FCTN_PENNY2:
	    DP_PSFNPARS(psffit) = 5
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.75
	    Memr[DP_PSFPARS(psffit)+3] = 0.0
	    Memr[DP_PSFPARS(psffit)+4] = 0.0
	case FCTN_LORENTZ:
	    DP_PSFNPARS(psffit) = 3
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dp) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.0
	default:
	    call error (0, "Unknown analytic PSF function")
	}

	DP_PSFHEIGHT(psffit) = INDEFR
	DP_PSFMAG(psffit) = INDEFR
	DP_PSFX(psffit) = INDEFR
	DP_PSFY(psffit) = INDEFR

	DP_PSFSIZE(psffit) = 0
	DP_PSFLUT(psffit) = NULL
end


# DP_APSELSETUP -- Procedure to set up the APSEL parameters.

procedure dp_apselsetup (dp)

pointer	dp		# pointer to daophot structure

pointer	apsel

begin
	# APSEL structure
	call malloc (DP_APSEL(dp), LEN_DPAPSTRUCT, TY_STRUCT)
	apsel = DP_APSEL(dp)
	call malloc (DP_APRESULT(apsel), NAPPAR, TY_INT)

	# Set the default values for the apsel parameters.
	DP_APID(apsel) = NULL
	DP_APXCEN(apsel)= NULL
	DP_APYCEN(apsel)= NULL
	DP_APMAG(apsel)	= NULL
	DP_APERR(apsel)	= NULL
	DP_APMSKY(apsel)= NULL
	DP_APGROUP(apsel) = NULL
	DP_APNITER(apsel) = NULL
	DP_APCHI(apsel) = NULL
	DP_APSHARP(apsel) = NULL
end
