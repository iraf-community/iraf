include "../lib/allstardef.h"
include "../lib/apsel.h"
include "../lib/daophotdef.h"
include "../lib/nstardef.h"
include "../lib/psfdef.h"
include "../lib/psf.h"

# DP_INIT - Procedure to initialize the daophot structure.

procedure dp_init (dp)

pointer	dp		# pointer to the daophot structure

begin
	# Set the daophot structure.
	call malloc (dp, LEN_DPSTRUCT, TY_STRUCT)

	# Initalize the output parameters.
	DP_TEXT(dp) = YES
	DP_VERBOSE(dp) = YES

	# Initialize the data depedent parameters.
	DP_SCALE(dp) = DEF_SCALE
	DP_MINGDATA(dp) = INDEFR
	DP_MAXGDATA(dp) = INDEFR
	DP_PHOT_ADC(dp) = INDEFR
	DP_READ_NOISE(dp) = INDEFR
	DP_OTIME(dp) = EOS
	DP_XAIRMASS(dp) = INDEFR
	DP_ITIME(dp) = INDEFR

	# Initialize the psf fitting parameters.
	DP_VARPSF(dp) = NO
	DP_RPSFRAD(dp) = DEF_PSFRAD
	DP_SPSFRAD (dp) = DEF_PSFRAD
	DP_PSFRAD (dp) = DEF_PSFRAD
	DP_SFITRAD(dp) = DEF_FITRAD
	DP_FITRAD(dp) = DEF_FITRAD
	DP_MAXITER(dp) = DEF_MAXITER
	DP_MAXGROUP(dp) = DEF_MAXGROUP
	DP_MAXSTAR(dp) = DEF_MAXSTAR
	#DP_CUR_SKY(dp) = INDEFR
	DP_RECENTER(dp) = YES

	# Initialize the pointers.
	DP_PSF(dp) = NULL
	DP_PSFFIT(dp) = NULL
	DP_GROUP(dp) = NULL
	DP_PEAK(dp) = NULL
	DP_NSTAR(dp) = NULL
	DP_SUBSTAR(dp) = NULL
	DP_ADDSTAR(dp) = NULL
	DP_ALLSTAR(dp) = NULL
	DP_APSEL(dp) = NULL

	# Initialize the file names.
	DP_IMNAME(dp) = EOS
	DP_APFILE(dp) = EOS
	DP_PSFIMAGE(dp) = EOS
	DP_GRPSFFILE(dp) = EOS
	DP_PKFILE(dp) = EOS
	DP_GRPFILE(dp) = EOS
	DP_NSTARFILE(dp) = EOS
	DP_SUBIMAGE(dp) = EOS
	DP_ASTARFILE(dp) = EOS
	DP_ADDIMAGE(dp) = EOS
	DP_ADDFILE(dp) = EOS
end


# DP_FITSETUP -- Procedure to set up the PSF fitting parameters.

procedure dp_fitsetup (dp)

pointer	dp		# pointer to daophot structure

pointer	psffit

begin
	# Set up the psf fit structure.
	call malloc (DP_PSFFIT(dp), LEN_PSFFIT, TY_STRUCT)
	psffit = DP_PSFFIT(dp)

	DP_PSFSIZE(psffit) = 2 * (2 * nint (DP_PSFRAD (dp)) + 1) + 7
	DP_PSFLUT(psffit) = NULL
end


# DP_PSFSETUP -- Procedure to set up the PSF parameters.

procedure dp_psfsetup (dp)

pointer	dp		# pointer to daophot structure

pointer	psf, psfpl

begin
	# Allocate memory for the psf fitting structure.
	call malloc (DP_PSF(dp), LEN_PSFSTRUCT, TY_STRUCT)
	psf = DP_PSF(dp)

	# Initialize the psf fitting structure.
	DP_CUR_PSF(psf) = 0
	DP_CUR_PSFID(psf) = 0
	DP_CUR_PSFX (psf) = INDEFR
	DP_CUR_PSFY (psf) = INDEFR
	DP_CUR_PSFSKY(psf) = INDEFR
	DP_CUR_PSFMAG(psf) = INDEFR
	DP_PSFNUMB (psf) = 0
	DP_PSFMATRIX(psf) = NULL
	DP_SZLOOKUP(psf) = 2 * nint (DP_PSFRAD(dp)) + 1 + 7
	DP_PLOOKUP(psf) = NULL

	# Allocate memory for the psf output structure.
	call malloc (DP_COLPOINT(psf), NAPCOLUMNS, TY_INT)

	# Allocate space for the plot structure.
	call malloc (DP_PSFPLOT(psf), LEN_PSFPLOT, TY_STRUCT)
	psfpl = DP_PSFPLOT(psf)

	# Initialize the plot structure.
	DP_PLOT_TYPE(psfpl) = DP_MESHPLOT
	DP_MANGV(psfpl)	= 30.
	DP_MANGH(psfpl) = -30.
	DP_MFLOOR(psfpl) = 0.
	DP_MCEILING(psfpl) = 0.
end


# DP_NSTARSETUP -- Procedure to set up the NSTAR parameters.

procedure dp_nstarsetup (dp)

pointer	dp		# pointer to daophot structure

pointer	nstar

begin
	# Allocate Memory
	call malloc (DP_NSTAR(dp), LEN_NSTARSTRUCT, TY_STRUCT)
	nstar = DP_NSTAR(dp)

	DP_NPIX(nstar) = NULL
	DP_NUMER(nstar) = NULL
	DP_DENOM(nstar) = NULL
	DP_RPIXSQ(nstar) = NULL
	DP_SKIP(nstar) = NULL
	DP_XCLAMP(nstar) = NULL
	DP_XOLD(nstar) = NULL
	DP_X(nstar) = NULL
	DP_V(nstar) = NULL
	DP_SUMWT(nstar) = NULL
	DP_C(nstar) = NULL
end


# DP_ALLSTARSETUP -- Procedure to set up the ALLSTAR parameters.

procedure dp_allstarsetup (dp)

pointer	dp		# pointer to daophot structure

pointer	allstar

begin
	# Allocate memory.
	call malloc (DP_ALLSTAR(dp), LEN_ALLSTARSTRUCT, TY_STRUCT)
	allstar = DP_ALLSTAR(dp)

	DP_DATA(allstar) = NULL
	DP_SUBT(allstar) = NULL
	DP_WEIGHTS(allstar) = NULL
	DP_DBUF(allstar) = NULL
	DP_SBUF(allstar) = NULL
	DP_WBUF(allstar) = NULL

	DP_ANUMER1(allstar) = NULL
	DP_ANUMER2(allstar) = NULL
	DP_ADENOM1(allstar) = NULL
	DP_ADENOM2(allstar) = NULL
	DP_ASUMWT(allstar) = NULL
	DP_ARPIXSQ(allstar) = NULL
	DP_ASKIP(allstar) = NULL
	DP_AXCLAMP(allstar) = NULL
	DP_AYCLAMP(allstar) = NULL
	DP_AXOLD(allstar) = NULL
	DP_AYOLD(allstar) = NULL
	DP_AX(allstar) = NULL
	DP_AV(allstar) = NULL
	DP_AC(allstar) = NULL
	DP_ALAST(allstar) = NULL
	DP_ANPIX(allstar) = NULL
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
