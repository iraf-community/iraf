include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	<mwset.h>


# APMW_OPEN   -- Open APMW structure.
# APMW_CLOSE  -- Close APMW structure.
# APMW_SETAP  -- Set aperture values in APMW structure.
# APMW_SAVEIM -- Set WCS in image header.
# APMW_WCSFIX -- Fix up WCS

# Output formats
define	ONEDSPEC	1	# Individual 1D spectra
define	MULTISPEC	2	# Multiple spectra
define	ECHELLE		3	# Echelle spectra
define	STRIP		4	# Strip spectra
define	NORM		5	# Normalized spectra
define	FLAT		6	# Flat spectra
define	RATIO		7	# Ratio of data to model
define	DIFF		8	# Difference of data and model
define	FIT		9	# Model
define	NOISE		10	# Noise calculation


# Data structure for the apertures.  This version assumes the coordinates
# are the same for all the apertures.

define	APMW_LEN	(8 + $1 * 6)		# Structure length

define	APMW_LABEL	Memi[$1]			# WCS label
define	APMW_UNITS	Memi[$1+1]			# WCS units
define	APMW_DTYPE	Memi[$1+2]			# Dispersion type
define	APMW_NW		Memi[$1+3]			# Number of pixels
define	APMW_W1		Memd[P2D($1+4)]			# Starting coordinate
define	APMW_DW		Memd[P2D($1+6)]			# Coordinate per pixel
define	APMW_AP		Memi[$1+6*($2-1)+8]		# Aperture
define	APMW_BEAM	Memi[$1+6*($2-1)+9]		# Beam
define	APMW_APLOW	Memd[P2D($1+6*($2-1)+10)]	# Aperture low
define	APMW_APHIGH	Memd[P2D($1+6*($2-1)+12)]	# Aperture high


# APMW_OPEN -- Open APMW structure.

pointer procedure apmw_open (in, out, dispaxis, naps, nw)

pointer	in		#I Input IMIO pointer
pointer	out		#I Output IMIO pointer
int	dispaxis	#I Input dispersion axis
int	naps		#I Number of apertures
int	nw		#I Number of dispersion pixels
pointer	apmw		#O Returned APMW pointer

int	imgeti()
double	mw_c1trand()
pointer	mw, ct, mw_openim(), mw_sctran()
errchk	mw_openim, mw_sctran, mw_c1trand, apmw_wcsfix

begin
	# Allocate data structure.
	call malloc (apmw, APMW_LEN(naps), TY_STRUCT)
	call malloc (APMW_LABEL(apmw), SZ_LINE, TY_CHAR)
	call malloc (APMW_UNITS(apmw), SZ_LINE, TY_CHAR)

	# Set defaults.
	call strcpy ("Pixel", Memc[APMW_LABEL(apmw)], SZ_LINE)
	Memc[APMW_UNITS(apmw)] = EOS
	APMW_DTYPE(apmw,i) = -1
	APMW_NW(apmw,i) = nw
	APMW_W1(apmw,i) = 1.
	APMW_DW(apmw,i) = 1.

	# Get WCS info from input image.
	iferr {
	    mw = mw_openim (in)
	    iferr (APMW_DTYPE(apmw) = imgeti (in, "DC-FLAG"))
		APMW_DTYPE(apmw) = -1
	    iferr (call mw_gwattrs (mw, dispaxis, "label",
		Memc[APMW_LABEL(apmw)], SZ_LINE)) {
		if (APMW_DTYPE(apmw) == -1)
		    call strcpy ("Pixel", Memc[APMW_LABEL(apmw)], SZ_LINE)
		else
		    call strcpy ("Wavelength", Memc[APMW_LABEL(apmw)], SZ_LINE)
	    }
	    iferr (call mw_gwattrs (mw, dispaxis, "units",
		Memc[APMW_UNITS(apmw)], SZ_LINE)) {
		if (APMW_DTYPE(apmw) == -1)
		    Memc[APMW_UNITS(apmw)] = EOS
		else
		    call strcpy ("Angstroms", Memc[APMW_UNITS(apmw)], SZ_LINE)
	    }

	    call apmw_wcsfix (in, mw)
	    iferr (ct = mw_sctran (mw, "logical", "world", dispaxis))
		call error (1,
	    "Coordinate system ignored (rotated?). Using pixel coordinates.")
	    APMW_W1(apmw) = mw_c1trand (ct, 1D0)
	    APMW_DW(apmw) = mw_c1trand (ct, double (nw))
	    APMW_DW(apmw) = (APMW_DW(apmw)-APMW_W1(apmw))/(nw-1)
	} then
	    call erract (EA_WARN)

	call mw_close (mw)

	return (apmw)
end


# APMW_CLOSE -- Close APMW structure.

procedure apmw_close (apmw)

pointer	apmw		# APMW pointer

begin
	call mfree (APMW_LABEL(apmw), TY_CHAR)
	call mfree (APMW_UNITS(apmw), TY_CHAR)
	call mfree (apmw, TY_STRUCT)
end


# APMW_SETAP -- Set aperture values in APMW structure.

procedure apmw_setap (apmw, line, ap, beam, aplow, aphigh)

pointer	apmw		# APMW pointer
int	line		# Image line
int	ap		# Aperture
int	beam		# Beam
real	aplow		# Aperture lower limit
real	aphigh		# Aperture upper limit

begin
	APMW_AP(apmw,line) = ap
	APMW_BEAM(apmw,line) = beam
	APMW_APLOW(apmw,line) = aplow
	APMW_APHIGH(apmw,line) = aphigh
end


# APMW_SAVEIM -- Save WCS in image header.

procedure apmw_saveim (apmw, im, fmt)

pointer	apmw			#I APMW pointer
pointer	im			#I IMIO pointer
int	fmt			#I Output format

int	i, naps, wcsdim, axes[3], imaccf()
double	r[3], w[3], cd[9]
bool	strne()
pointer	sp, key, str, mw, list, mw_open(), imofnlu(), imgnfn()
errchk	imdelf
data	axes/1,2,3/

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	if (fmt == STRIP)
	    naps = 1
	else
	    naps = IM_LEN(im, 2)

	# Workaround for truncation of header during image header copy.
	IM_HDRLEN(im) = IM_LENHDRMEM(im)

	# Delete keywords.
	list = imofnlu (im, "SLFIB[0-9]*")
	while (imgnfn (list, Memc[key], SZ_FNAME) != EOF)
	    call imdelf (im, Memc[key])
	call imcfnl (list)

	# Add aperture parameters to image header.
	do i = 1, naps {
	    call sprintf (Memc[key], SZ_FNAME, "APNUM%d")
		call pargi (i)
	    call sprintf (Memc[str], SZ_LINE, "%d %d %.2f %.2f")
		call pargi (APMW_AP(apmw,i))
		call pargi (APMW_BEAM(apmw,i))
		call pargd (APMW_APLOW(apmw,i))
		call pargd (APMW_APHIGH(apmw,i))
	    call imastr (im, Memc[key], Memc[str])
	    if (naps == 1) {
		call sprintf (Memc[key], SZ_FNAME, "APID%d")
		    call pargi (i)
		ifnoerr (call imgstr (im, Memc[key], Memc[str], SZ_LINE)) {
		    if (strne (Memc[str], IM_TITLE(im))) {
			call imastr (im, "MSTITLE", IM_TITLE(im))
			call strcpy (Memc[str], IM_TITLE(im), SZ_IMTITLE)
		    }
		    call imdelf (im, Memc[key])
		}
	    }
	}

	# Add dispersion parameters to image header.
	if (APMW_DTYPE(apmw) != -1)
	    call imaddi (im, "DC-FLAG", APMW_DTYPE(apmw))
	else if (imaccf (im, "DC-FLAG") == YES)
	    call imdelf (im, "DC-FLAG")
	if (APMW_NW(apmw) < IM_LEN(im,1))
	    call imaddi (im, "NP2", APMW_NW(apmw))
	else if (imaccf (im, "NP2") == YES)
	    call imdelf (im, "NP2")
	iferr (call imdelf (im, "dispaxis"))
	    ;
	if (fmt == STRIP)
	    call imaddi (im, "dispaxis", 1)

	# Set WCS in image header.
	wcsdim = IM_NPHYSDIM(im)
	mw = mw_open (NULL, wcsdim)
	if (fmt == STRIP)
	    call mw_newsystem (mw, "linear", wcsdim)
	else
	    call mw_newsystem (mw, "equispec", wcsdim)
	call mw_swtype (mw, axes, wcsdim, "linear", "")
	if (Memc[APMW_LABEL(apmw)] != EOS)
	    call mw_swattrs (mw, 1, "label", Memc[APMW_LABEL(apmw)])
	if (Memc[APMW_UNITS(apmw)] != EOS)
	    call mw_swattrs (mw, 1, "units", Memc[APMW_UNITS(apmw)])

	call aclrd (r, 3)
	call aclrd (w, 3)
	call aclrd (cd, 9)
	r[1] = 1.
	w[1] = APMW_W1(apmw)
	cd[1] = APMW_DW(apmw)
	if (wcsdim == 2)
	    cd[4] = 1.
	if (wcsdim == 3) {
	    cd[5] = 1.
	    cd[9] = 1.
	}
	call mw_swtermd (mw, r, w, cd, wcsdim)

	call mw_saveim (mw, im)
	call mw_close (mw)

	call sfree (sp)
end


# APMW_WCSFIX -- Fix up WCS to avoid CDELT=0 which occurs if there are WCS
# keywords in the header but no CDELT.

procedure apmw_wcsfix (im, mw)

pointer	im			# IMIO pointer
pointer	mw			# MWCS pointer

int	i, ndim, mw_stati()
double	val
pointer	sp, r, w, cd
errchk	mw_gwtermd, mw_swtermd

begin
	call mw_seti (mw, MW_USEAXMAP, NO)
	ndim = mw_stati (mw, MW_NDIM)

	call smark (sp)
	call salloc (r, ndim, TY_DOUBLE)
	call salloc (w, ndim, TY_DOUBLE)
	call salloc (cd, ndim*ndim, TY_DOUBLE)

	# Check cd terms.  Assume no rotation.
	call mw_gwtermd (mw, Memd[r], Memd[w], Memd[cd], ndim)
	do i = 0, ndim-1 {
	    val = Memd[cd+i*(ndim+1)]
	    if (val == 0D0) {
		Memd[w+i] = 1D0
	        Memd[cd+i*(ndim+1)] = 1D0
	    }
	}
	call mw_swtermd (mw, Memd[r], Memd[w], Memd[cd], ndim)

	call sfree (sp)
end
