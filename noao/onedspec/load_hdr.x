include <imhdr.h>
include "idsmtn.h"

define	APFORMATS	"|onedspec|echelle|multispec|"
define	ONEDSPEC	1
define	ECHELLE		2
define	MULTISPEC	3

# LOAD_IDS_HDR -- Read in IDS format header and decode all
#                 the elements into the structure

procedure load_ids_hdr (ids, im, line)

pointer	ids, im
int	line

int	i, strdic()
real	crpix
pointer	sp, str1, str2, psave

begin
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Save DF pointer before clearing
	psave = POINT(ids)

	# Initialize all elements to zero
	call aclri (Memi[ids], LEN_IDS * SZ_STRUCT / SZ_INT)
	POINT(ids) = psave
	call aclrr (Memr[POINT(ids)], MAX_NCOEFF)

	# Initialize defaults
	call init_ids_values (ids, im)
	crpix = 1.

	# Get header parameters.
	iferr (call imgstr (im, "APFORMAT", Memc[str1], SZ_LINE))
	    call strcpy ("onedspec", Memc[str1], SZ_LINE)
	i = strdic (Memc[str1], Memc[str1], SZ_LINE, APFORMATS)
	switch (i) {
	case ECHELLE, MULTISPEC:
	    call sprintf (Memc[str2], SZ_LINE, "APNUM%d")
		call pargi (line)
	    call imgstr (im, Memc[str2], Memc[str1], SZ_LINE)
	    call sscan (Memc[str1])
	    call gargi (BEAM(ids))
	    call gargi (i)
	    call gargr (W0(ids))
	    call gargr (WPC(ids))
	    call gargi (NP2(ids))
	    NP1(ids) = 0
	default:
	    call ids_hdri (im, "BEAM-NUM", BEAM(ids))
	    crpix = 1.
	    call ids_hdrr (im, "W0", W0(ids))
	    call ids_hdrr (im, "WPC", WPC(ids))
	    call ids_hdri (im, "NP1", NP1(ids))
	    call ids_hdri (im, "NP2", NP2(ids))
	}
	LINE(ids) = line
	NP1(ids) = max (NP1(ids), 0)
	NP2(ids) = min (NP2(ids), IM_LEN(im,1))

	call ids_hdri (im, "OFLAG", OFLAG(ids))
	call ids_hdrr (im, "EXPOSURE", ITM(ids))
	call ids_hdrr (im, "ITIME", ITM(ids))
	call ids_hdrr (im, "EXPTIME", ITM(ids))
	call ids_hdrr (im, "UT", UT(ids))
	call ids_hdrr (im, "ST", ST(ids))
	call ids_hdrr (im, "RA", RA(ids))
	call ids_hdrr (im, "DEC", DEC(ids))
	call ids_hdrr (im, "HA", HA(ids))
	call ids_hdrr (im, "AIRMASS", AIRMASS(ids))
	call ids_hdri (im, "SM-FLAG", SM_FLAG(ids))
	call ids_hdri (im, "QF-FLAG", QF_FLAG(ids))
	call ids_hdri (im, "DC-FLAG", DC_FLAG(ids))
	call ids_hdri (im, "QD-FLAG", QD_FLAG(ids))
	call ids_hdri (im, "EX-FLAG", EX_FLAG(ids))
	call ids_hdri (im, "BS-FLAG", BS_FLAG(ids))
	call ids_hdri (im, "CA-FLAG", CA_FLAG(ids))
	call ids_hdri (im, "CO-FLAG", CO_FLAG(ids))
	call ids_hdri (im, "DF-FLAG", DF_FLAG(ids))

	# Allow for data using CRVAL and CDELT.
	if (IS_INDEF (W0(ids))) {
	    call ids_hdrr (im, "CRPIX1", crpix)
	    call ids_hdrr (im, "CRVAL1", W0(ids))
	}
	if (IS_INDEF (WPC(ids)))
	    call ids_hdrr (im, "CDELT1", WPC(ids))
	if (IS_INDEF (WPC(ids)))
	    call ids_hdrr (im, "CD1_1", WPC(ids))
	
	# Change to a reference pixel of 1 and Angstrom units.
	if (!IS_INDEF (W0(ids)) && (crpix != 1.))
	    W0(ids) = W0(ids) + WPC(ids) * (1 - crpix)

	if (!IS_INDEF (W0(ids)) && (abs (W0(ids)) < 0.001)) {
	    W0(ids) = W0(ids) * 1e10
	    WPC(ids) = WPC(ids) * 1e10
	}

	# Flag bad airmass value; i.e. 0.
	if (!IS_INDEF (AIRMASS(ids)) && AIRMASS(ids) < 1.)
	    AIRMASS(ids) = INDEFR
	    
	if (DF_FLAG(ids) > 0)
	    do i = 1, DF_FLAG(ids) {
		call sprintf (Memc[str1], SZ_LINE, "DF%d")
		    call pargi (i)
		call ids_hdrr (im, Memc[str1], Memr[POINT(ids)+i-1])
	    }

	call sfree (sp)
end

# IDS_HDRI -- Load an integer value from the header

procedure ids_hdri (im, field, ival)

pointer	im
char	field[ARB]
int	ival

int	imaccf(), imgeti()

begin
	if (imaccf (im, field) == YES)
	    ival = imgeti (im, field)
end

# IDS_HDRR -- Load a real value from the header

procedure ids_hdrr (im, field, rval)

pointer	im
char	field[ARB]
real	rval

int	imaccf()
real	imgetr()

begin
	if (imaccf (im, field) == YES)
	    rval = imgetr (im, field)
end


# GET_HDRR -- Load a real value from the header.

real procedure get_hdrr (im, field)

pointer	im
char	field[ARB]

int	imaccf()
real	rval, imgetr()

begin
	if (imaccf (im, field) == YES)
	    rval = imgetr (im, field)
	else
	    rval = INDEF
	return (rval)
end


# INIT_IDS_VALUES -- Initialize several important flags in the header

procedure init_ids_values (ids, im)

pointer	ids
pointer	im

begin
	# Processing flags set to not done
	DF_FLAG(ids) = -1		# Dispersion fitting
	SM_FLAG(ids) = -1		# Smoothing
	QF_FLAG(ids) = -1		# Quartz fit
	DC_FLAG(ids) = -1		# Dispersion correction
	QD_FLAG(ids) = -1		# Quartz division
	EX_FLAG(ids) = -1		# Extinction corrected
	BS_FLAG(ids) = -1		# Beam-switched
	CA_FLAG(ids) = -1		# Calibrated to flux
	CO_FLAG(ids) = -1		# Coincidence corrected

	# Object/sky defaults to object
	OFLAG(ids) = 1			# 1=object, 0=sky

	# Initialize other parameters.

	BEAM(ids) = 0
	NP1(ids) = 0
	NP2(ids) = IM_LEN(im, 1)

	ITM(ids) = INDEFR
	UT(ids) = INDEFR
	ST(ids) = INDEFR
	RA(ids) = INDEFR
	DEC(ids) = INDEFR
	W0(ids) = INDEFR
	WPC(ids) = INDEFR
	HA(ids) = INDEFR
	AIRMASS(ids) = INDEFR
end
