include	<error.h>
include <imhdr.h>
include "idsmtn.h"


# LOAD_IDS_HDR -- Read in IDS format header and decode all
#                 the elements into the structure

procedure load_ids_hdr (ids, im, line)

pointer	ids, im
int	line

int	i, apformat, dispaxis, ap, beam, np2
real	w0, wpc, low, high
pointer	mw, smw_openim()
pointer	sp, str1, str2, psave

begin
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Save pointers before clearing
	psave = POINT(ids)

	# Initialize all elements to zero
	call aclri (Memi[ids], LEN_IDS * SZ_STRUCT / SZ_INT)
	POINT(ids) = psave
	call aclrr (Memr[POINT(ids)], MAX_NCOEFF)

	# Initialize defaults
	call init_ids_values (ids, im)

	# Get dispersion parameters and title.
	i = max (1, line)
	ap = INDEFI
	apformat = 0
	mw = smw_openim (im)
	call gmwcs (im, mw, apformat, dispaxis, i, ap, beam, w0, wpc,
	    np2, low, high, LABEL(ids), SZ_IDS_ID)
	call mw_close (mw)

	if (abs (w0) < 0.001) {
	    w0 = w0 * 1e10
	    wpc = wpc * 1e10
	}

	BEAM(ids) = ap
	W0(ids) = w0
	WPC(ids) = wpc
	NP1(ids) = 0
	NP2(ids) = np2
	LINE(ids) = line

	# Get other header parameters.
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
	    
	if (DF_FLAG(ids) > 0)
	    do i = 1, DF_FLAG(ids) {
		call sprintf (Memc[str1], SZ_LINE, "DF%d")
		    call pargi (i)
		call ids_hdrr (im, Memc[str1], Memr[POINT(ids)+i-1])
	    }

	# Flag bad airmass value; i.e. 0.
	if (!IS_INDEF (AIRMASS(ids)) && AIRMASS(ids) < 1.)
	    AIRMASS(ids) = INDEFR

	call sfree (sp)
end

# IDS_HDRI -- Load an integer value from the header

procedure ids_hdri (im, field, ival)

pointer	im
char	field[ARB]
int	ival

int	dummy, imaccf(), imgeti()

begin
	if (imaccf (im, field) == YES) {
	    iferr (dummy = imgeti (im, field))
		call erract (EA_WARN)
	    else
		ival = dummy
	}
end

# IDS_HDRR -- Load a real value from the header

procedure ids_hdrr (im, field, rval)

pointer	im
char	field[ARB]
real	rval

int	imaccf()
real	dummy, imgetr()

begin
	if (imaccf (im, field) == YES) {
	    iferr (dummy = imgetr (im, field))
		call erract (EA_WARN)
	    else
		rval = dummy
	}
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
