include	<mach.h>
include <imhdr.h>
include "idsmtn.h"

define	COL_VALUE	11
define	LEN_KEYWORD	8
define	LEN_OBJECT	65
define	LEN_CARD	80

# IDSM_STORE_KEYWORDS -- store IDS specific keywords in the IRAF image header.

procedure idsm_store_keywords (ids, im)

pointer	ids	# Pointer to program data structure
pointer	im	# Pointer to image

char	keyword[LEN_KEYWORD]
int	fd, i, nrp 
bool	fp_equalr()
int	stropen()
real	value
errchk	stropen, addcard_i, addcard_r, addcard_st

begin
	# Open image user area as a file.
	fd = stropen (Memc[IM_USERAREA(im)], LEN_USER_AREA - 1, NEW_FILE)

	# FITS keyword are formatted and appended to the image user
	# area with the addcard procedures.  

	call addcard_i (fd, "RECORD", NREC(ids), "IDS record")
	call addcard_i (fd, "EXPOSURE", ITM(ids), "Exposure time (seconds)")
	call addcard_i (fd, "OFLAG", OFLAG(ids), "Object flag")

	call addcard_i (fd, "BEAM-NUM", BEAM(ids), "Beam number")

	value = real (W0(ids))
	nrp = NDIGITS_RP
	call addcard_r (fd, "W0", value, "Starting wavelength", nrp)

	value = real (WPC(ids))
	call addcard_r (fd, "WPC", value, "Wavelength per channel", nrp)

	call addcard_i (fd, "NP1", NP1(ids), "Left plot limit")

	call addcard_i (fd, "NP2", NP2(ids), "Right plot limit")

	value = real (UT(ids) / 3600.)
	call addcard_time (fd, "UT", value, "Universal time")

	value = real (ST(ids)/ 3600.)
	call addcard_time (fd, "ST", value, "Sidereal time")

	value = real (RA(ids))
	call addcard_time (fd, "RA", value, "Right ascension")

	value = real (DEC(ids))
	call addcard_time (fd, "DEC", value, "Declination")

	value = real (HA(ids))
	call addcard_time (fd, "HA", value, "Hour angle")

	# The 9 reduction flags
	call addcard_i (fd, "DF-FLAG", DF_FLAG(ids), "Dispersion flag")
	call addcard_i (fd, "SM-FLAG", SM_FLAG(ids), "Smoothing flag")
	call addcard_i (fd, "QF-FLAG", QF_FLAG(ids), "Quartz fit flag")
	call addcard_i (fd, "DC-FLAG", DC_FLAG(ids), "Dispersion corrected")
	call addcard_i (fd, "QD-FLAG", QD_FLAG(ids), "Quartz division flag")
	call addcard_i (fd, "EX-FLAG", EX_FLAG(ids), "Extinction flag")
	call addcard_i (fd, "BS-FLAG", BS_FLAG(ids), "Beam switch flag")
	call addcard_i (fd, "CA-FLAG", CA_FLAG(ids), "Calibration flag")
	call addcard_i (fd, "CO-FLAG", CO_FLAG(ids), "Coincidence flag")

	# The df coeffecients are written out in the case where the df
	# flag is set, and the first coefficient is nonzero.  (The later
	# condition is a test for IDSOUT data, where the df coeffecients
	# have been applied but not stored in the header.)

	if (DF_FLAG(ids) != -1 && (! fp_equalr (Memr[COEFF(ids)], 0.))) {
	    call strcpy ("DF", keyword, LEN_KEYWORD)
   	    do i = 1, DF_FLAG(ids) {
		call sprintf (keyword[3], LEN_KEYWORD, "%s")
		    call pargi (i)
		call addcard_r (fd, keyword, Memr[COEFF(ids)+i-1], "", nrp)
	    }
	}

	# call strclose (fd)
	call close (fd)
end
