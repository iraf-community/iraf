include <imhdr.h>
include "idsmtn.h"

# STORE_KEYWORDS -- store IDS specific keywords in the IRAF image header.

procedure store_keywords (ids, im)

pointer	ids	# Pointer to program data structure
pointer	im	# Pointer to image

real	r, w, cd, mw_c1tranr()
pointer	mw, ct, smw_openim(), mw_sctran()
errchk	smw_openim, mw_sctran

begin
	mw = smw_openim (im)
	ct = mw_sctran (mw, "logical", "physical", 1)
	r = mw_c1tranr (ct, 1.)
	w = W0(ids)
	cd = WPC(ids)

	iferr (call mw_newsystem (mw, "onedspec", 1))
	    ;
	call mw_ssystem (mw, "world")
	call mw_swtermr (mw, r, w, cd, 1)
	call smw_saveim (mw, im)
	call mw_close (mw)

	call ids_addr (im, "EXPTIME", ITM(ids))
	call ids_addi (im, "OFLAG", OFLAG(ids))
	call ids_addi (im, "BEAM-NUM", BEAM(ids))
	call ids_addi (im, "NP1", NP1(ids))
	call ids_addi (im, "NP2", NP2(ids))
	call ids_addr (im, "AIRMASS", AIRMASS(ids))

	# Sexigesimal numbers.
	call ids_sex (im, "UT", UT(ids))
	call ids_sex (im, "ST", ST(ids))
	call ids_sex (im, "RA", RA(ids))
	call ids_sex (im, "DEC", DEC(ids))
	call ids_sex (im, "HA", HA(ids))

	# The 9 reduction flags
	call ids_addi (im, "DF-FLAG", DF_FLAG(ids))
	call ids_addi (im, "SM-FLAG", SM_FLAG(ids))
	call ids_addi (im, "QF-FLAG", QF_FLAG(ids))
	call ids_addi (im, "DC-FLAG", DC_FLAG(ids))
	call ids_addi (im, "QD-FLAG", QD_FLAG(ids))
	call ids_addi (im, "EX-FLAG", EX_FLAG(ids))
	call ids_addi (im, "BS-FLAG", BS_FLAG(ids))
	call ids_addi (im, "CA-FLAG", CA_FLAG(ids))
	call ids_addi (im, "CO-FLAG", CO_FLAG(ids))
end

#  IDS_ADDI -- Add a integer parameter to the image header.

procedure ids_addi (im, field, value)

pointer	im			# IMIO pointer
char	field[ARB]		# Header parameter
int	value			# Value

begin
	if (!IS_INDEFI (value)) {
#	    iferr (call imdelf (im, field))
#		;
	    call imaddi (im, field, value)
	}
end

#  IDS_ADDR -- Add a real parameter to the image header.

procedure ids_addr (im, field, value)

pointer	im			# IMIO pointer
char	field[ARB]		# Header parameter
real	value			# Value

begin
	if (!IS_INDEFR (value)) {
	    iferr (call imdelf (im, field))
		;
	    call imaddr (im, field, value)
	}
end

#  IDS_SEX -- Format and add a sexigesimal string parameter to the image header.

procedure ids_sex (im, field, value)

pointer	im			# IMIO pointer
char	field[ARB]		# Header parameter
real	value			# Value

char	str[20]

begin
	if (!IS_INDEFR (value)) {
	    iferr (call imdelf (im, field))
		;
	    call sprintf (str, 20, "%-18.1h")
	        call pargr (value)
	    call imastr (im, field, str)
	}
end
