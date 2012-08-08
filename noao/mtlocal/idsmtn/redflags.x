include	<mach.h>
include	"idsmtn.h"

define	RED_FLAG_OFFSET 3	# Byte offset (starts at bit 17)
define	DF_ORD_OFFSET	9	# Byte offset to df order value (bit 65)
define	DFB		4	# Number of bytes per DF coefficient
define	FIRST_BYTE	13

# REDUCTION_FLAGS -- Extract and interpret the reduction flags found in
# the IDS header.  9 flags are used: DF, SM, QF, DC, QD, EX, BS, CA, CO.
# Each flag is represented by a single bit in a varian 16-bit integer.
# If a flag is set, it's value = 0; unset flags = -1.  If the QF flag is
# set, the order of the fitting polynomial must be unpacked.  If the DF
# flag is set, the order of the fitting polynomial as well as the 
# coefficients must be unpacked from the header.  The location of the
# coefficients depends on the order of the fit.  

procedure reduction_flags (vn, ids)

int	vn[ARB]		# Input buffer of IDS record - one byte per int
pointer	ids		# Pointer to ids header structure

int	flag_word, offset, byte_offset, op
real	rtemp, tmp[MAX_NCOEFF]
errchk	vn_rred

begin
	# Initialize flags to -1
	DF_FLAG(ids) = -1
	SM_FLAG(ids) = -1
	QF_FLAG(ids) = -1
	DC_FLAG(ids) = -1
	QD_FLAG(ids) = -1
	EX_FLAG(ids) = -1
	BS_FLAG(ids) = -1
	CA_FLAG(ids) = -1
	CO_FLAG(ids) = -1

	# Unpack the flag_word from the header and determine flags.  If a
	# flag_bit is set, the corresponding flag is true and set = 0.

	flag_word = vn[RED_FLAG_OFFSET] * (2 ** 8) + vn[RED_FLAG_OFFSET+1]

	if (and (flag_word,    40B) != 0)
	    CO_FLAG(ids) = 0

	if (and (flag_word,   100B) != 0)
	    CA_FLAG(ids) = 0

	if (and (flag_word,   200B) != 0)
	    BS_FLAG(ids) = 0

	if (and (flag_word,   400B) != 0)
	    EX_FLAG(ids) = 0

	if (and (flag_word,  1000B) != 0)
	    QD_FLAG(ids) = 0

	if (and (flag_word,  2000B) != 0)
	    DC_FLAG(ids) = 0

	if (and (flag_word,  4000B) != 0) 
	    # The qf_flag is set equal to the degree of the fitting polynomial.
	    # This value is stored in the lowest 5-bits of the flag word.
	    QF_FLAG(ids) = and (flag_word, 37B)

	if (and (flag_word, 10000B) != 0)
	    SM_FLAG(ids) = 0

	# The df_flag is interpreted next:
	if (and (flag_word, 20000B) != 0) {
	    # The degree of the fitting polynomial is written in 2wrd_vn_fp at
	    # the location of the first data pixel.  The df_flag is set equal
	    # to the integer value of the polynomial degree.

	    call vn_rred (vn, DF_ORD_OFFSET, rtemp, 1)
	    DF_FLAG(ids) = int (rtemp)

	    # Now to unpack the coefficients.  The coefficients have been 
	    # written over pixels at the beginning and end of the IDS scan.
	    # The number and location of the overwritten pixels depends on
	    # the order of fit and values of NP1 and NP2.

	    do op = 1, DF_FLAG(ids) {
	        offset = op
		if (offset >= NP1(ids))
		    # This coefficient must be stored at the end of scan
		    offset = offset + NP2(ids) - NP1(ids)

		byte_offset = FIRST_BYTE + ((offset - 1) * DFB)
		call vn_rred (vn, byte_offset, tmp[op], 1)
	    }

	    # Copy decoded coefficients into structure
	    call amovr (tmp, Memr[COEFF(ids)], DF_FLAG(ids))
	}
end
