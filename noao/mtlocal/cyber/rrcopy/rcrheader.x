include	<mach.h>
include	<imhdr.h>
include	<error.h>
include "rrcopy.h"

# RC_READ_HEADER -- reads the IPPS header (64 60-bit words) as
# a bit stream into consecutive elements of char array header. 
# Any extraneous information between the header and data is skipped;
# the tape is left positioned at the first data record.

int procedure rc_header_read (rd, rp)

int	rd
pointer	rp
char  	raw_header[SZ_HEADER], header[SZ_HEADER]
int	nchars_to_skip, first_word
int	rc_read_cyber()
errchk	rc_header_unpk, rc_skip_chars, rc_read_cyber, rc_order_cyber_bits

begin
	if (rc_read_cyber (rd, raw_header, SZ_HEADER) == EOF)
	    return (EOF)

	first_word = 1
	call rc_order_cyber_bits (raw_header, first_word, header, LEN_PRU)

	# Unpack bit stream and fill structure rp
	iferr {
	    call rc_header_unpk (header, rp)
	    nchars_to_skip = (PRU_ROW_ONE(rp) - 1) * NBITS_PRU / NBITS_CHAR
	} then {
	    call erract (EA_WARN)
	    # Position to first row of raster before posting error
	    if (nchars_to_skip > 0)
	        call rc_skip_chars (rd, nchars_to_skip)
	    call error (1, "Bad header, attempting to skip raster")
	}

	# Position to first row of IPPS raster
	if (nchars_to_skip > 0)
	    call rc_skip_chars (rd, nchars_to_skip)

	return (OK)
end


# RC_LIST_HEADER -- prints the RCOPY header information.

procedure rc_list_header (rp, raster_num)

pointer	rp
int	raster_num

begin
        # Print header information from rcopy tape
	call printf ("[%d]%7t IPPS_ID: %s\n")
	    call pargi (raster_num)
	    call pargstr (IPPS_ID(rp))
	call printf ("%7t NCOLS=%d, NROWS=%d, MIN=%g, MAX=%g, NBPP=%d\n")
	    call pargi (NCOLS(rp))
	    call pargi (NROWS(rp))
	    call pargr (DATA_MIN(rp))
	    call pargr (DATA_MAX(rp))
	    call pargi (BITS_PIXEL(rp))
end


# RC_UNPACK_HEADER -- unpacks header words from the char array header
# and fills the RCOPY data structure.   A few values are checked to
# make sure a valid IPPS raster is being read.  Offsets to various
# header words have been defined previously.

procedure rc_header_unpk (header, rp)

char 	header[SZ_HEADER]
pointer	rp

begin
	# From the reordered array, first the ID is unpacked
	call rc_up_id (header, IPPS_ID(rp))

	# An EOR marker terminates each raster
	call rc_up_60i (header, EOR_OFFSET, PRU_EOR(rp), 1)

	# The PRU containing the first data row
	call rc_up_60i (header, FIRST_PRU_OFFSET, PRU_ROW_ONE(rp), 1)

	# Most significant 30 bits of the data min are used
	call rc_up_60r (header, MIN_OFFSET, DATA_MIN(rp), 1)

	# Most significant 30 bits of the data max are used
	call rc_up_60r (header, MAX_OFFSET, DATA_MAX(rp), 1)

	# Bits per pixel is unpacked and tested
	call rc_up_60i (header, DATA_TYPE_OFFSET, BITS_PIXEL(rp), 1)

	switch (BITS_PIXEL(rp)) {
	case 12,20,30,60:
	    ;
	default:
	    call error (2, "Incorrect IPPS BITS_PIXEL")
	}

	# Number of columns is unpacked and tested
	call rc_up_60i (header, NCOLS_OFFSET, NCOLS(rp), 1)
	if (NCOLS(rp) <= 0)
	    call error (2, "IPPS ncols <= 0")

	# Number of Cyber words per row must be integral # of PRU's
	call rc_up_60i (header, NWORDS_OFFSET, WRDS_PER_ROW(rp), 1)

	if (mod (WRDS_PER_ROW(rp), LEN_PRU) != 0)
	    call error (2, "Invalid IPPS NWPR")

	# Number of rows is unpacked and tested
	call rc_up_60i (header, NROWS_OFFSET, NROWS(rp), 1)
	if (NROWS(rp) <= 0)
	    call error (2, "IPPS nrows <= 0")
end
