include	<mach.h>
include	<imhdr.h>
include	<error.h>
include "cyber.h"

# CY_READ_HEADER -- reads the IPPS header (64 60-bit words) into the 128 element
# integer array "header".  Any extraneous information between the header
# and data is skipped; the tape is left positioned at the first data
# record.

int procedure cy_read_header (rd, dt)

int	rd
pointer	dt
int  	header[NINT_CYBER_WRD * LEN_HEADER]
int	npru_skip
int	read_dumpf()
errchk	cy_unpk_header, read_dumpf, cy_skip_pru, unpack_cyber_record 
errchk  order_cyber_bits

begin
	# Read the header into array header, one cyber word per two elements
	if (read_dumpf (rd, header, LEN_HEADER) == EOF)
	    return (EOF)

	# Unpack bit stream and fill structure dt
	iferr {
	    call cy_unpk_header (header, dt)
	    npru_skip = PRU_ROW_ONE(dt) - 1 
	} then {
	    call erract (EA_WARN)
	    # Position to first row of raster before posting error
	    if (npru_skip > 0)
	        call cy_skip_pru (rd, npru_skip)
	    call error (1, "Bad header, attempting to skip raster")
	}

	# Position to first row of IPPS raster
	if (npru_skip > 0)
	    call cy_skip_pru (rd, npru_skip)

	return (OK)
end


# CY_LIST_HEADER -- prints the IPPS header information.

procedure cy_list_header (dt, file_number, raster_num)

pointer	dt
int	raster_num, file_number

begin
        # Print header information from IPPS raster
	call printf ("[%d.%d]%7t IPPS_ID: %s\n")
	    call pargi (file_number)
	    call pargi (raster_num)
	    call pargstr (IPPS_ID(dt))
	call printf ("%7t NCOLS=%d, NROWS=%d, MIN=%g, MAX=%g, NBPP=%d\n")
	    call pargi (NCOLS(dt))
	    call pargi (NROWS(dt))
	    call pargr (DATA_MIN(dt))
	    call pargr (DATA_MAX(dt))
	    call pargi (BITS_PIXEL(dt))
end


# CY_UNPK_HEADER -- unpacks header words from the char array header
# and fills the program data structure.   A few values are checked to
# make sure a valid IPPS raster is being read.  Offsets to various
# header words have been defined previously.

procedure cy_unpk_header (header, dt)

int	header[NINT_CYBER_WRD * LEN_HEADER]
pointer	dt

begin
	# From array header, first the ID is unpacked
	call unpk_id (header, 0, IPPS_ID(dt))

	# An EOR marker terminates each raster
	PRU_EOR(dt) = header[EOR_OFFSET]

	# The PRU containing the first data row
	PRU_ROW_ONE(dt) = header[FIRST_PRU_OFFSET]

	# Most significant 30 bits of the data min are used
	call unpk_60r (header, MIN_OFFSET, DATA_MIN(dt), 1)

	# Most significant 30 bits of the data max are used
	call unpk_60r (header, MAX_OFFSET, DATA_MAX(dt), 1)

	# Bits per pixel is unpacked and tested
	BITS_PIXEL(dt) = header[DATA_TYPE_OFFSET]

	switch (BITS_PIXEL(dt)) {
	case 12,20,30,60:
	    ;
	default:
	    call error (2, "Incorrect IPPS BITS_PIXEL")
	}

	# Number of columns is unpacked and tested
	NCOLS(dt) = header[NCOLS_OFFSET]
	if (NCOLS(dt) <= 0)
	    call error (2, "IPPS ncols <= 0")

	# Number of Cyber words per row must be integral # of PRU's
	WRDS_PER_ROW(dt) = header[NWORDS_OFFSET]
	NPRU_ROW(dt) = WRDS_PER_ROW(dt) / LEN_PRU

	if (mod (WRDS_PER_ROW(dt), LEN_PRU) != 0)
	    call error (2, "Invalid IPPS NWPR")

	# Number of rows is unpacked and tested
	NROWS(dt) = header[NROWS_OFFSET]
	if (NROWS(dt) <= 0)
	    call error (2, "IPPS nrows <= 0")
end
