include	<mach.h>
include <imhdr.h>
include	<fset.h>
include	<error.h>
include	"cyber.h"


# T_RIDSFILE __ code for the DUMPF IDSFILE reader.  IDS records in an IDSFILE 
# are read from a Cyber DUMPF tape and optionally converted to IRAF images.
# IDS records are not written sequentially in the IDSFILE, so, each record
# must be read and then checked against the  list of "record_numbers" to
# see if the user requested the record to be read.  The procedure terminates
# when the requested number of records has been read or EOF is encountered.
# The IDS trailer information is printed in either a short or long form;
# the pixel values can also be printed.

procedure t_ridsfile()

pointer	sp, cp
char	in_fname[SZ_FNAME], dumpf_file[SZ_FNAME]
int	file_ordinal

int	mtfile(), clgeti(), get_data_type(), btoi()
bool	clgetb()
char	clgetc()

begin
	# Allocate space for the control parameter descriptor structure
	call smark (sp)
	call salloc (cp, LEN_CP, TY_STRUCT)

	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get parameters from cl and generate input file name.  If the input
	# file is a tape, append the file_ordinal suffix, incremented by one
	# to skip over the DUMPF tape label.

	call clgstr ("dumpf_file", dumpf_file, SZ_FNAME)
	if (mtfile (dumpf_file) == YES) {
	    file_ordinal = clgeti ("file_ordinal")
	    call mtfname (dumpf_file, file_ordinal + 1, in_fname, SZ_FNAME)
	} else
	    call strcpy (dumpf_file, in_fname, SZ_FNAME)

	LONG_HEADER(cp) = btoi (clgetb ("long_header"))
	PRINT_PIXELS(cp) = btoi (clgetb ("print_pixels"))
	call clgstr ("record_numbers", REC_NUMBERS(cp), SZ_LINE)

	# If an output image is to be written, get root output file name and
	# output data type.
	MAKE_IMAGE(cp) = btoi (clgetb ("make_image"))
	if (MAKE_IMAGE(cp) == YES) {
	    call clgstr ("iraf_file", IRAF_FILE(cp), SZ_FNAME)
	    DATA_TYPE(cp) = get_data_type (clgetc ("data_type"))
	    if (DATA_TYPE(cp) == ERR)
		DATA_TYPE(cp) = TY_REAL
	}
	call read_idsfile (in_fname, cp)

	call sfree (sp)
end


# READ_IDSFILE -- read and sort the index of record ranges.  Call 
# idsf_read_record for each record in each index range.

procedure read_idsfile (in_fname, cp)

char	in_fname[SZ_FNAME]	# Name of input file
pointer	cp			# Pointer to control parameter structure

int	records[3, MAX_RANGES], nrecs, i
pointer	sp, pft, pru_buf
int	fd, junk, index_buf[LEN_INDEX * NINT_CYBER_WRD], nranges, nids_read
int	current_pru, n_index, next_pru, nrecords_to_read, npru_skip, n_rec
long	sorted_index[LEN_INDEX]

int	mtopen(), get_cyber_words_init(), read_dumpf_init(), read_dumpf()
int	get_cyber_words(), idsf_read_record(), decode_ranges()
errchk	mtopen, read_dumpf, get_cyber_words, idsf_read_record
errchk	sort_index, decode_ranges

begin
	# Allocate space for program data structure and buffers
	call smark (sp)
	call salloc (pft, NINT_CYBER_WRD * LEN_PFT, TY_INT)
	call salloc (pru_buf, NINT_CYBER_WRD * LEN_PRU, TY_INT)

	# Open and initialize the tape file, and read the permanent file table
	fd = mtopen (in_fname, READ_ONLY, SZ_TAPE_BUFFER)
	junk = get_cyber_words_init()
	junk = read_dumpf_init()
	if (get_cyber_words (fd, Memi[pft], LEN_PFT) == EOF) {
	    call printf ("DUMPF tape at EOT\n")
	    call sfree (sp)
	    call close (fd)
	    return
	}

	# Read and sort IDSFILE user index information.  The first two
	# pru's of this index are relevant.  Up to 3 more pru's can
	# follow, depending on the format of the idsfile.  The code was
	# modified 13Jan86 to read an old format tape of Paul Hintzen's
	# and hopefully provide a general solution to the problem of 
	# different formats.

	if (read_dumpf (fd, index_buf, LEN_USER_INDEX)== EOF) {
	    call close (fd)
	    call error (1, "Unexpected EOF when reading index")
	}
	if (decode_ranges (REC_NUMBERS(cp), records, MAX_RANGES, junk) == ERR)
	    call error (2, "Error in record_numbers specification")

	call sort_index (index_buf, records, sorted_index, nranges, nrecs)

	# Loop over each range of records in the index.  nids_read counts
	# the number of records requested by the user that have been read.
	# nrecords_to_read is the number of records in the current index range.

	nids_read = 0
	current_pru = 3
	for (n_index = 1; n_index <= nranges; n_index = n_index + 1) {
            next_pru = sorted_index[n_index] / 1000
	    nrecords_to_read = mod (sorted_index[n_index], 1000)
	    npru_skip = next_pru - current_pru
	    do i = 1, npru_skip {
	        if (read_dumpf (fd, Memi[pru_buf], LEN_PRU) == EOF) {
	            # At end of IDSFILE
	            call printf ("DUMPF tape at EOF\n")
		    break
	        }
	    }

	    current_pru = current_pru + npru_skip

	    # Loop over each record within the current range of records
	    for (n_rec = 1; n_rec <= nrecords_to_read; n_rec = n_rec + 1) {
	        if (nids_read >= nrecs)  {
	            # No need to continue
		    call close (fd)
		    call sfree (sp)
		    return
	        }

		if (idsf_read_record (fd, records, nrecs, nids_read, 
		    cp) == EOF) {
		    call close (fd)
		    call sfree (sp)
		    return
		}
		    

		current_pru = current_pru + (LEN_IDS_RECORD / LEN_PRU)
	    }
	}

	call close (fd)
	call sfree (sp)
end


# IDSF_READ_RECORD -- reads a single idsrecord.  If the record is in the
# set of records to be read, the record is processed and the count of requested
# records read is incremented.

int procedure idsf_read_record (fd, records, nrecs, nids_read, cp)

int	fd			# File descriptor of input file
int	records[3, MAX_RANGES]	# Array of ranges of records specified by user
int	nrecs			# Number of requested records found on tape
int	nids_read		# Number of requested records already read
pointer	cp			# Pointer to control parameter structure

char	out_fname[SZ_FNAME]
pointer	sp, ids
int	ids_buffer[LEN_IDS_RECORD * NINT_CYBER_WRD], this_record
int	tape, scan
real	pixels[NPIX_IDS_RECORD]

bool	is_in_range()
int	read_dumpf(), bitupk(), strlen()
errchk	read_dumpf, read_header, idsf_write_image, list_values

begin
	# Allocate space for program data structure
	call smark (sp)
	call salloc (ids, LEN_IDS, TY_STRUCT)

	# Read the next ids record
	if (read_dumpf (fd, ids_buffer, LEN_IDS_RECORD) == EOF) {
	    # At end of IDSFILE
		call printf ("DUMPF tape at EOF\n")
		call sfree (sp)
		return (EOF)
	}

	scan = bitupk (ids_buffer, SCAN_OFFSET, NBITS_INT)
	tape = bitupk (ids_buffer, TAPE_OFFSET, NBITS_INT)
	this_record = (tape * 1000) + scan
        if (is_in_range (records, this_record)) {
	    nids_read = nids_read + 1
	    RECORD_NUMBER(ids) = this_record
	    iferr {
	        call calloc (COEFF(ids), MAX_COEFF, TY_DOUBLE)
		call idsf_read_header (ids_buffer, ids)
	    } then {
		call erract (EA_WARN)
		call mfree (COEFF(ids), TY_DOUBLE)
		call sfree (sp)
		return (ERR)
	    }

	    call print_header (ids, LONG_HEADER(cp))

	    if (MAKE_IMAGE(cp) == YES) {
		call strcpy (IRAF_FILE(cp), out_fname, SZ_FNAME)
		call sprintf (out_fname[strlen(out_fname) + 1], SZ_FNAME, ".%d")
		        call pargi (RECORD_NUMBER(ids))
		iferr {

		    call idsf_write_image (ids_buffer, DATA_TYPE(cp), 
		        PRINT_PIXELS(cp), out_fname, ids)

		} then {
		    call ERRACT (EA_WARN)
		    call mfree (COEFF(ids), TY_DOUBLE)
		    call sfree (sp)
		    return (ERR)
		}
	    } 
		    
	    if (PRINT_PIXELS(cp) == YES && MAKE_IMAGE(cp) == NO) {
		call unpk_30 (ids_buffer, 1, pixels, NPIX_IDS_RECORD)
		call list_values (pixels)
	    }
	    call mfree (COEFF(ids), TY_DOUBLE)
	}

	call sfree (sp)
	return (OK)
end


# SORT_INDEX -- Sort index information that precedes each IDSFILE.  This
# index occupies 5 PRU's and points to ranges of records.  Each index
# entry contains a PRU number and the low and high record numbers of the
# records that begin at the stated PRU.  These three pieces of information
# are stored in a single 60-bit Cyber word.  The number of records requested
# by the user that are actually in the IDSFILE is also counted.  This
# number is returned as a parameter to the calling procedure.

procedure sort_index (index_buf, records, sorted_index, nranges, nrecs_on_tape)

int	index_buf[ARB]		# Buffer containing IDS index information
int	records[3, MAX_RANGES]	# Array of ranges of records specified by user
long	sorted_index[LEN_INDEX]	# Returned array of sorted index information
int	nranges			# Number of ranges of IDS records in IDSFILE
int	nrecs_on_tape		# Number of requested records actually on tape

int	i, start_pru, low_record_number, high_record_number, nrecs, j
long	index[LEN_INDEX]
bool	is_in_range()
int	bitupk()
errchk	asrtl, bitupk

begin
	nrecs_on_tape = 0
	nranges = 0
	do i = 1, NINT_CYBER_WRD * LEN_USER_INDEX, NINT_CYBER_WRD {
	    start_pru = bitupk (index_buf[i], NPRU_OFFSET, NBITS_NPRU)
	    if (start_pru == 0)
		next
	    low_record_number = bitupk (index_buf[i], LRN_OFFSET, NBITS_LRN)
	    high_record_number = bitupk (index_buf[i], HRN_OFFSET, NBITS_HRN)
	    nrecs = high_record_number - low_record_number + 1
	    nranges = nranges + 1
	    index[nranges] = real (start_pru * 1000) + nrecs

	    for (j=low_record_number; j<=high_record_number; j=j+1) {
		if (is_in_range (records, j))
		    nrecs_on_tape = nrecs_on_tape + 1
	    }
	}

	call asrtl (index, sorted_index, nranges)
end


# LIST_VALUES -- Print the ids pixel values.  

procedure list_values (pixel_buf)

real	pixel_buf[NPIX_IDS_RECORD]	# Buffer containing pixels to be listed
int	n_pix

begin
	for (n_pix = 1; n_pix <= NPIX_IDS_RECORD; n_pix = n_pix + 4) {
	    call printf ("%10.4e %10.4e %10.4e %10.4e\n")
	        call pargr (pixel_buf[n_pix])
	        call pargr (pixel_buf[n_pix + 1])
	        call pargr (pixel_buf[n_pix + 2])
	        call pargr (pixel_buf[n_pix + 3])
	}
	call printf ("\n")
end

# IDSF_READ_HEADER -- Decode ids header parameters from the input buffer and
# fill the program data structure.

procedure idsf_read_header (ids_buffer, ids)

int	ids_buffer[NINT_CYBER_WRD*LEN_IDS_RECORD]	# Input IDSFILE buffer
pointer	ids				# Pointer to program data structure

int	n_coeff, i
char	alpha[3]
int	bitupk()
double	convert_60bit_fp()
errchk	bitupk, unpk_60i, convert_60bit_fp, unpk_id, display_code

begin
	# Get unsigned integer parameters from header
	ITM(ids) = bitupk (ids_buffer, ITM_OFFSET, NBITS_INT)
	NP1(ids) = bitupk (ids_buffer, NP1_OFFSET, NBITS_INT)
	NP2(ids) = bitupk (ids_buffer, NP2_OFFSET, NBITS_INT)
	BEAM_NUMBER(ids) = bitupk (ids_buffer, BEAM_OFFSET, NBITS_INT)
	SMODE(ids) = bitupk (ids_buffer, SMODE_OFFSET, NBITS_INT)
	if (SMODE(ids) != 0) {
	    # Determine companion record number
	    if (BEAM_NUMBER(ids) == 1)
		COMPANION_RECORD(ids) = RECORD_NUMBER(ids) - 1
	    else 
		COMPANION_RECORD(ids) = RECORD_NUMBER(ids) + 1
	}
	UT(ids) = bitupk (ids_buffer, UT_OFFSET, NBITS_INT)
	ST(ids) = bitupk (ids_buffer, ST_OFFSET, NBITS_INT)

	# The following integer parameters can be negative
	call unpk_60i (ids_buffer, DF_OFFSET, DF_FLAG(ids), 1)
	call unpk_60i (ids_buffer, SM_OFFSET, SM_FLAG(ids), 1)
	call unpk_60i (ids_buffer, QF_OFFSET, QF_FLAG(ids), 1)
	call unpk_60i (ids_buffer, DC_OFFSET, DC_FLAG(ids), 1)
	call unpk_60i (ids_buffer, QD_OFFSET, QD_FLAG(ids), 1)
	call unpk_60i (ids_buffer, EX_OFFSET, EX_FLAG(ids), 1)
	call unpk_60i (ids_buffer, BS_OFFSET, BS_FLAG(ids), 1)
	call unpk_60i (ids_buffer, CA_OFFSET, CA_FLAG(ids), 1)
	call unpk_60i (ids_buffer, CO_OFFSET, CO_FLAG(ids), 1)
	call unpk_60i (ids_buffer, OFLAG_OFFSET, OFLAG(ids), 1)  

	# If the dispersion flag (DF) is set, get the coeffecients.  The pointer
	# to the coeffecient array is stored in the structure ids.
	if (DF_FLAG(ids) > -1) {
	    n_coeff = DF_FLAG(ids)
	    do i = 1, n_coeff {
	        Memd[COEFF(ids)+i-1] = convert_60bit_fp (ids_buffer, 
	           (COEFF_OFFSET + (i - 1)) * 64 + 1)
	    }
	}

	# These header values converted from Cyber 60-bit floating point
	          HA(ids) = convert_60bit_fp (ids_buffer, HA_OFFSET)
	     AIRMASS(ids) = convert_60bit_fp (ids_buffer, AIR_OFFSET)
	          RA(ids) = convert_60bit_fp (ids_buffer, RA_OFFSET)
	         DEC(ids) = convert_60bit_fp (ids_buffer, DEC_OFFSET)
	     LAMBDA0(ids) = convert_60bit_fp (ids_buffer, LAM_OFFSET)
	DELTA_LAMBDA(ids) = convert_60bit_fp (ids_buffer, DEL_OFFSET)

	# The 3 character ALPHA_ID is stored in Cyber display code
	call display_code (bitupk (ids_buffer, ALPHA1_OFFSET, NBITS_DC), 
	    alpha[1])
	call display_code (bitupk (ids_buffer, ALPHA2_OFFSET, NBITS_DC), 
	    alpha[2])
	call display_code (bitupk (ids_buffer, ALPHA3_OFFSET, NBITS_DC), 
	    alpha[3])
	call strcpy (alpha, ALPHA_ID(ids), NCHAR_ALPHA)

	# The ids label is written in 7-bit ascii
	call unpk_id (ids_buffer, IDS_ID_OFFSET, LABEL(ids))
end


# PRINT_HEADER -- print the ids header in either long or short mode.

procedure print_header (ids, long_header)

pointer	ids		# Pointer to program data structure
int	long_header	# Print header in long format (YES/NO)?
int	i

real	value1, value2

begin
	if (long_header == YES) {
	    call printf ("RECORD = %d, label = \"%s\",\n")
	        call pargi (RECORD_NUMBER(ids))
	        call pargstr (LABEL(ids))

	    if (OFLAG(ids) == 1) {
		call printf ("oflag = OBJECT, beam_number = %d,   ")
		    call pargi (BEAM_NUMBER(ids))
	    } else if (OFLAG (ids) == 0) {
		call printf ("oflag = SKY,    beam_number = %d,   ")
		    call pargi (BEAM_NUMBER(ids))
	    }
            call printf ("alpha_ID = %s")
		call pargstr (ALPHA_ID(ids))
	    if (SMODE(ids) != 0) {
	        call printf (", companion = %d,\n")
		    call pargi (COMPANION_RECORD(ids))
	    } else
		call printf (",\n")

	    call printf ("airmass = %5.3f,%24tW0 = %0.3f,")
	        call pargd (AIRMASS(ids))
	        call pargd (LAMBDA0(ids))
	    call printf ("    WPC = %0.3f,      ITM = %d,\n")
	        call pargd (DELTA_LAMBDA(ids))
		call pargi (ITM(ids))
	    call printf ("NP1 = %d, NP2 = %d,")
		call pargi (NP1(ids))
		call pargi (NP2(ids))

	    if (IS_INDEFI (UT(ids)))
		value1 = INDEFR
	    else 
		value1 = real (UT(ids) / 3600.)

	    if (IS_INDEFI (ST(ids)))
		value2 = INDEFR
	    else
		value2 = real (ST(ids) / 3600.)
	    call printf ("    UT = %h,   ST = %h,\n")
		call pargr (value1)
		call pargr (value2)

	    call printf ("HA = %h,")
		call pargd (HA(ids))
	    call printf ("        RA = %h,   DEC = %h,\n")
		call pargd (RA(ids))
		call pargd (DEC(ids))
	    call printf ("df = %d, sm = %d, qf = %d, dc = %d, qd = %d, ") 
		call pargi (DF_FLAG(ids))
		call pargi (SM_FLAG(ids))
		call pargi (QF_FLAG(ids))
		call pargi (DC_FLAG(ids))
		call pargi (QD_FLAG(ids))
	    call printf ("ex = %d, bs = %d, ca = %d, co = %d")
		call pargi (EX_FLAG(ids))
		call pargi (BS_FLAG(ids))
		call pargi (CA_FLAG(ids))
		call pargi (CO_FLAG(ids))

	    # The df coeffecients are printed out in the case where the df
	    # flag is set, and the first coefficient is nonzero.  The later
	    # condition is a test for IDSOUT data, where the df coeffecients
	    # have been applied but not stored in the header.

	    if (DF_FLAG(ids) != -1 && COEFF(ids) != 0) {
		call printf (",\n")
		do i = 1, DF_FLAG(ids) {
		    call printf ("df[%d] = %10.8g")
			call pargi(i)
			call pargd(Memd[COEFF(ids)+i-1])
		    if (i != DF_FLAG(ids))
		        call printf (", ")
		    if (mod (i, 4) == 0)
		        call printf ("\n")
		}
		call printf ("\n")
	    } else
		call printf ("\n")
	    call printf ("\n")
	} else {
	    call printf ("RECORD = %d, label = \"%s\"\n")
	        call pargi (RECORD_NUMBER(ids))
	        call pargstr (LABEL(ids))
	}
end


# IDSF_WRITE_IMAGE -- pixels are unpacked from the input buffer and written to
# a one dimensional IRAF image.  

procedure idsf_write_image (ids_buffer, data_type, print_pixels, out_fname, 
    ids)

int	ids_buffer[NINT_CYBER_WRD * LEN_IDS_RECORD]	# Input IDSFILE buffer
int	data_type			# Data type of pixels to be written
int	print_pixels			# List pixel values (YES/NO)?
char	out_fname[SZ_FNAME]		# Name of output image
pointer	ids				# Pointer to program data structure

pointer	im, pixels
pointer	impl1r(), immap()
errchk	immap, unpk_30, cy_store_keywords, imunmap

begin
	# Map new iraf image and set up image header
	im = immap (out_fname, NEW_IMAGE, LEN_USER_AREA)
	IM_NDIM(im) = 1
	IM_LEN(im, 1) = NPIX_IDS_RECORD
	call strcpy (LABEL(ids), IM_TITLE(im), SZ_IMTITLE)
	IM_PIXTYPE(im) = data_type
	pixels = impl1r(im)

	# Convert pixels to spp reals and write image line
	call unpk_30 (ids_buffer, 1, Memr[pixels], NPIX_IDS_RECORD)

	if (print_pixels == YES)
	    call list_values (Memr[pixels])

	# Write ids specific header words to iraf image header
	call cy_store_keywords (ids, im) 

	call imunmap (im)
end
