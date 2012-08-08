include	<error.h>
include	<fset.h>
include	<mach.h>
include	<imhdr.h>
include	"idsmtn.h"

# T_RIDSMTN -- Code for the IDS mountain format tape reader.  IDS
# records in raw or mountain reduced format can be read into a series of
# one dimensional IRAF images.  The first record on each mtn tape is a
# dummy record and is ignored.  Each IDS header is read and compared
# against the "record_numbers" list.  Depending on the user's request,
# the header can be printed in long or short form, an IRAF image can created.
#
# Modified 6May85 to key off the IIDS header parameters NP1 and NP2
# to copy to the output image only the pixels indicated.

procedure t_ridsmtn ()

pointer sp, cp, ids, vnb
char	ids_file[SZ_PATHNAME], rec_numbers[SZ_LINE]
int	file_number, records[3, MAX_RANGES], nrecs, nrecs_read, fd
int	unp1, unp2, i, sz_buffer

bool	clgetb(), is_in_range()
char	clgetc()
int	clgeti(), mtopen(), decode_ranges(), read()
int	get_data_type(), btoi(), mtfile(), mtneedfileno()

include	"lut.com"
include	"powersof2.com"

begin
	# Allocate space for the control parameter descriptor structure
	# and the program data structure

	call smark (sp)
	call salloc (cp, LEN_CP, TY_STRUCT)
	call salloc (ids, LEN_IDS, TY_STRUCT)

	# Initialize look up tables used to decode bytes of varian data.
	do i = 0, 255 {
	    neg_lut6[i] = and (not (i),  77B)
	    neg_lut7[i] = and (not (i), 177B)
	    neg_lut8[i] = and (not (i), 377B) 
	}

	# Initialize powers of 2 table used to assemble floating point numbers.
	do i = 1, 255
	    tbl[i] = 2.0D0 ** double (i - 129)

	# Get parameters from the cl and generate the input file name.  If
	# the input file is a general tape device, append the file_number suffix

	call clgstr ("ids_file", ids_file, SZ_FNAME)
	if (mtfile(ids_file) == YES) {
	    if (mtneedfileno (ids_file) == YES) {
	        file_number = clgeti ("file_number")
		call mtfname (ids_file, file_number, ids_file, SZ_FNAME)
	    }
	}

	IS_REDUCED(cp) = btoi (clgetb ("reduced_data"))
	LONG_HEADER(cp) = btoi (clgetb ("long_header"))
	PRINT_PIXELS(cp) = btoi (clgetb ("print_pixels"))
	call clgstr ("record_numbers", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (1, "Error in record_numbers specifications")

	# If an output image is to be written, get output data type and
	# root output data type.

	MAKE_IMAGE(cp) = btoi (clgetb ("make_image"))
	if (MAKE_IMAGE(cp) == YES) {
	    call clgstr ("iraf_file", IRAF_FILE(cp), SZ_FNAME)
	    OFFSET(cp) = clgeti ("offset")
	    DATA_TYPE(cp) = get_data_type (clgetc ("data_type"))
	    if (DATA_TYPE(cp) == ERR)
		DATA_TYPE(cp) = TY_REAL

	    unp1 = clgeti ("np1")
	    unp2 = clgeti ("np2")
	}

	fd = mtopen (ids_file, READ_ONLY, 0)
	sz_buffer = SZB_IDS_RECORD / SZB_CHAR
	if (mtfile (ids_file) == YES)
	    sz_buffer = sz_buffer + 64

	# Allocate input buffer in units of integers
	call salloc (vnb, sz_buffer * SZ_INT, TY_INT)
	
	nrecs_read = 0
	while (nrecs_read < nrecs) {

	    # Read IDS record into buffer.  Unpack bytes into integer array.
	    if (read (fd, Memi[vnb], sz_buffer) == EOF) {
		call printf ("IDS tape at End of File\n")
	        break
	    } else {
		call achtbi (Memi[vnb], Memi[vnb], SZB_IDS_RECORD)
		call vn_int_to_int (Memi[vnb], 1, NREC(ids))

		iferr {
	            if (is_in_range (records, NREC(ids))) {
		        nrecs_read = nrecs_read + 1
		        call idsm_read_record (Memi[vnb], cp, ids, unp1,unp2)
			call flush (STDOUT)
		    }
	        } then {
		    call erract (EA_WARN)
		    next
		}
	    }
	}

	call sfree (sp)
	call close (fd)
end


# IDSM_READ_RECORD -- is called once for each IDS record that appears in
# the "record_numbers" range.   The header is printed and the IDS pixels
# converted or skipped, depending on user request.

procedure idsm_read_record (vnb, cp, ids, unp1, unp2)

int	vnb[ARB]		# Buffer of unpacked IDS record - one byte/int
pointer	cp			# Pointer to control parameter data structure
pointer	ids			# Pointer to program data structure
int	unp1, unp2		# End points of the spectrum

pointer	sp, pixels
char	out_fname[SZ_FNAME]
int	stat
int	strlen(), idsm_read_header()
errchk	idsm_read_header, idsm_write_image, bswap4, vn_rraw
errchk	vn_rred, salloc, malloc

begin
	# Allocate space on stack for pixel buffers
	call smark (sp)
	call salloc (pixels, NPIX_IDS_REC, TY_REAL)
	call malloc (COEFF(ids), MAX_NCOEFF, TY_REAL)

	iferr (stat = idsm_read_header (vnb, ids)) {
	    call mfree (COEFF(ids), TY_REAL)
	    call sfree (sp)
	    call erract (EA_WARN)
	    return 
	}

	if (stat == DUMMY) {
	    call printf ("Dummy IDS record encountered\n")
	    call mfree (COEFF(ids), TY_REAL)
	    call sfree (sp)
	    return
	} else 
	    call idsm_print_header (ids, LONG_HEADER(cp))

	if (MAKE_IMAGE(cp) == YES || PRINT_PIXELS(cp) == YES) {

	    if (IS_REDUCED(cp) == YES)
	        call vn_rred (vnb, DATA_BYTE, Memr[pixels], NPIX_IDS_REC)
	    else
	        call vn_rraw (vnb, DATA_BYTE, Memr[pixels], NPIX_IDS_REC)
	    
	    if (MAKE_IMAGE(cp) == YES) {
	        call strcpy (IRAF_FILE(cp), out_fname, SZ_FNAME)
	        call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME, ".%04d")
	            call pargi (NREC(ids) + OFFSET(cp))
	        iferr {
		    call idsm_write_image (Memr[pixels], DATA_TYPE(cp), 
			IS_REDUCED(cp), out_fname, ids, unp1, unp2)
		} then {
	            call mfree (COEFF(ids), TY_REAL)
	            call sfree (sp)
		    call erract (EA_WARN)
		    return
		}
	    }
    
	    if (PRINT_PIXELS(cp) == YES)
	        call idsm_print_pixels (Memr[pixels], NP2(ids))

	}
	call mfree (COEFF(ids), TY_REAL)
	call sfree (sp)
end


# IDSM_READ_HEADER -- Read an IDS header and fill the program data
# structure with header values.  Returns EOF or OK.

int procedure idsm_read_header (varian, ids)

int	varian[ARB]		# Buffer of unpack varian record - one byte/int
pointer	ids			# Pointer to program data structure

errchk	vn_int_to_int, vn_long_to_int, vn_gdouble, reduction_flags
errchk	unpk_vn_id

begin
	# The following header words are written as Varian single word
	# integers.

	call vn_int_to_int (varian, NREC_OFFSET, NREC(ids))

	if (NREC(ids) == 0) 
	    # DUMMY IDS record encountered
	    return (DUMMY)
	 
	call vn_int_to_int (varian,   NP1_OFFSET,   NP1(ids))
	call vn_int_to_int (varian,   NP2_OFFSET,   NP2(ids))
	call vn_int_to_int (varian, OFLAG_OFFSET, OFLAG(ids))
	call vn_int_to_int (varian, SMODE_OFFSET, SMODE(ids))
	call vn_int_to_int (varian,  BEAM_OFFSET,  BEAM(ids))
	call vn_int_to_int (varian,   DRA_OFFSET,   DRA(ids))
	call vn_int_to_int (varian,  DDEC_OFFSET,  DDEC(ids))

	# Now unpack Varian double word integers into integers.
	call vn_long_to_int (varian, ITM_OFFSET, ITM(ids))
	call vn_long_to_int (varian,  UT_OFFSET,  UT(ids))
	call vn_long_to_int (varian,  ST_OFFSET,  ST(ids))

	# Now unpack those header words written in Varian 3 word floating point.
	call vn_gdouble (varian,  W0_OFFSET,  W0(ids))
	call vn_gdouble (varian, WPC_OFFSET, WPC(ids))
	call vn_gdouble (varian,  HA_OFFSET,  HA(ids))
	call vn_gdouble (varian,  RA_OFFSET,  RA(ids))
	call vn_gdouble (varian, DEC_OFFSET, DEC(ids))

	# Extract and interpret the 9 reduction flags
	call reduction_flags (varian, ids)

	# Unpack the IDS label
	call unpk_vn_id (varian, LABEL_OFFSET, LABEL(ids))

	return (OK)
end


# VN_INT_TO_INT -- Unpack a single integer value from a char buffer containing
# packed varian short (16-bit) integers.  The number of the word containing
# the value of interest is passed as an argument.

procedure vn_int_to_int (inbuf, offset, int_value)

int	inbuf[ARB]		# Unpacked IDS record
int	offset			# WORD offset as read from tape
int	int_value		# Integer value returned

int	bytes[2]

include	"lut.com"

begin
	call amovi (inbuf[offset], bytes, 2)
	if (bytes[1] < 127)
	    int_value = (bytes[1] * (2 ** 8)) + bytes[2]
	else {
	    bytes[1] = neg_lut8[bytes[1]] * (2 ** 8)
	    bytes[2] = neg_lut8[bytes[2]] 
	    int_value = -1 * (bytes[1] + bytes[2] + 1)
	}
end


# VN_LONG_TO_INT -- Unpack a SPP integer from a buffer containing packed
# varian long (32-bit) intgers.

procedure vn_long_to_int (inbuf, offset, int_value)

int	inbuf[ARB]	# Buffer containing unpacked varian longs one byte/int
int	offset		# Byte offset to field to be unpacked
int	int_value	# Integer value returned

int	bytes[4]

include	"lut.com"

begin
	call amovi (inbuf[offset], bytes, 4)

	if (bytes[1] < 127)
	    int_value = bytes[4] + (bytes[3] * (2 ** 8)) + (bytes[2] * 
	        (2 ** 15)) + (bytes[1] * (2 ** 23))
	 else {
	    bytes[1] = neg_lut8[bytes[1]] + (2 ** 23)
	    bytes[2] = neg_lut8[bytes[2]] + (2 ** 15)
	    bytes[3] = neg_lut7[bytes[3]] + (2 ** 8)
	    bytes[4] = neg_lut8[bytes[4]]
	    int_value = -1 * (bytes[1] + bytes[2] + bytes[3] + bytes[4] + 1)
	}
end


# VN_GDOUBLE -- Unpack a SPP double from a buffer containing packed
# varian 3-word (48-bit) floating point numbers.

procedure vn_gdouble (inbuf, offset, dbl_value)

int	inbuf[ARB]	# Buffer of unpacked varian 3wrd reals 1 byte/int
int	offset		# Word offset to value to be unpacked
double	dbl_value	# Double value returned

int	bytes[6], exp, mantissa

include	"lut.com"
include	"powersof2.com"

begin
	call amovi (inbuf[offset], bytes, 6)

	if (bytes[3] > 127) {
	    bytes[6] = neg_lut8[bytes[6]]
	    bytes[5] = neg_lut7[bytes[5]] * (2 ** 8)
	    bytes[4] = neg_lut8[bytes[4]] * (2 ** 15)
	    bytes[3] = neg_lut6[bytes[3]] * (2 ** 23)
	    mantissa = -1 * (bytes[6] + bytes[5] + bytes[4] + bytes[3] + 1)
	} else
	    mantissa = bytes[6] + (bytes[5] * (2 ** 8)) + (bytes[4] * 
	        (2 ** 15)) + (bytes[3] * (2 ** 23))

	if (bytes[1] > 127) {
	    bytes[1] = neg_lut8[bytes[1]] * (2 ** 8)
	    bytes[2] = neg_lut8[bytes[2]]
	    exp = -1 * (bytes[1] + bytes[2] + 1)
	} else
	    exp = bytes[2] + (bytes[1] * (2 ** 8))

	# Reconstruct the floating point number as a SPP real.  Powers of
	# two are stored in the tbl[] array where 2 ** n = tbl[n+129].
	# The mantissa is divided by 2 ** 29 to move the binary point
	# above bit 29.

	exp = exp + 129 - 29

	if (exp <= 0)
	    dbl_value = 0.0D0
	else if (exp > 255)
	    dbl_value = double (MAX_REAL)
	else if (exp > 0 && exp <= 255)
	    dbl_value = double (mantissa) * tbl[exp]
end


# IDSM_WRITE_IMAGE -- Write a one dimensional IRAF image of an IDS record.

procedure idsm_write_image (pixels, data_type, is_reduced, out_fname, 
	ids, unp1, unp2)

real	pixels[ARB]		# Array of unpacked IDS pixels
int	data_type		# Data type of output pixels
int	is_reduced		# Is data in reduced format?
char	out_fname[SZ_FNAME]	# Filename of output IRAF image
pointer	ids			# Pointer to program data structure
int	unp1, unp2		# Pixel endpoints of spectrum

int	pix1, pix2, npts, temp
pointer	im
pointer	immap(), impl1r()
errchk	immap, amovr, idsm_store_keywords, impl1r, imunmap

begin
	# Map new IRAF image and set up image header
	im = immap (out_fname, NEW_IMAGE, LEN_USER_AREA)
	IM_NDIM(im) = 1

	# Select the pixels to be written out.
	# When the data is in reduced format, write out the
	# entire vector.
	#
	# If the user parameter is zero, use the header elements.
	# On some IRS tapes, these parameters are backwards.

	if (NP1(ids) > NP2(ids)) {
	    temp = NP1(ids)
	    NP1(ids) = NP2(ids)
	    NP2(ids) = temp
	}

	if (is_reduced == YES) {
	    pix1 = 1
	    pix2 = NPIX_IDS_REC

	} else {

	    if (unp1 == 0)
	        pix1 = NP1(ids) + 1
	    else
	        pix1 = unp1

	    if (unp2 == 0)
	        pix2 = NP2(ids)
	    else
	        pix2 = unp2
	}

	npts = pix2 - pix1 + 1

	NP1(ids) = 0
	NP2(ids) = npts

	IM_LEN(im, 1) = npts
	call strcpy (LABEL(ids), IM_TITLE(im), SZ_IMTITLE)
	IM_PIXTYPE(im) = data_type

	# Move pixel_buffer into image row vector
	call amovr (pixels[pix1], Memr[impl1r(im)], npts)

	# Write IDS specific header words to IRAF image header
	# Update W0 for possible new starting pixel

	W0(ids) = W0(ids) + double (pix1 - 1) * WPC(ids)

	call idsm_store_keywords (ids, im)

	call imunmap (im)
end


# IDSM_PRINT_HEADER -- print the ids header in either long or short mode.  This
# procedure has been slightly modified from the version used in ridsfile or
# RIDSOUT.

procedure idsm_print_header (ids, long_header)

pointer	ids		# Pointer to program data structure
int	long_header	# Print header in long format (YES/NO)?
int	i

begin
	if (long_header == YES) {
	    call printf ("\nRECORD = %d, label = \"%s\",\n")
	        call pargi (NREC(ids))
	        call pargstr (LABEL(ids))

	    if (OFLAG(ids) == 1) {
		call printf ("oflag = OBJECT, beam_number = %d,")
		    call pargi (BEAM(ids))
	    } else {
		call printf ("oflag = SKY,    beam_number = %d,")
		    call pargi (BEAM(ids))
	    }

	    call printf ("   W0 = %0.3f,")
	        call pargd (W0(ids))
	    call printf ("    WPC = %0.3f,  ITM = %d,\n")
	        call pargd (WPC(ids))
		call pargi (ITM(ids))
	    call printf ("NP1 = %d, NP2 = %d,")
		call pargi (NP1(ids))
		call pargi (NP2(ids))
	    call printf ("    UT = %h,   ST = %h,")
		call pargr (UT(ids) / 3600.)
		call pargr (ST(ids) / 3600.)
	    call printf ("   HA = %h,\n")
		call pargd (HA(ids))
	    call printf ("RA = %h,   DEC = %h,")
		call pargd (RA(ids))
		call pargd (DEC(ids))
	    if (DRA(ids) != 0 || DDEC(ids) != 0) {
		call printf ("   DRA = %g,   DDEC = %g,\n")
		    call pargr (DRA(ids) / 100.)
		    call pargr (DDEC(ids) / 100.)
	    } else
		call printf ("\n")
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
	     # flag has been set.

	    if (DF_FLAG(ids) != -1) {
		call printf (",\n")
		do i = 1, DF_FLAG(ids) {
		    call printf ("df[%d] = %10.8g")
			call pargi(i)
			call pargr(Memr[COEFF(ids)+i-1])
		    if (i != DF_FLAG(ids))
		        call printf (", ")
		    if (mod (i, 4) == 0)
		        call printf ("\n")
		}
	    } else
		call printf ("\n")
	    call printf ("\n")
	} else {
	    call printf ("RECORD = %d, label = \"%.45s\", ITM = %d\n")
	        call pargi (NREC(ids))
	        call pargstr (LABEL(ids))
		call pargi (ITM(ids))
	}
end


# IDSM_PRINT_PIXELS -- Print the ids pixel values.  

procedure idsm_print_pixels (pixel_buf, nvalues)

real	pixel_buf[ARB]		# Buffer containing pixels to be listed
int	nvalues			# Number of pixel values to print
int	n_pix

begin
	for (n_pix = 1; n_pix < nvalues; n_pix = n_pix + 4) {
	    call printf ("%10.4e %10.4e %10.4e %10.4e\n")
	        call pargr (pixel_buf[n_pix])
	        call pargr (pixel_buf[n_pix + 1])
	        call pargr (pixel_buf[n_pix + 2])
	        call pargr (pixel_buf[n_pix + 3])
	}
	call printf ("\n")
end
