include	<mach.h>
include	<ctype.h>
include	<imhdr.h>
include	<fset.h>
include	<error.h>
include	"cyber.h"

define	MAX_WIDTH	10	# Maximum format width for pixel data

# T_RIDSOUT --  the IDSOUT format reader, which reads a text file of IDS
# records in IDSOUT format.  Each IDS record contains 133 card images: 4
# cards of header information followed by 128 cards of pixel values and
# one blank card.  The IDSOUT reader will read the text file and optionally
# convert the data to a sequence of one dimensional IRAF images.  The
# header information can be printed in long or short form; the pixel values
# can also be listed.

procedure t_ridsout ()

pointer	sp, cp
int	fd
char	in_fname[SZ_FNAME]
int	get_data_type(), clpopni(), clgfil(), btoi()
bool	clgetb()
char	clgetc()
errchk	clpopni

begin
	# Allocate space for the control parameter descriptor structure
	call smark (sp)
	call salloc (cp, LEN_CP, TY_STRUCT)

	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get parameters from the cl and fill control parameter structure
	fd = clpopni ("idsout_file")

	LONG_HEADER(cp) = btoi (clgetb ("long_header"))
	PRINT_PIXELS(cp) = btoi (clgetb ("print_pixels"))
	call clgstr ("record_numbers", REC_NUMBERS(cp), SZ_LINE)

	# If an output image is to be written, get root output file name 
	# and output data type.

	MAKE_IMAGE(cp) = btoi (clgetb ("make_image"))
	if (MAKE_IMAGE(cp) == YES) {
	    call clgstr ("iraf_file", IRAF_FILE(cp), SZ_FNAME)
	    DATA_TYPE(cp) = get_data_type (clgetc ("data_type"))
	    if (DATA_TYPE(cp) == ERR)
		DATA_TYPE(cp) = TY_REAL
	}

	while (clgfil (fd, in_fname, SZ_FNAME) != EOF)  {
	    call read_idsout (in_fname, cp)
	}

	call clpcls (fd)
	call sfree (sp)
end


# READ_IDSOUT -- open IDSOUT text file and direct processing depending on 
# user's request.

procedure read_idsout (in_fname, cp)

char	in_fname[SZ_FNAME]	# Input file name
pointer	cp			# Pointer to control parameter structure

pointer	sp, ids
char	out_fname[SZ_FNAME]
int	in, records[3, MAX_RANGES], nrecs, nrecs_read, stat
bool	is_in_range()
int	open(), decode_ranges(), idso_read_header(), strlen()
errchk	open, read, idso_read_header, decode_ranges, idso_write_image
errchk	copy_pixels

begin
	# Open text file and allocate space for descriptor structure ids
	in = open (in_fname, READ_ONLY, TEXT_FILE)
	call smark (sp)
	call salloc (ids, LEN_IDS, TY_STRUCT)

	if (decode_ranges (REC_NUMBERS(cp), records, MAX_RANGES, nrecs) == ERR)
	    call error (1, "Error in record_numbers specification")

	nrecs_read = 0
	repeat {
	    iferr {
	        stat = idso_read_header (in, ids)
	    } then {
		call erract (EA_WARN)
		call eprintf ("Bad header, attempting to skip pixels\n")
		call copy_pixels (in, NULL)
		next
	    }

	    if (stat == EOF) {
		call printf ("\nIDSOUT file \"%s\" at EOF\n")
		    call pargstr (in_fname)
		call close (in)
		call sfree (sp)
		return 
	    }

	    if (is_in_range (records, RECORD_NUMBER(ids))) {
		nrecs_read = nrecs_read + 1
		call print_header (ids, LONG_HEADER(cp))
		
		if (MAKE_IMAGE(cp) == YES) {
		    call strcpy (IRAF_FILE(cp), out_fname, SZ_FNAME)
		    call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME,
			".%04d")
			call pargi (RECORD_NUMBER(ids))
		    iferr {
	    	        call idso_write_image (in, DATA_TYPE(cp), 
			    PRINT_PIXELS(cp), out_fname, ids)
		    } then  {
			call erract (EA_WARN)
			if (PRINT_PIXELS(cp) == YES)
			    call copy_pixels (in, STDOUT)
			else
			    call copy_pixels (in, NULL)
			next
		    }

		} else if (PRINT_PIXELS(cp) == YES)
	    	    # Simply copy card image data to standard output
	    	    call copy_pixels (in, STDOUT)

		if (PRINT_PIXELS(cp) == NO && MAKE_IMAGE(cp) == NO)
		    call copy_pixels (in, NULL)
	    } else
	        call copy_pixels (in, NULL)

	} until (nrecs_read == nrecs)

	call sfree (sp)
	call close (in)
end


# IDSO_READ_HEADER -- decode header parameter from IDSOUT header cards.  The
# IDSO prefix implies IDSOUT format; IDSF is used for IDSFILE format.  The
# first four cards of each record in the IDSOUT file are the header.

int procedure idso_read_header (in, ids)

int	in		# File descriptor of input text file
pointer	ids		# Pointer to program data structure

pointer	sp, temp
int	record, tape, junk, ip
int	fscan(), nscan(), strlen(), getline()
errchk	fscan

begin
	# Allocate space on stack for temporary char storage
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)

	# Skip any blank lines that may preceede the first header card.
	# Break out of repeat when first non-blank line is encountered.

	repeat {
	    if (getline (in, Memc[temp]) == EOF) {
	        call sfree (sp)
	        return (EOF)
	    } else {
		for (ip = temp; IS_WHITE (Memc[ip]); ip = ip + 1) 
		    ;
		if (Memc[ip] == '\n' || Memc[ip] == EOS)
		    # Blank line
		    ;
		else
		    break
	    }
	}

	call sscan (Memc[temp]) {
	    # Values to be read from first card of file:
	    call gargi (record)
	    call gargi (ITM(ids))
	    call gargd (LAMBDA0(ids))
	    call gargd (DELTA_LAMBDA(ids))
	    call gargi (NP1(ids))
	    call gargi (NP2(ids))
	    call gargi (BEAM_NUMBER(ids))
	    call gargi (junk)
	    call gargi (junk)
	    call gargi (SMODE(ids))
	    call gargi (UT(ids))
	    if (nscan() < 11) 
	        call error (2, "First header card incomplete")
        }

	if (fscan (in) == EOF) {
	    call sfree (sp)
	    return (EOF)
	} else {
	    # Values to be read from second card of file
	    call gargi (ST(ids))
	    call gargd (RA(ids))
	    call gargd (DEC(ids))
	    call gargi (tape)
	    if (tape > 0)
	        RECORD_NUMBER(ids) = tape * 1000 + record
	    else
		RECORD_NUMBER(ids) = record
	    call gargi (DF_FLAG(ids))
	    call gargi (SM_FLAG(ids))
	    call gargi (QF_FLAG(ids))
	    call gargi (DC_FLAG(ids))
	    call gargi (QD_FLAG(ids))
	    call gargi (EX_FLAG(ids))
	    call gargi (BS_FLAG(ids))
	    if (nscan() < 11)
	        call error (3, "Second header card incomplete")
	}

	if (fscan (in) == EOF)  {
	    call sfree (sp)
	    return (EOF)
	} else {
	    # Values to be read from third card of file
	    call gargi (CA_FLAG(ids))
	    call gargi (CO_FLAG(ids))
	    call gargwrd (ALPHA_ID(ids), 3)
	    call gargi (OFLAG(ids))
	    call gargd (HA(ids))
	    call gargd (AIRMASS(ids))
	    if (nscan() < 6) {
	        call error (4, "Third header card incomplete")
	    }
	}

	if (fscan (in) == EOF) {
	    call sfree (sp)
	    return (EOF)
	} else {
	    call gargstr (Memc[temp], SZ_LINE)

	    # Count number of characters in id string by skipping trailing
	    # white space.  'END' are always the last 3 characters of the
	    # line of text containing the ID; they are also skipped.

	    for (ip = strlen(Memc[temp]) - 3; IS_WHITE (Memc[temp+ip-1]) && 
		ip > 0 ; ip = ip - 1)
		    ;
	    Memc[temp+ip] = EOS
	    call strcpy (Memc[temp], LABEL(ids), SZ_LINE)
	}

	# Since this is IDSOUT format data, initialize COEFF(ids) to integer 0.
	COEFF(ids) = 0

	# Determine companion record number if appropriate
	if (SMODE(ids) != 0) {
	    if (BEAM_NUMBER(ids) == 1)
		COMPANION_RECORD(ids) = RECORD_NUMBER(ids) - 1
	    else
		COMPANION_RECORD(IDS) = RECORD_NUMBER(IDS) + 1
	}

	call sfree (sp)
	return (OK)

end


# COPY_PIXELS -- copy pixel values from input to output file.  A blank line
# terminates each IDS record.

procedure copy_pixels (in, out)

int	in	# File descriptor of input text file
int	out	# File descriptor of output file

pointer	sp, line_buffer
bool	leading_blank
int	ip
int	getline()

begin
	# Allocate space on stack for line_buffer
	call smark (sp)
	call salloc (line_buffer, SZ_LINE, TY_CHAR)

	# Ignore blank lines until first non_blank line encountered
	leading_blank = true

	repeat {
	    if (getline (in, Memc[line_buffer]) == EOF) 
		call error (5, "Unexpected EOF when copying pixels")

	    # Find first non-whitespace character
	    for (ip = line_buffer; IS_WHITE (Memc[ip]); ip = ip + 1)
		;

	    if (Memc[ip] == '\n' || Memc[ip] == EOS) {
	        # Blank line
	        if (leading_blank)
	            ;
	        else
	            break
	    } else {
	        leading_blank = false
	        if (out != NULL)
	    	    call putline (out, Memc[line_buffer])
	    }
	} 

	call sfree (sp)
end


# IDSO_WRITE_IMAGE -- convert card image data values to reals and write
# 1-d IRAF image.  

procedure idso_write_image (in, data_type, print_pixels, out_fname, ids)

int	in			# File descriptor of input text file
int	data_type		# Data type of image pixels to be written
int	print_pixels		# Are pixel values to be listed (YES/NO)?
char	out_fname[SZ_FNAME]	# Name of output image
pointer	ids			# Pointer to program data structure

pointer	im, pixels, op
char	line[SZ_LINE], temp[MAX_WIDTH]
int	i, j, ip, iip, nchars
pointer	immap(), impl1r()
int	fscan(), nscan(), strlen(), ctowrd(), ctor()
errchk	immap, imunmap, fscan, impl1r, unpk_30, cy_store_keywords

begin
	im = immap (out_fname, NEW_IMAGE, LEN_USER_AREA)
	IM_NDIM(im) = 1
	IM_LEN(im, 1) = NPIX_IDS_RECORD
	call strcpy (LABEL(ids), IM_TITLE(im), SZ_IMTITLE)
	IM_PIXTYPE(im) = data_type
	pixels = impl1r(im)
	op = pixels

	# Position to first non_blank line.
	repeat {
	    if (fscan (in) == EOF) {
		call imunmap (im)
		return
	    } else 
		call gargstr (line, SZ_LINE)
	} until (strlen (line) > 1)
	call reset_scan()


	# Scan and convert pixel values to image pixels.  A blank line
	# terminates the IDS record.

	do i = 1, NLINES_PIXELS {
	    ip = 1
	    # Extract 8 pixel values from each line of text
	    do j = 1, NPIX_LINE {
		iip = 1
		call gargstr (line, SZ_LINE)
		nchars = ctowrd (line, ip, temp, MAX_WIDTH)
		nchars = ctor (temp, iip, Memr[op])
	        # call gargr (Memr[op])
	        op = op + 1
	    }

	    if (nscan () <= 1)
		# Premature blank line encountered
		break

	    # Get next line of pixels
	    if (fscan (in) == EOF)
		break
	}

	if (print_pixels == YES)
	    call list_values (Memr[pixels])
	
	# Write ids specific header words to iraf image header
	call cy_store_keywords (ids, im)

	call imunmap (im)
end
