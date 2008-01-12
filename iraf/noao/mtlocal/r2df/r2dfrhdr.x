include <imhdr.h>
include <mach.h>
include	"r2df.h"

# R2DFRHDR -- Read a 2D-FRUTTI header.
# If EOF is reached the routine returns EOF, otherwise it returns the
# number of data records in the 2D-FRUTTI image file.

int procedure r2dfrhdr (cam_fd, im)

pointer	cam_fd		# pointer to camera file
pointer	im		# pointer to the IRAF image

int	i, sz_rec
char	text[LEN_CAM_TEXT], header[LEN_HEADER * SZ_SHORT]
short	parameters[LEN_CAM_PARAMETERS]

int	read(), r2dfrndup()
errchk	r2dfdcd_hdr, read
include "r2df.com"

begin
	# Read in header record
	sz_rec = r2dfrndup (SZB_CHAR * LEN_HEADER, SZB_CHAR) / SZB_CHAR

	i = read (cam_fd, header, sz_rec)

	if (i == EOF)
	    return (EOF)
	else if (i != sz_rec)
	    call error (1, "Error reading 2D-FRUTTI header")

	# If the least significant byte is first byteswap the 2d-frutti
	# parameters otherwise byteswap the header text.

	call bytmov (header, FST_HDRBYTE, text, 1, LEN_CAM_TEXT)
	if (lsbf == NO)
	    call bswap2 (text, 1, text, 1, LEN_CAM_TEXT)
	call bytmov (header, 1,  parameters, 1, LEN_CAM_PARAMETERS * 2)
	if (lsbf != BYTE_SWAP2)
	    call bswap2 (parameters, 1, parameters, 1, 2 * LEN_CAM_PARAMETERS)

	# Decode the text string
	call chrupk (text, 1, text, 1, LEN_CAM_TEXT)
	text[LEN_CAM_TEXT+1] = EOS

	# Put the 2D-FRUTTI parameters in the IRAF image header
	call r2dfdcd_hdr (im, parameters, text)
	call r2dfprnt_hdr (parameters, text)

	return (OK)
end


# R2DFDCD_HDR -- Decode a 2D-FRUTTI header record.

procedure r2dfdcd_hdr (im, parameters, text)

pointer	im
short	parameters[ARB]
char	text[ARB]

include	"r2df.com"

begin
	# Determine the length of the record in short integers.
	len_record = REC_LEN(parameters)

	# Set IRAF image parameters.  Send extra keywords to the user area.
	if (make_image == YES) {
	    NAXIS(im) = 2
	    PARAM5(im) = NAXIS1(parameters)
	    PARAM6(im) = NAXIS2(parameters)
	    call strcpy (text, TITLE(im), LEN_TITLE)
	    call r2dfstore_token (parameters, im)
	}
end


# R2DFPRNT_HDR -- Print the 2D-FRUTTI header.

procedure r2dfprnt_hdr (parameters, text)

short	parameters[ARB]
char	text[ARB]

include	"r2df.com"

begin
	if (long_header == YES)
	    call r2dflng_hdr1 (parameters, text)

	if (short_header == YES && long_header == NO) {
	    call printf ("ID: %.30s  ")
	        call pargstr (text)
	    call printf ("Size = %d x %d\n")
		call pargs (NAXIS1(parameters))
		call pargs (NAXIS2(parameters))
	}
end


# R2DFLNG_HDR1 -- Print the full 2D-FRUTTI header.

procedure r2dflng_hdr1 (parameters, text)

short	parameters[ARB]
char	text[ARB]

begin
	call printf ("ID: %.30s   CCDPICNO: %d\n")
	    call pargstr (text)
	    call pargs (CCD_PICNO(parameters))
	call printf ("NAXIS1= %3d                 ")
	    call pargs (NAXIS1(parameters))
	call printf ("NAXIS2= %3d\n")
	    call pargs (NAXIS2(parameters))
	call printf ("INTEGRATION= %5d          ")
	    call pargs (ITIME(parameters))
	call printf ("OPEN TIME= %5d\n")
	    call pargs (OTIME(parameters))
	call r2dflng_hdr2 (parameters, text)
end


# R2DFLNG_HDR2 -- This routine is used because of number of strings
# limitation in preprocessor.

procedure r2dflng_hdr2  (parameters, text)

short	parameters[ARB]
char	text[ARB]

int	nrecs

begin
	nrecs = (int (NAXIS2(parameters)) * int (NAXIS1(parameters))) / 4096 + 1
	call printf ("RECORDS= %3d                ")
	    call pargi (nrecs)
	call printf ("RECORD LENGTH= %5d\n")
	    call pargi (REC_LEN(parameters))
end


# R2DFRNDUP -- Procedure to round an integer to the next highest number
# divisible by base.

int procedure r2dfrndup (number, base)

int	number, base
int	value

begin
	if (mod(number, base) == 0)
	    return (number)
	else {
	    value = (number/base + 1) * base
	    return (value)
	}
end
