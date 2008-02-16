# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"imtext.h"

define	NBITS_ASCII	8
define	NDEC_PLACES	7


# WTI_WRITE_HEADER -- write information from IRAF image header to text file in
# FITS "keyword = value / comment" format.  One keyword is written per line
# of text.

procedure wti_write_header (im, image, tx, out_format)

pointer	im			# Pointer to image file
char	image[ARB]		# Image filename
int	tx			# File descriptor of text file
char	out_format[ARB] 	# Output format for pixel conversion

int	i, nlines, user, op, max_lenuser
pointer	sp, root, line, comment
bool	streq()
int	strlen(), sizeof(), getline(), stropen(), gstrcpy(), stridx()

errchk	addcard_b, addcard_i, addcard_r, addcard_st
errchk	wti_iraf_type, streq, strupr, stropen, strclose, getline

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE,  TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	call addcard_i (tx, "BITPIX", NBITS_ASCII, "8-bit ASCII characters")
	call addcard_i (tx, "NAXIS", IM_NDIM(im), "Number of Image Dimensions")

	nlines = NFITS_LINES

	# Construct and output an NAXISn card for each axis
	do i = 1, IM_NDIM(im) {
	    op = gstrcpy ("NAXIS", Memc[root], LEN_KEYWORD)
	    call sprintf (Memc[root+op], LEN_KEYWORD-op, "%d")
		call pargi (i)
	    call addcard_i (tx, Memc[root], IM_LEN(im,i), "Length of axis")
	    nlines = nlines + 1
	}
	
	call addcard_st (tx,  "ORIGIN", "NOAO-IRAF: WTEXTIMAGE", "", 
	    strlen("NOAO-IRAF: WTEXTIMAGE"))

	# Add the image MIN and MAX header cards
	call strcpy ("Max image pixel", Memc[comment], SZ_LINE)
	if (IM_MTIME(im) > IM_LIMTIME(im))
	    call strcat (" (out of date)", Memc[comment], SZ_LINE)
	call addcard_r (tx, "IRAF-MAX", IM_MAX(im), Memc[comment],
	    NDEC_PLACES)

	call strcpy ("Min image pixel", Memc[comment], SZ_LINE)
	if (IM_MTIME(im) > IM_LIMTIME(im))
	    call strcat (" (out of date)", Memc[comment], SZ_LINE)
	call addcard_r (tx, "IRAF-MIN", IM_MIN(im), Memc[comment],
	    NDEC_PLACES)

	# The number of bits per pixel is calculated and output
	call addcard_i (tx, "IRAF-B/P", sizeof (IM_PIXTYPE(im)) *
	    SZB_CHAR * NBITS_BYTE, "Image bits per pixel")

	call wti_iraf_type (IM_PIXTYPE(im), Memc[root])
	call addcard_st (tx, "IRAFTYPE", Memc[root], "Image datatype", 
	    strlen(Memc[root]))

	call strupr (IM_TITLE(im))
	call addcard_st (tx, "OBJECT" , IM_TITLE(im), "",
	    strlen (IM_TITLE(im)))

	call strupr (image)
	call addcard_st (tx, "FILENAME", image, "IRAF filename", 
	    strlen (image))
	nlines = nlines + 1

	call strcpy ("Text line format", Memc[comment], SZ_LINE)
	if (streq (out_format, "*"))
	    call strcat (" (* = list directed)", Memc[comment], SZ_LINE)
	call addcard_st (tx, "FORMAT", out_format, Memc[comment],
	    LEN_STRING)
	nlines = nlines + 1

	# Write any information stored in image user area
	if ((IM_HDRLEN(im) - LEN_IMHDR) > 0) {
	    max_lenuser = (LEN_IMDES + IM_LENHDRMEM(im) - IMU) * SZ_STRUCT - 1
	    user = stropen (Memc[IM_USERAREA(im)], max_lenuser, READ_ONLY)

	    while (getline (user, Memc[line]) != EOF) {
	        call putline (tx, Memc[line])
	        nlines = nlines + 1
	    }

	    # Make sure last line written out included a newline.  It won't if 
	    # the user area was truncated when it was read.
	    if (stridx ("\n", Memc[line]) == 0)
	        call putline (tx, "\n")

	    call close (user)
	}

	# Final header line is END  (FITS keywords are 8 characters long)
	call fprintf (tx, "END%77w\n")
	nlines = nlines + 1

	# Pad output file with blank lines until header block occupies
	# a multiple of 36 lines.

	if (nlines != NCARDS_FITS_BLK) {
	    do i = 1, NCARDS_FITS_BLK - mod(nlines, NCARDS_FITS_BLK)
	        call fprintf (tx, "%80w\n")
	}

	call sfree (sp)
end


# WTI_IRAF_TYPE -- Procedure to set the iraf datatype keyword. Permitted strings
# are INTEGER, FLOATING or COMPLEX.

procedure wti_iraf_type (datatype, type_str)

int	datatype	# the IRAF data type
char	type_str[ARB]	# the output IRAF type string

begin
	switch (datatype) {
	case TY_SHORT:
	    call strcpy ("SHORT INTEGER", type_str, LEN_STRING) 
	case TY_USHORT:
	    call strcpy ("UNSIGNED SHORT INT", type_str, LEN_STRING) 
	case TY_INT:
	    call strcpy ("INTEGER", type_str, LEN_STRING) 
	case TY_LONG:
	    call strcpy ("LONG INTEGER", type_str, LEN_STRING) 
	case TY_REAL:
	    call strcpy ("REAL FLOATING", type_str, LEN_STRING)
	case TY_DOUBLE:
	    call strcpy ("DOUBLE FLOATING", type_str, LEN_STRING)
	case TY_COMPLEX:
	    call strcpy ("COMPLEX", type_str, LEN_STRING)
	default:
	    call error (4, "IRAF_TYPE: Unknown IRAF image type.")
	}
end
