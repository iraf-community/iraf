# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fset.h>

define	MAX_RANGES	100
define	NTYPES	7

# RFITS -- Read FITS format data.  Further documentation given in rfits.hlp

procedure t_rfits()

char	infile[SZ_FNAME]		# fits file
char	outfile[SZ_FNAME]		# IRAF file
char	in_fname[SZ_FNAME]		# input file name
char	out_fname[SZ_FNAME]		# output file name
char	file_list[SZ_LINE]		# list of tape files

pointer	list
int	lenlist, junk
int	range[MAX_RANGES*3+1], nfiles, file_number, offset, stat, fits_record

bool	clgetb()
char	clgetc()
int	rft_get_image_type(), clgeti(), mtfile(), strlen(), btoi()
int	rft_read_fitz(), decode_ranges(), get_next_number(), fntgfnb()
int	fntlenb(), fstati()
pointer	fntopnb()
real	clgetr()
data	fits_record/2880/
include	"rfits.com"

begin
	# Set up the standard output to flush on a newline
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get RFITS parameters.
	call clgstr ("fits_file", infile, SZ_FNAME)
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	len_record = fits_record

	make_image = btoi (clgetb ("make_image"))
	if (make_image == YES) {
	    call clgstr ("iraf_file", outfile, SZ_FNAME)
	    data_type = rft_get_image_type (clgetc ("datatype"))
	    scale = btoi (clgetb ("scale"))
	    blank = clgetr ("blank")
	    offset = clgeti ("offset")
	} else
	    outfile[1] = EOS
	old_name = btoi (clgetb ("oldirafname"))

	# Compute the number of files to be converted
	if (mtfile (infile) == YES)  {
	    list = NULL
	    if (infile[strlen(infile)] != ']')
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
	        call strcpy ("1", file_list, SZ_LINE)
	} else {
	    list = fntopnb (infile, YES)
	    lenlist = fntlenb (list)
	    if (lenlist > 0) {
	        call sprintf  (file_list, SZ_LINE, "1-%d")
		    call pargi (lenlist)
	    } else
	        call sprintf  (file_list, SZ_LINE, "0")
	}

	# Decode the ranges
	if (decode_ranges (file_list, range, MAX_RANGES, nfiles) == ERR)
	    call error (1, "T_RFITS: Illegal file number list")

	# Read successive FITS files, convert and write into a numbered
	# succession of output IRAF files.

	file_number = 0
	while (get_next_number (range, file_number) != EOF) {

	    # Get input file name
	    if (list != NULL)
		junk = fntgfnb (list, in_fname, SZ_FNAME)
	    else {
	        call strcpy (infile, in_fname, SZ_FNAME)
	        if (infile[strlen(infile)] != ']') {
		    call sprintf (in_fname[strlen(in_fname)+1], SZ_FNAME,
		        "[%d]")
		        call pargi (file_number)
		}
	    }

	    # Get output file name
	    call strcpy (outfile, out_fname, SZ_FNAME)
	    if (nfiles > 1) {
		call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME, "%03d")
		    call pargi (file_number + offset)
	    }

	    # Convert FITS file to the output IRAF file.
	    # If EOT is reached then exit.
	    # If an error is detected then print a warning and continue with
	    # the next file.

	    iferr (stat = rft_read_fitz (in_fname, out_fname))
		call erract (EA_FATAL)
	    if (stat == EOF)
		break
	}

	if (list != NULL)
	    call fntclsb (list) 
end


# RFT_GET_IMAGE_TYPE -- Convert a character to and IRAF image type.

int procedure rft_get_image_type (c)

char	c
int	type_codes[NTYPES], i
int	stridx()
string	types "usilrdx"			# supported image data types
data	type_codes /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL,
		    TY_DOUBLE, TY_COMPLEX/
begin
	i = stridx (c, types)
	if (i == 0)
	    return (ERR)
	else
	    return (type_codes[stridx(c,types)])
end
