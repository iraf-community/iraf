# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fset.h>
include "rfits.h"

define	MAX_RANGES	100		# the maxium number of ranges
define	NTYPES		7		# the number of image data types

# RFITS -- Read FITS format data.  Further documentation given in rfits.hlp

procedure t_rfits()

char	infile[SZ_FNAME]		# fits file
char	file_list[SZ_LINE]		# list of tape files
char	outfile[SZ_FNAME]		# IRAF file
char	in_fname[SZ_FNAME]		# input file name
char	out_fname[SZ_FNAME]		# output file name

int	len_inlist, len_outlist
int	range[MAX_RANGES*3+1], file_number, offset, stat
pointer	inlist, outlist

bool	clgetb()
char	clgetc()
int	rft_get_image_type(), clgeti(), mtfile(), strlen(), btoi(), fntlenb()
int	rft_read_fitz(), decode_ranges(), get_next_number(), fntgfnb(), fstati()
int	mtneedfileno(), fntrfnb()
pointer	fntopnb()
real	clgetr(), rft_fe()

include	"rfits.com"

begin
	# Set up the standard output to flush on a newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get RFITS parameters.
	call clgstr ("fits_file", infile, SZ_FNAME)
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	len_record = FITS_RECORD
	old_name = btoi (clgetb ("oldirafname"))
	make_image = btoi (clgetb ("make_image"))

	# Open the input file list.
	if (mtfile (infile) == YES) {
	    inlist = NULL
	    if (mtneedfileno (infile) == YES)
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
	        call strcpy ("1", file_list, SZ_LINE)
	} else {
	    inlist = fntopnb (infile, NO)
	    len_inlist = fntlenb (inlist)
	    if (len_inlist > 0) {
	        call sprintf  (file_list, SZ_LINE, "1-%d")
		    call pargi (len_inlist)
	    } else
	        call sprintf  (file_list, SZ_LINE, "0")
	}

	# Decode the ranges string.
	if (decode_ranges (file_list, range, MAX_RANGES, len_inlist) == ERR)
	    call error (1, "T_RFITS: Illegal file number list")

	# Open the output file list.
	if (make_image == YES) {
	    call clgstr ("iraf_file", outfile, SZ_FNAME)
	    if (outfile[1] == EOS) {
		if (old_name == YES)
		    call mktemp ("tmp", outfile, SZ_FNAME) 
		else
		    call error (0, "T_RFITS: Undefined output file name")
	    }
	    outlist = fntopnb (outfile, NO)
	    len_outlist = fntlenb (outlist)
	    data_type = rft_get_image_type (clgetc ("datatype"))
	    scale = btoi (clgetb ("scale"))
	    blank = clgetr ("blank")
	    offset = clgeti ("offset")
	} else {
	    outfile[1] = EOS
	    outlist = NULL
	    len_outlist = 1
	}
	if ((len_outlist > 1) && (len_outlist != len_inlist))
	    call error (0,
	        "T_RFITS: Output and input lists have different lengths")

	# Get the scan size parameter.
	fe = rft_fe (infile)

	# Read successive FITS files, convert and write into a numbered
	# succession of output IRAF files.

	file_number = 0
	while (get_next_number (range, file_number) != EOF) {

	    # Get the input file name.
	    if (inlist != NULL) {
		if (fntgfnb (inlist, in_fname, SZ_FNAME) == EOF)
		    call error (0, "T_RFITS: Error reading input file name")
	    } else {
		if (mtneedfileno (infile) == YES)
		    call mtfname (infile, file_number, in_fname, SZ_FNAME)
		else
	            call strcpy (infile, in_fname, SZ_FNAME)
	    }

	    # Get the output file name.
	    if (outlist == NULL) {
		out_fname[1] = EOS
	    } else if (len_inlist > len_outlist) {
		if (fntrfnb (outlist, 1, out_fname, SZ_FNAME) == EOF)
	            call strcpy (outfile, out_fname, SZ_FNAME)
	        if (len_inlist > 1) {
		    call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME,
		        "%04d")
		        call pargi (file_number + offset)
	        }
	    } else if (fntgfnb (outlist, out_fname, SZ_FNAME) == EOF)
		call error (0, "T_RFITS: Error reading output file name")

	    # Convert FITS file to the output IRAF file. If EOT is reached
	    # then exit. If an error is detected then print a warning and
	    # continue with the next file.

	    iferr (stat = rft_read_fitz (in_fname, out_fname))
		call erract (EA_FATAL)
	    if (stat == EOF)
		break
	}

	if (inlist != NULL)
	    call fntclsb (inlist) 
	if (outlist != NULL)
	    call fntclsb (outlist) 
end


# RFT_GET_IMAGE_TYPE -- Convert a character to and IRAF image type.

int procedure rft_get_image_type (c)

char	c

int	type_codes[NTYPES], i
string	types "usilrdx"	
int	stridx()
data	type_codes /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL,
		    TY_DOUBLE, TY_COMPLEX/
begin
	i = stridx (c, types)
	if (i == 0)
	    return (ERR)
	else
	    return (type_codes[stridx(c,types)])
end


# RFT_FE -- Fetch the maximum file size in MB for tape scanning mode.

real procedure rft_fe (file)

char	file[ARB]		# the input file name

pointer	gty
real	fe
int	mtfile(), gtygeti()
pointer	mtcap()
errchk	gtygeti()

begin
	if (mtfile (file) == NO)
	    return (0.0)
	iferr (gty = mtcap (file))
	    return (0.0)
	iferr (fe = gtygeti (gty, "fe"))
	    fe = 0.0
	call gtyclose (gty)
	return (fe)
end
