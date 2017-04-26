# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fset.h>
include "rfits.h"

define	NTYPES		7		# the number of image data types

# RFITS -- Read FITS format data.  Further documentation given in rfits.hlp

procedure t_rfits()

int	inlist, outlist, len_inlist, len_outlist
int	file_number, offset, stat, first_file, last_file
pointer	sp, infile, file_list, outfile, ext_list, in_fname, out_fname
pointer	pl, axes

bool	clgetb(), pl_linenotempty()
#char	clgetc()
int	rft_get_image_type(), clgeti(), mtfile(), strlen(), btoi(), fntlenb()
int	rft_read_fitz(), fntgfnb(), fstati(), mtneedfileno(), fntrfnb()
pointer	fntopnb(), rft_flist()
real	clgetr(), rft_fe()

include	"rfits.com"

begin
	# Set up the standard output to flush on a newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (file_list, SZ_LINE, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (ext_list, SZ_LINE, TY_CHAR)
	call salloc (in_fname, SZ_FNAME, TY_CHAR)
	call salloc (out_fname, SZ_FNAME, TY_CHAR)
	call salloc (axes, 2, TY_INT)

	# Get RFITS parameters.
	call clgstr ("fits_file", Memc[infile], SZ_FNAME)
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	len_record = FITS_RECORD
	old_name = btoi (clgetb ("oldirafname"))
	make_image = btoi (clgetb ("make_image"))

	# Open the input file list.
	call clgstr ("file_list", Memc[ext_list], SZ_LINE)
	if (mtfile (Memc[infile]) == YES) {
	    inlist = NULL
	    if (mtneedfileno (Memc[infile]) == YES) {
	        call strcpy (Memc[ext_list], Memc[file_list], SZ_LINE)
	    } else {
		call sprintf (Memc[file_list], SZ_LINE, "1[%s]")
		    call pargstr (Memc[ext_list])
	    }
	} else {
	    inlist = fntopnb (Memc[infile], NO)
	    len_inlist = fntlenb (inlist)
	    if (len_inlist > 0) {
		if (Memc[ext_list] == EOS) {
	            call sprintf  (Memc[file_list], SZ_LINE, "1-%d[0]")
		        call pargi (len_inlist)
		        #call pargstr (Memc[ext_list])
		} else {
	            call sprintf  (Memc[file_list], SZ_LINE, "1-%d[%s]")
		        call pargi (len_inlist)
		        call pargstr (Memc[ext_list])
		}
	    } else {
	        call sprintf  (Memc[file_list], SZ_LINE, "0[%s]")
		    call pargstr (Memc[ext_list])
	    }
	}

	# Decode the ranges string.
	pl = rft_flist (Memc[file_list], first_file, last_file, len_inlist) 
	if (pl == NULL || len_inlist <= 0)
	    call error (1, "T_RFITS: Illegal file/extensions number list")

	# Open the output file list.
	if (make_image == YES) {
	    call clgstr ("iraf_file", Memc[outfile], SZ_FNAME)
	    if (Memc[outfile] == EOS) {
		if (old_name == YES)
		    call mktemp ("tmp$", Memc[outfile], SZ_FNAME) 
		else
		    call error (0, "T_RFITS: Undefined output file name")
	    }
	    outlist = fntopnb (Memc[outfile], NO)
	    len_outlist = fntlenb (outlist)
	    offset = clgeti ("offset")
	} else {
	    Memc[outfile] = EOS
	    outlist = NULL
	    len_outlist = 1
	}
	if ((len_outlist > 1) && (len_outlist != len_inlist))
	    call error (0,
	        "T_RFITS: Output and input lists have different lengths")

	# Get the remaining parameters. Use the string in_fname as a
	# temporary variable.
	#data_type = rft_get_image_type (clgetc ("datatype"))
	call clgstr ("datatype", Memc[in_fname], SZ_FNAME)
	data_type = rft_get_image_type (Memc[in_fname])
	scale = btoi (clgetb ("scale"))
	blank = clgetr ("blank")

	# Get the scan size parameter.
	fe = rft_fe (Memc[infile])

	# Read successive FITS files, convert and write into a numbered
	# succession of output IRAF files.

	do file_number = first_file, last_file {

	    # Get the next file number.
	    Memi[axes] = 1
	    Memi[axes+1] = file_number
	    if (! pl_linenotempty (pl, Memi[axes]))
		next

	    # Get the input file name.
	    if (inlist != NULL) {
		if (fntgfnb (inlist, Memc[in_fname], SZ_FNAME) == EOF)
		    call error (0, "T_RFITS: Error reading input file name")
	    } else {
		if (mtneedfileno (Memc[infile]) == YES)
		    call mtfname (Memc[infile], file_number, Memc[in_fname],
		        SZ_FNAME)
		else
	            call strcpy (Memc[infile], Memc[in_fname], SZ_FNAME)
	    }

	    # Get the output file name.
	    if (outlist == NULL) {
		Memc[out_fname] = EOS
	    } else if (len_inlist > len_outlist) {
		if (fntrfnb (outlist, 1, Memc[out_fname], SZ_FNAME) == EOF)
	            call strcpy (Memc[outfile], Memc[out_fname], SZ_FNAME)
	        if (len_inlist > 1) {
		    call sprintf (Memc[out_fname+strlen(Memc[out_fname])],
		        SZ_FNAME, "%04d")
		        call pargi (file_number + offset)
	        }
	    } else if (fntgfnb (outlist, Memc[out_fname], SZ_FNAME) == EOF)
		call error (0, "T_RFITS: Error reading output file name")

	    # Convert FITS file to the output IRAF file. If EOT is reached
	    # then exit. If an error is detected then print a warning and
	    # continue with the next file.

	    iferr (stat = rft_read_fitz (Memc[in_fname], Memc[out_fname],
	        pl, file_number))
		call erract (EA_FATAL)
	    if (stat == EOF)
		break
	}

	if (inlist != NULL)
	    call fntclsb (inlist) 
	if (outlist != NULL)
	    call fntclsb (outlist) 
	if (pl != NULL)
	    call pl_close (pl)

	call sfree (sp)
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
