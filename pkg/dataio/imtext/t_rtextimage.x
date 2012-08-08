# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>
include	"imtext.h"

# T_RTEXTIMAGE -- Read text files into IRAF images.  Information
# about the dimensionality of the image (the number of dimensions and the
# length of each dimension) must either be read from a FITS header or supplied
# by the user.

procedure t_rtextimage ()

char	output[SZ_FNAME], text_file[SZ_FNAME], temp[SZ_FNAME]
char	out_fname[SZ_FNAME]
pointer	im
int	header, pixels, nskip, nfiles, ntext, format, data_type, tf, i, input
int	fd_dim, junk, ndim, ip

bool	clgetb()
#char	clgetc()
pointer	immap()
int	btoi(), clgeti(), clpopni(), clplen(), clgfil(), get_data_type()
int	open(), rt_skip_lines(), clpopnu(), ctoi()

begin
	# Determine the input and output file names
	input = clpopni ("input")
	call clgstr ("output", output, SZ_FNAME)

	# Get hidden parameters from cl. 
	# data_type = get_data_type (clgetc ("otype"))
	call clgstr ("otype", out_fname, SZ_FNAME)
	data_type = get_data_type (out_fname[1])
	header = btoi (clgetb ("header"))
	pixels = btoi (clgetb ("pixels"))
	if (header == NO)
	    nskip = clgeti ("nskip")

	# Loop over the input files, generating an output name and processing. 
	nfiles = clplen (input)
	do ntext = 1, nfiles {
	    if (clgfil (input, text_file, SZ_FNAME) == EOF)
		return
	    tf = open (text_file, READ_ONLY, TEXT_FILE)
	    if (nfiles > 1) {
		call sprintf (out_fname, SZ_FNAME, "%s.%03d")
		    call pargstr (output)
		    call pargi (ntext)
	    } else
		call strcpy (output, out_fname, SZ_FNAME)

	    im = immap (out_fname, NEW_IMAGE, 0)

	    # Initialize those values that could be read from the header.
	    format = UNSET
	    IM_NDIM(im) = UNSET
	    IM_PIXTYPE(im) = UNSET

	    if (header == YES) {
		iferr (call rt_rheader (tf, im, format))
		    call erract (EA_FATAL)
	    } else if (nskip > 0) {
		if (rt_skip_lines (tf, nskip) == EOF)
		    call error (1, "Unexpected EOF when skipping lines")
	    }

	    # Get data_type of output image.  If supplied by user, use parameter
	    # value over anything read from FITS header. 

	    if (IM_PIXTYPE(im) == UNSET) {
		# Not read from header, use parameter value if supplied.  
		# Otherwise, wait until pixels are read to set pixel type.
		if (data_type == ERR)
		    IM_PIXTYPE(im) = UNSET
		else
		    IM_PIXTYPE(im) = data_type
	    } else if (data_type != ERR)
		# Available in header, but user has specified value to be used
		IM_PIXTYPE(im) = data_type

	    # If image dimension information wasn't read from header, the user
	    # must supply it.

	    if (IM_NDIM(im) == UNSET) {
		fd_dim = clpopnu ("dim")
		ndim = clplen (fd_dim)
		do i = 1, ndim {
		    junk = clgfil (fd_dim, temp, SZ_FNAME)
		    ip = 1
		    junk = ctoi (temp, ip, IM_LEN (im, i))
		}
		IM_NDIM(im) = ndim
	        call clpcls (fd_dim)
	    }

	    # Convert text pixels to image pixels, posting only a warning
	    # message if an error occurs.  Processing continues to the next
	    # file in the input list.

	    iferr (call rt_convert_pixels (tf, im, format, pixels))
		call erract (EA_WARN)

	    call imunmap (im)
	    call close (tf)
	}

	call clpcls (input)
end
