include <imhdr.h>
include <error.h>
include	<mach.h>

# Binary file image transfer utilities --
# 	1. Convert from IRAF image to binary format
# 	1. Convert from binary formats to IRAF image

define	LEN_USER_AREA	720

# BINFIL -- Convert IRAF image file of shorts to a binary string
#           A short header of 90 bytes is prepended and has the
#           following elements;
#
#	       bytes   content
#		1-2    nrows
#		3-4    ncols
#		5-6    IRAF pixel type
#		7-26   space set to 0
#		27-90  header (ASCII)

procedure t_binfil()

char	ifile[SZ_FNAME], header[64], out_image[SZ_FNAME]
int	infile, nfiles, fd, i, file_nr, ncols, nrows, ptype
short	space[10], sncols, snrows, sptype
long	v1[IM_MAXDIM]
real	scale_fact, temp
bool	add_header
pointer	im, pix, sp, inpix

int	clpopni(), clplen(), clgfil(), open(), imgnlr(), strlen()
real	clgetr()
bool	clgetb()
pointer	immap()

begin
	# Get file names
	infile = clpopni ("input")
	nfiles = clplen (infile)

	# Get optional scaling factor
	scale_fact = clgetr ("scale_fact")
	if (scale_fact == 0.0)
	    scale_fact = 1.0

	# Should a header string be added?
	add_header = clgetb ("header")

	# Zero header spaces
	do i = 1, 10
	    space[i] = 0

	# Loop over all images
	while (clgfil (infile, ifile, SZ_FNAME) != EOF) {
	    iferr (im = immap (ifile, READ_ONLY, LEN_USER_AREA)) {
		call eprintf ("[%s] not found\n")
		call pargstr (ifile)
		go to 10
	    }

	    ncols = IM_LEN (im, 1)
	    nrows = IM_LEN (im, 2)
	    ptype = IM_PIXTYPE (im)

	    # Pack header characters
	    call strpak (IM_TITLE(im), header, strlen (IM_TITLE(im)))

	    # Create output file name and open it - append ".b"
	    call sprintf (out_image, SZ_FNAME, "%s.b")
		call pargstr (ifile)
	    call printf ("%s --> %s\n")
		call pargstr (ifile)
		call pargstr (out_image)
	    call flush (STDOUT)

	    file_nr = file_nr + 1

	    fd = open (out_image, NEW_FILE, BINARY_FILE)

	    # Write header parameters
	    if (add_header) {
		sncols = ncols
		snrows = nrows
		sptype = ptype
		call write (fd, sncols, SZ_SHORT/SZ_CHAR)
		call write (fd, snrows, SZ_SHORT/SZ_CHAR)
		call write (fd, sptype, SZ_SHORT/SZ_CHAR)
		call write (fd, space, 10 * SZ_SHORT/SZ_CHAR)
		call write (fd, header, 64 / SZB_CHAR)
	    }

	    call smark (sp)
	    call salloc (pix, ncols, TY_SHORT)

	    # Access pixels and write them out for each row
	    call amovkl (long(1), v1, IM_MAXDIM)
	    while (imgnlr (im, inpix, v1) != EOF) {
		do i = 1, ncols {
		    temp = Memr[inpix+i-1] * scale_fact

		    if (temp > MAX_SHORT)
			temp = MAX_SHORT
		    else if (temp < -(MAX_SHORT))
			temp = -MAX_SHORT

		    Mems[pix+i-1] = temp
		}

		call write (fd, Mems[pix], ncols * SZ_SHORT/SZ_CHAR)

	    }
	    call close (fd)
	    call sfree (sp)

	    call imunmap (im)
10	    ;
	}
end

# IRAFIL -- Convert 16 or 8-bit binary string to IRAF image file

procedure t_irafil()

char	ifile[SZ_FNAME], out_image[SZ_FNAME]
int	infile, nfiles, fd, i, j, file_nr, ncols, nrows, ptype, krow
int	nr_bits, nr_chars, nr_skip, nc_skip, ival
long	offset
bool	flip, sign16
pointer	im, pix, sp, temp, opix, sp1, hdr, src

int	clpopni(), clplen(), clgfil(), clgeti()
int	open(), read()
bool	clgetb()
pointer	immap(), impl2s(), impl2l()

begin
	# Get file names
	infile = clpopni ("input")
	nfiles = clplen (infile)

	# Get image dimensions
	nrows = clgeti ("nrows")
	ncols = clgeti ("ncols")

	# Is input string of data 8 or 16 bits?
	nr_bits = clgeti ("bits")
	if (nr_bits != 8 && nr_bits != 16)
	    call error (0, "Must be 8 or 16 bits")

	# Is bit 16 to be used as a sign bit?
	if (nr_bits == 16) {
	    sign16 = clgetb ("signed")
	    offset = 65536
	} else {
	    sign16 = true
	    offset = 256
	}

	# Should image be top-bottom flipped?
	# For some input images (e.g. Compaq 286 display) this is
	# needed to make SNAPS look correct.
	flip = clgetb ("tb_flip")

	# Header info can be skipped if number of bytes is given
	nr_skip = clgeti ("skip")

	# Loop over all images
	while (clgfil (infile, ifile, SZ_FNAME) != EOF) {
	    iferr (fd = open (ifile, READ_ONLY, BINARY_FILE)) {
		call eprintf ("cannot open %s\n")
		call pargstr (ifile)
		go to 10
	    }

	    if (sign16)
		ptype = TY_SHORT
	    else
		ptype = TY_LONG

	    # Create output file name and open it - append ".i"
	    call sprintf (out_image, SZ_FNAME, "%s.i")
		call pargstr (ifile)
	    file_nr = file_nr + 1
	    call printf ("%s --> %s\n")
		call pargstr (ifile)
		call pargstr (out_image)
	    call flush (STDOUT)

	    im = immap (out_image, NEW_IMAGE, 0)
	    IM_NDIM (im) = 2
	    IM_LEN (im, 1) = ncols
	    IM_LEN (im, 2) = nrows
	    IM_PIXTYPE (im) = ptype

	    call smark (sp)
	    call salloc (pix, ncols, TY_SHORT)
	    call salloc (temp, ncols, TY_SHORT)

	    # Skip over header pixels if any
	    nc_skip = nr_skip / 2
	    if (nr_skip > 0) {
		call smark (sp1)
		call salloc (hdr, nc_skip, TY_SHORT)
		if (read (fd, Mems[hdr], nc_skip) != EOF)
		    ;
		call sfree (sp1)
	    }

	    # Access pixels and write them out for each row
	    nr_chars = ncols * nr_bits / 8 / 2
	    do i = 1, nrows {
	        iferr (nc_skip = read (fd, Mems[pix], nr_chars))
		    call amovks (0, Mems[pix], nr_chars)
		else {
		    if (nr_bits == 8) {
			call chrupk (Mems[pix], 1, Mems[temp], 1, ncols)
			src = temp
		    } else
			src = pix
		}

		# Provide top-bottom flip for special image formats
		if (flip)
		    krow = nrows-i+1
		else
		    krow = i

		# Select proper pointer type
		if (sign16)
		    opix = impl2s (im, krow)
		else
		    opix = impl2l (im, krow)

		# Transfer all pixels, correcting for signed/unsigned data
		do j = 1, ncols {
		    ival = Mems[src+j-1]
		    if (sign16) {
			if (nr_bits == 8 && ival < 0)
			    Mems[opix+j-1] = ival + offset
		        else
			    Mems[opix+j-1] = ival
		    } else {
		        if (ival < 0)
			    Meml[opix+j-1] = ival + offset
		        else
			    Meml[opix+j-1] = ival
		    }
		}
	    }

	    call sfree (sp)
	    call close (fd)
	    call imunmap (im)
10	    ;
	}
end
