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
int	nfiles, fd, i, file_nr, ptype
size_t	ncols, nrows
short	space[10], sncols, snrows, sptype
long	j, v1[IM_MAXDIM]
real	scale_fact, temp
bool	add_header
pointer	infile, im, pix, sp, inpix
pointer	p_val
long	l_val
size_t	sz_val

int	clplen(), clgfil(), open(), strlen()
long	imgnlr()
real	clgetr()
bool	clgetb()
pointer	immap(), clpopni()

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
	    p_val = LEN_USER_AREA
	    iferr (im = immap (ifile, READ_ONLY, p_val)) {
		call eprintf ("[%s] not found\n")
		call pargstr (ifile)
		go to 10
	    }

	    ncols = IM_LEN (im, 1)
	    nrows = IM_LEN (im, 2)
	    ptype = IM_PIXTYPE (im)

	    # Pack header characters
	    sz_val = strlen (IM_TITLE(im))
	    call strpak (IM_TITLE(im), header, sz_val)

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
		sz_val = SZ_SHORT/SZ_CHAR
		call write (fd, sncols, sz_val)
		call write (fd, snrows, sz_val)
		call write (fd, sptype, sz_val)
		sz_val = 10 * SZ_SHORT/SZ_CHAR
		call write (fd, space, sz_val)
		sz_val = 64 / SZB_CHAR
		call write (fd, header, sz_val)
	    }

	    call smark (sp)
	    call salloc (pix, ncols, TY_SHORT)

	    # Access pixels and write them out for each row
	    l_val = 1
	    sz_val = IM_MAXDIM
	    call amovkl (l_val, v1, sz_val)
	    while (imgnlr (im, inpix, v1) != EOF) {
		do j = 1, ncols {
		    temp = Memr[inpix+j-1] * scale_fact

		    if (temp > MAX_SHORT)
			temp = MAX_SHORT
		    else if (temp < -(MAX_SHORT))
			temp = -MAX_SHORT

		    Mems[pix+j-1] = temp
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
int	nfiles, fd, file_nr, ptype
size_t	ncols, nrows, nr_chars, nr_skip, nc_skip, c_1
int	nr_bits, ival
long	offset, i, j, krow
bool	flip, sign16
pointer	infile, im, pix, sp, temp, opix, sp1, hdr, src
short	s_val

int	clplen(), clgfil(), clgeti()
long	clgetl()
int	open()
long	read()
bool	clgetb()
pointer	clpopni(), immap(), impl2s(), impl2i()
include	<nullptr.inc>

begin
	c_1 = 1

	# Get file names
	infile = clpopni ("input")
	nfiles = clplen (infile)

	# Get image dimensions
	nrows = clgetl ("nrows")
	ncols = clgetl ("ncols")

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
	nr_skip = clgetl ("skip")

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
		ptype = TY_INT

	    # Create output file name and open it - append ".i"
	    call sprintf (out_image, SZ_FNAME, "%s.i")
		call pargstr (ifile)
	    file_nr = file_nr + 1
	    call printf ("%s --> %s\n")
		call pargstr (ifile)
		call pargstr (out_image)
	    call flush (STDOUT)

	    im = immap (out_image, NEW_IMAGE, NULLPTR)
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
	        iferr (nc_skip = read (fd, Mems[pix], nr_chars)) {
		    s_val = 0
		    call amovks (s_val, Mems[pix], nr_chars)
		} else {
		    if (nr_bits == 8) {
			call chrupk (Mems[pix], c_1, Mems[temp], c_1, ncols)
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
		    opix = impl2i (im, krow)

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
			    Memi[opix+j-1] = ival + offset
		        else
			    Memi[opix+j-1] = ival
		    }
		}
	    }

	    call sfree (sp)
	    call close (fd)
	    call imunmap (im)
10	    ;
	}
end
