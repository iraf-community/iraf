# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMSUM -- Sum or average images with optional high and low pixel rejection.

procedure t_imsum ()

int	list			# Input image list
pointer	image			# Output image
pointer	hparams			# Header parameter list
pointer	option  		# Output option
int	pixtype			# Output pixel datatype
int	calctype		# Internal calculation type
real	low_reject		# Number or frac of low pix to reject
real	high_reject		# Number or frac of high pix to reject

int	i, nimages, nlow, nhigh
pointer	sp, str, im_in, im_out

bool	clgetb(), streq()
real	clgetr()
int	imtopenp(), imtlen(), imtgetim(), clgwrd()
pointer	immap()

errchk	imsum_set, immap, imunmap

begin
	# Get the input image list.  Check that there is at least 1 image.
	list = imtopenp ("input")
	nimages = imtlen (list)
	if (nimages < 1) {
	    call imtclose (list)
	    call error (0, "No input images in list")
	}

	# Allocate strings and get the parameters.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (hparams, SZ_LINE, TY_CHAR)
	call salloc (option, SZ_LINE, TY_CHAR)

	i = clgwrd ("option", Memc[option], SZ_LINE, "|sum|average|median|")
	if (streq (Memc[option], "median")) {
	    nlow = nimages / 2
	    nhigh = nimages - nlow - 1
	} else {
	    # If the rejection value is less than 1 then it is a fraction of the
	    # input images otherwise it is the number of pixels to be rejected.
	    low_reject = clgetr ("low_reject")
	    high_reject = clgetr ("high_reject")

	    if (low_reject < 1.)
	        nlow = low_reject * nimages
	    else
	        nlow = low_reject

	    if (high_reject < 1.)
	        nhigh = high_reject * nimages
	    else
	        nhigh = high_reject

	    if (nlow + nhigh >= nimages) {
		call sfree (sp)
		call imtclose (list)
	        call error (0, "Number of pixels rejected >= number of images")
	    }
	}
	call clgstr ("hparams", Memc[hparams], SZ_LINE)

	# Map the output image and set the title and pixel type.
	# Check all images have the same number and length of dimensions.

	call imsum_set (list, pixtype, calctype)

	i = imtgetim (list, Memc[image], SZ_FNAME)
	im_in = immap (Memc[image], READ_ONLY, 0)
	call clgstr ("output", Memc[image], SZ_FNAME)
	im_out = immap (Memc[image], NEW_COPY, im_in)
	call new_title ("title", im_out)
	IM_PIXTYPE (im_out) = pixtype

	call imtrew (list)

	# Print verbose info.
	if (clgetb ("verbose")) {
	    call salloc (str, SZ_LINE, TY_CHAR)
	    call printf ("IMSUM:\n")
	    call printf ("  Input images:\n")
	    while (imtgetim (list, Memc[str], SZ_LINE) != EOF) {
		call printf ("    %s\n")
		    call pargstr (Memc[str])
	    }
	    call imtrew (list)
	    call printf ("  Output image: %s\n")
		call pargstr (Memc[image])
	    call printf ("  Header parameters: %s\n")
		call pargstr (Memc[hparams])
	    call printf ("  Output pixel datatype: %s\n")
		call dtstring (pixtype, Memc[str], SZ_FNAME)
		call pargstr (Memc[str])
	    call printf ("  Calculation type: %s\n")
		call dtstring (calctype, Memc[str], SZ_FNAME)
		call pargstr (Memc[str])
	    call printf ("  Option: %s\n")
		call pargstr (Memc[option])
	    call printf ("  Low rejection: %d\n  High rejection: %d\n")
		call pargi (nlow)
		call pargi (nhigh)
	    call flush (STDOUT)
	}

	# Do the image average.  Switch on the calculation type.
	switch (calctype) {
	case TY_SHORT:
	    call imsums (list, Memc[image], im_out, nlow, nhigh, Memc[option])
	case TY_INT:
	    call imsumi (list, Memc[image], im_out, nlow, nhigh, Memc[option])
	case TY_LONG:
	    call imsuml (list, Memc[image], im_out, nlow, nhigh, Memc[option])
	case TY_REAL:
	    call imsumr (list, Memc[image], im_out, nlow, nhigh, Memc[option])
	case TY_DOUBLE:
	    call imsumd (list, Memc[image], im_out, nlow, nhigh, Memc[option])
	default:
	    call imsumr (list, Memc[image], im_out, nlow, nhigh, Memc[option])
	}
	call imunmap (im_out)
	call imunmap (im_in)

	# Set the header parameters.
	call imtrew (list)
	call imsum_hparam (list, Memc[image], Memc[hparams], Memc[option]) 

	call imtclose (list)
	call sfree (sp)
end

# IMSUM_SET -- Determine the output image pixel type and the calculation
# datatype.  The default pixel types are based on the highest arithmetic
# precendence of the input images.

define	NTYPES	5

procedure imsum_set (list, pixtype, calctype)

int	list				# List of input images
int	pixtype				# Pixel datatype of output image
int	calctype			# Pixel datatype for calculations

int	i, j, nimages, max_type
pointer	sp, str, im1, im2

int	imtgetim(), imtlen()
bool	xt_imleneq()
pointer	immap()
errchk	immap, imunmap

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Determine maximum precedence datatype.
	# Also check that the images are the same dimension and size.

	nimages = imtlen (list)
	j = imtgetim (list, Memc[str], SZ_LINE)
	im1 = immap (Memc[str], READ_ONLY, 0)
	max_type = IM_PIXTYPE (im1)

	do i = 2, nimages {
	    j = imtgetim (list, Memc[str], SZ_LINE)
	    im2 = immap (Memc[str], READ_ONLY, 0)

	    if ((IM_NDIM(im1) != IM_NDIM(im2)) || !xt_imleneq (im1, im2)) {
		call imunmap (im1)
		call imunmap (im2)
		call error (0, "Images have different dimensions or sizes")
	    }

	    switch (IM_PIXTYPE (im2)) {
	    case TY_SHORT:
		if (max_type == TY_USHORT)
		    max_type = TY_INT
	    case TY_USHORT:
		if (max_type == TY_SHORT)
		    max_type = TY_INT
	    case TY_INT:
		if (max_type == TY_USHORT || max_type == TY_SHORT)
		    max_type = IM_PIXTYPE (im2)
	    case TY_LONG:
		if (max_type == TY_USHORT || max_type == TY_SHORT ||
		    max_type == TY_INT)
		    max_type = IM_PIXTYPE (im2)
	    case TY_REAL:
		if (max_type != TY_DOUBLE)
		    max_type = IM_PIXTYPE (im2)
	    case TY_DOUBLE:
		max_type = IM_PIXTYPE (im2)
	    default:
	    }
	    call imunmap (im2)
	}

	call imunmap (im1)
	call imtrew (list)

	# Set calculation datatype.
	call clgstr ("calctype", Memc[str], SZ_LINE)
	switch (Memc[str]) {
	case EOS:
	    calctype = max_type
	case 's':
	    calctype = TY_SHORT
	case 'i':
	    calctype = TY_INT
	case 'l':
	    calctype = TY_LONG
	case 'r':
	    calctype = TY_REAL
	case 'd':
	    calctype = TY_DOUBLE
	default:
	    call error (0, "Unrecognized datatype")
	}

	# Set output pixel datatype.
	call clgstr ("pixtype", Memc[str], SZ_LINE)
	switch (Memc[str]) {
	case EOS:
	    pixtype = calctype
	case 'u':
	    pixtype = TY_USHORT
	case 's':
	    pixtype = TY_SHORT
	case 'i':
	    pixtype = TY_INT
	case 'l':
	    pixtype = TY_LONG
	case 'r':
	    pixtype = TY_REAL
	case 'd':
	    pixtype = TY_DOUBLE
	default:
	    call error (0, "Unrecognized datatype")
	}

	call sfree (sp)
end

# IMSUM_HPARM -- Arithmetic on image header parameters.
#
# This program is limited by a lack of a rewind procedure for the image
# header fields list.  Thus, a static array of field names is used
# to require only one pass through the list and the images.

define	NFIELDS		10	# Maximum number of fields allowed.

procedure imsum_hparam (list, output, hparams, option)

int	list			# List of input images.
char	output[ARB]		# Output image
char	hparams[ARB]		# List of header parameters
char	option[ARB]		# Sum option

int	i, nfields, flist
pointer	sp, field, dvals, image, in, out

int	imofnlu(), imgnfn(), imtgetim(), imtlen()
bool	strne(), streq()
double	imgetd()
pointer	immap()

errchk	immap, imofnlu, imgetd, imputd, imunmap

begin
	# Return if median.
	if (strne (option, "average") && strne (option, "sum"))
	    return

	# Allocate memory.
	call smark (sp)
	call salloc (field, NFIELDS*SZ_FNAME, TY_CHAR)
	call salloc (dvals, NFIELDS, TY_DOUBLE)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Map the fields.
	out = immap (output, READ_WRITE, 0)
	flist = imofnlu (out, hparams)
	i = 0
	while ((i < NFIELDS) &&
	    (imgnfn (flist, Memc[field+i*SZ_FNAME], SZ_FNAME) != EOF))
	    i = i + 1
	call imcfnl (flist)

	# Accumulate values from each image.

	nfields = i
	call aclrd (Memd[dvals], nfields)

	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    in = immap (Memc[image], READ_ONLY, 0)
	    do i = 1, nfields
		Memd[dvals+i-1] = Memd[dvals+i-1] +
		    imgetd (in, Memc[field+(i-1)*SZ_FNAME])
	    call imunmap (in)
	}

	# Output the sums or average.
	if (streq (option, "average")) {
	    i = imtlen (list)
	    call adivkd (Memd[dvals], double (i), Memd[dvals], nfields)
	}

	do i = 1, nfields
	    call imputd (out, Memc[field+(i-1)*SZ_FNAME], Memd[dvals+i-1])

	call imunmap (out)
	call sfree (sp)
end
