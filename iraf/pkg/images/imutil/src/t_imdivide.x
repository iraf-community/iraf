# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# T_IMDIVIDE -- Image division with rescaling.

# Options for rescaling.
define	NORESC	1		# Do not scale resultant image
define	MEAN	2		# Scale resultant mean to given value
define	NUMER	3		# Scale resultant mean to mean of numerator

procedure t_imdivide ()

char	image1[SZ_FNAME]			# Numerator image
char	image2[SZ_FNAME]			# Denominator image
char	image3[SZ_FNAME]			# Resultant image
char	title[SZ_IMTITLE]			# Resultant image title
int	rescale					# Option for rescaling
real	constant				# Replacement for zero divide
bool	verbose					# Verbose output?

char	str[SZ_LINE]
int	i, npix, ntotal
real	sum1, sum2, sum3, scale
long	line1[IM_MAXDIM], line2[IM_MAXDIM], line3[IM_MAXDIM]
pointer	im1, im2, im3, data1, data2, data3

int	clgwrd(), imgnlr(), impnlr()
bool	clgetb(), strne()
real	clgetr(), asumr(), ima_efncr()
pointer	immap()
extern	ima_efncr

common	/imadcomr/ constant

begin
	# Access images and set parameters.
	call clgstr ("numerator", image1, SZ_FNAME)
	im1 = immap (image1, READ_ONLY, 0)
	call clgstr ("denominator", image2, SZ_FNAME)
	im2 = immap (image2, READ_ONLY, 0)
	call clgstr ("resultant", image3, SZ_FNAME)
	im3 = immap (image3, NEW_COPY, im1)

	if (IM_NDIM (im1) != IM_NDIM (im2))
	    call error (0, "Input images have different dimensions")
	do i = 1, IM_NDIM (im1)
	    if (IM_LEN (im1, i) != IM_LEN (im2, i))
		call error (0, "Input images have different sizes")

	call clgstr ("title", title, SZ_IMTITLE)
	if (strne (title, "*"))
	    call strcpy (title, IM_TITLE(im3), SZ_IMTITLE) 
	IM_PIXTYPE(im3) = TY_REAL

	constant = clgetr ("constant")
	verbose = clgetb ("verbose")

	# Initialize.
	npix = IM_LEN(im1, 1)
	ntotal = 0
	sum1 = 0.
	sum2 = 0.
	sum3 = 0.
	call amovkl (long(1), line1, IM_MAXDIM)
	call amovkl (long(1), line2, IM_MAXDIM)
	call amovkl (long(1), line3, IM_MAXDIM)

	# Loop through the images doing the division.
	# Accumulate the sums for mean values.
	while (impnlr (im3, data3, line3) != EOF) {
	    i = imgnlr (im1, data1, line1)
	    i = imgnlr (im2, data2, line2)
	    call advzr (Memr[data1], Memr[data2], Memr[data3], npix, ima_efncr)
	    sum1 = sum1 + asumr (Memr[data1], npix)
	    sum2 = sum2 + asumr (Memr[data2], npix)
	    sum3 = sum3 + asumr (Memr[data3], npix)
	    ntotal = ntotal + npix
	}
	sum1 = sum1 / ntotal
	sum2 = sum2 / ntotal
	sum3 = sum3 / ntotal

	# Close the images.
	call imunmap (im1)
	call imunmap (im2)
	call imunmap (im3)

	# Print image means if verbose.
	if (verbose) {
	    call printf ("Task imdivide:\n")
	    call printf ("    %s: Mean = %g\n")
		call pargstr (image1)
		call pargr (sum1)
	    call printf ("    %s: Mean = %g\n")
		call pargstr (image2)
		call pargr (sum2)
	    call printf ("    %s: Mean = %g\n")
		call pargstr (image3)
		call pargr (sum3)
	}

	# Determine resultant image rescaling.
	rescale = clgwrd ("rescale", str, SZ_LINE, ",norescale,mean,numerator,")
	switch (rescale) {
	case NORESC:
	    return
	case MEAN:
	    scale = clgetr ("mean") / sum3
	case NUMER:
	    scale = sum1 / sum3
	}

	if(verbose) {
	    call printf ("    %s: Scale = %g\n")
		call pargstr (image3)
		call pargr (scale)
	}

	# Open image read_write and initialize line counters.
	im1 = immap (image3, READ_WRITE, 0)
	call amovkl (long(1), line1, IM_MAXDIM)
	call amovkl (long(1), line2, IM_MAXDIM)

	# Loop through the image rescaling the image lines.
	while (imgnlr (im1, data1, line1) != EOF) {
	    i = impnlr (im1, data2, line2)
	    call amulkr (Memr[data1], scale, Memr[data2], npix)
	}

	call imunmap (im1)
end
