include	<imhdr.h>

define	FUNCS	"|log10|alog10|sqrt|abs|"

# IMFUNCTION -- Apply a function to image pixel values.

procedure t_imfunction ()

char	input[SZ_LINE]		# Images to be modified
char	output[SZ_LINE]		# Images to be modified
int	func			# Function

int	list1, list2
char	image1[SZ_FNAME], image2[SZ_FNAME], image3[SZ_FNAME]
pointer	im1, im2
int	clgwrd(), imtopen(), imtgetim(), imtlen()
pointer	immap()

begin
	# Get image template list.

	call clgstr ("input", input, SZ_LINE)
	call clgstr ("output", output, SZ_LINE)
	func = clgwrd ("function", image1, SZ_FNAME, FUNCS)

	list1 = imtopen (input)
	list2 = imtopen (output)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (1, "Input and output image lists don't match")
	}

	# Apply function to each input image.  Optimize IMIO.

	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	    (imtgetim (list2, image2, SZ_FNAME) != EOF)) {

	    call xt_mkimtemp (image1, image2, image3, SZ_FNAME)
	    im1 = immap (image1, READ_WRITE, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    switch (func) {
	    case 1:
	        # For the log function the output image is set to real.
	        IM_PIXTYPE (im2) = TY_REAL
		call imlogr (im1, im2)
	    case 2:
	        # For the antilog function the output image is set to real.
	        IM_PIXTYPE (im2) = TY_REAL
		call imdexr (im1, im2)
	    case 3:
	        # For the sqrt function the output image is set to real.
	        IM_PIXTYPE (im2) = TY_REAL
		call imsqrr (im1, im2)
	    case 4:
		call imabsr (im1, im2)
	    }

	    call imunmap (im1)
	    call imunmap (im2)
	    call xt_delimtemp (image2, image3)
	}

	call imtclose (list1)
	call imtclose (list2)
end
