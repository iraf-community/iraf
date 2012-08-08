include	<imhdr.h>

define	IF_LOG10	1
define	IF_ALOG10	2
define	IF_LN		3
define	IF_ALN		4
define	IF_SQRT		5
define	IF_SQUARE	6
define	IF_CBRT		7
define	IF_CUBE		8
define	IF_ABS		9
define	IF_NEG		10
define	IF_COS		11
define	IF_SIN		12
define	IF_TAN		13
define	IF_ACOS		14
define	IF_ASIN		15
define	IF_ATAN		16
define	IF_COSH		17
define	IF_SINH		18
define	IF_TANH		19
define	IF_RECIPROCAL	20

define	FUNCS	"|log10|alog10|ln|aln|sqrt|square|cbrt|cube|abs|neg|\
cos|sin|tan|acos|asin|atan|hcos|hsin|htan|reciprocal|"

# T_FUNCTION -- Apply a function to a list of images.

procedure t_imfunction ()

pointer	input			# input images
pointer	output			# output images
int	func			# function
int	verbose			# verbose mode

int	list1, list2
pointer	sp, image1, image2, image3, function, im1, im2
bool	clgetb()
int	clgwrd(), imtopen(), imtgetim(), imtlen(), btoi()
pointer	immap()

begin
	# Allocate working space.

	call smark (sp)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_LINE, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (image3, SZ_FNAME, TY_CHAR)
	call salloc (function, SZ_FNAME, TY_CHAR)

	# Get image template list.

	call clgstr ("input", Memc[input], SZ_LINE)
	call clgstr ("output", Memc[output], SZ_LINE)
	func = clgwrd ("function", Memc[function], SZ_FNAME, FUNCS)
	verbose = btoi (clgetb ("verbose"))

	list1 = imtopen (Memc[input])
	list2 = imtopen (Memc[output])
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (1, "Input and output image lists don't match")
	}

	# Apply function to each input image.  Optimize IMIO.

	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
	    (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF)) {

	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[image3],
	        SZ_FNAME)
	    im1 = immap (Memc[image1], READ_ONLY, 0)
	    if (IM_PIXTYPE(im1) == TY_COMPLEX) {
		call printf ("%s is datatype complex: skipping\n")
		call imunmap (im1)
		next
	    }
	    im2 = immap (Memc[image2], NEW_COPY, im1)

	    switch (func) {
	    case IF_LOG10:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_log10d (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_log10r (im1, im2)
		}

	    case IF_ALOG10:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_alog10d (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_alog10r (im1, im2)
		}

	    case IF_LN:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_lnd (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_lnr (im1, im2)
		}

	    case IF_ALN:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_alnd (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_alnr (im1, im2)
		}

	    case IF_SQRT:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_sqrd (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_sqrr (im1, im2)
		}

	    case IF_SQUARE:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_squared (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_squarer (im1, im2)
		}

	    case IF_CBRT:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_cbrtd (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_cbrtr (im1, im2)
		}

	    case IF_CUBE:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_cubed (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_cuber (im1, im2)
		}

	    case IF_ABS:
		switch (IM_PIXTYPE(im1)) {
		case TY_SHORT, TY_INT, TY_LONG:
		    call if_absl (im1, im2)
		case TY_DOUBLE:
		    call if_absd (im1, im2)
		default:
		    call if_absr (im1, im2)
		}

	    case IF_NEG:
		# Preserve the original image type.
		switch (IM_PIXTYPE(im1)) {
		case TY_SHORT, TY_INT, TY_LONG:
		    call if_negl (im1, im2)
		case TY_DOUBLE:
		    call if_negd (im1, im2)
		default:
		    call if_negr (im1, im2)
		}

	    case IF_COS:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_cosd (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_cosr (im1, im2)
		}

	    case IF_SIN:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_sind (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_sinr (im1, im2)
		}

	    case IF_TAN:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_tand (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_tanr (im1, im2)
		}

	    case IF_ACOS:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_acosd (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_acosr (im1, im2)
		}

	    case IF_ASIN:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_asind (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_asinr (im1, im2)
		}

	    case IF_ATAN:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_atand (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_atanr (im1, im2)
		}

	    case IF_COSH:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_hcosd (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_hcosr (im1, im2)
		}

	    case IF_SINH:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_hsind (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_hsinr (im1, im2)
		}

	    case IF_TANH:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_htand (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_htanr (im1, im2)
		}

	    case IF_RECIPROCAL:
		switch (IM_PIXTYPE(im1)) {
		case TY_DOUBLE:
	            IM_PIXTYPE (im2) = TY_DOUBLE
		    call if_recipd (im1, im2)
		default:
	            IM_PIXTYPE (im2) = TY_REAL
		    call if_recipr (im1, im2)
		}

	    default:
		call error (0, "Undefined function\n")

	    }

	    if (verbose == YES) {
		call printf ("%s -> %s  function: %s\n")
		    call pargstr (Memc[image1])
		    call pargstr (Memc[image3])
		    call pargstr (Memc[function])
	    }

	    call imunmap (im1)
	    call imunmap (im2)
	    call xt_delimtemp (Memc[image2], Memc[image3])

	}

	call imtclose (list1)
	call imtclose (list2)
	call sfree (sp)
end
