include <imhdr.h>
include	<error.h>

# Maximum number of correction function coefficients.
define	MAXCOEF	3

# Maximum number of ADU....
define	MAXADU	32767.0


# T_ARLINCOR -- Corrects IR imager frames for non linearity. This task
# only corrects a section of the total image and copies the rest of
# the image intact to the output image.

procedure t_irlincor ()

pointer	inlist, outlist		# input and output image lists
char	section[SZ_LINE]	# image section
pointer	coeff			# coeficients of correction function

bool	sflag
pointer	imin, imout
pointer	input, output, orig, temp
pointer	sp

int	strlen()
int	imtgetim(), imtlen()
real	clgetr()
pointer	immap(), imtopenp()

begin
	# Get parameters
	inlist  = imtopenp ("input")
	outlist = imtopenp ("output")
	call clgstr ("section", section, SZ_LINE)

	# Check that the input and output image lists have the
	# same number of images. Abort if that's not the case.
	if (imtlen (inlist) != imtlen (outlist)) {
	    call imtclose (inlist)
	    call imtclose (outlist)
	    call error (1, "Input and output image lists don't match")
	}

	# Set section flag
	sflag = (strlen (section) > 0)

	# Allocate string space
	call smark  (sp)
	call salloc (input,  SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (orig,   SZ_FNAME, TY_CHAR)
	call salloc (temp,   SZ_FNAME, TY_CHAR)

	# Allocate memory for the correction coefficients and
	# read them from the parameter file.
	call malloc (coeff, MAXCOEF, TY_REAL)
	Memr[coeff]   = clgetr ("coeff1")
	Memr[coeff+1] = clgetr ("coeff2")
	Memr[coeff+2] = clgetr ("coeff3")

	# Loop over all images in the input and output lists
	while ((imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) &&
	       (imtgetim (outlist, Memc[output], SZ_FNAME) != EOF)) {

	    # Generate temporary output image name to allow for
	    # input and output images having the same name
	    call xt_mkimtemp (Memc[input], Memc[output], Memc[orig], SZ_FNAME)

	    # Take different actions depending on whether the image section
	    # is specified or not, in order to optimize speed. When the image
	    # section is specified the input image is copied to the output
	    # image and then the output image opened to work on the section.
	    # Otherwise the output image is created only once.
	    if (sflag) {

	        # Copy input image into output image using fast copy
		iferr (call irl_imcopy (Memc[input], Memc[output])) {
		    call erract (EA_WARN)
		    next
		}

		# Append section to image names. The output name should
		# be preserved without the section for later use.
		call strcat (section, Memc[input], SZ_FNAME)
		call sprintf (Memc[temp], SZ_FNAME, "%s%s")
		    call pargstr (Memc[output])
		    call pargstr (section)

		# Open input and output images. The output image already
		# exists, since it was created by the copy operation, so
		# it is opened as read/write.
		iferr (imin = immap (Memc[input], READ_ONLY, 0)) {
		    call erract (EA_WARN)
		    next
		}
		iferr (imout = immap (Memc[temp], READ_WRITE, 0)) {
		    call imunmap (imin)
		    call erract (EA_WARN)
		    next
		}

	    } else {

		# Open input and output images. The output image does not
		# exist already so it is opened as a new copy of the input
		# image.
		iferr (imin = immap (Memc[input], READ_ONLY, 0)) {
		    call erract (EA_WARN)
		    next
		}
		iferr (imout = immap (Memc[output], NEW_COPY, imin)) {
		    call imunmap (imin)
		    call erract (EA_WARN)
		    next
		}

	    }

	    # Perform the linear correction.
	    call irl_correct (imin, imout, Memr[coeff], MAXCOEF)

	    # Close images
	    call imunmap (imin)
	    call imunmap (imout)

	    # Replace output image with the temporary image. This is a
	    # noop if the input and output images have different names
	    call xt_delimtemp (Memc[output], Memc[orig])
	}

	# Free memory and close image lists
	call mfree (coeff, TY_REAL)
	call imtclose (inlist)
	call imtclose (outlist)
end


# IRL_CORRECT -- Corrects an IR imager frame for non-linearity using a
# simple power series polynomial correction function:
#
# ADU' = ADU * [ a + b * (ADU / MAXADU) + c * (ADU / MAXADU) **2 ]
#

procedure irl_correct (imin, imout, coeff, ncoef)

pointer	imin				# input image pointer
pointer	imout				# output image pointer
real	coeff[ncoef]			# coefficients of polynomial function
int	ncoef				# number of polynomial coeficients

int	col, ncols
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	inbuf, outbuf

int	imgeti()
int	imgnlr(), impnlr()
real	apolr()

begin
	# Initiliaze counters for line i/o
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	# Number of pixels per line
	ncols = imgeti (imin, "i_naxis1")

	# Loop over image lines
	while ((imgnlr (imin, inbuf, v1) != EOF) &&
	       (impnlr (imout, outbuf, v2) != EOF)) {
	    call adivkr (Memr[inbuf], MAXADU, Memr[outbuf], ncols)
	    do col = 1, ncols {
		Memr[outbuf+col-1] = apolr (Memr[outbuf+col-1], coeff, ncoef)
	    }
	    call amulr (Memr[inbuf], Memr[outbuf], Memr[outbuf], ncols)
	}
end


# IRL_IMCOPY -- Copy input image into the output image. Avoid data type
# conversion in order to opetimize speed.

procedure irl_imcopy (input, output)

char	input[ARB]		# input image name
char	output[ARB]		# output image name

int	npix
long	vin[IM_MAXDIM], vout[IM_MAXDIM]
pointer	imin, imout
pointer	inline, outline

int	imgeti()
int	imgnls(), impnls()
int	imgnli(), impnli()
int	imgnll(), impnll()
int	imgnlr(), impnlr()
int	imgnld(), impnld()
int	imgnlx(), impnlx()
pointer	immap()

begin
	# Open input and output images
	iferr (imin = immap (input, READ_ONLY, 0))
	    call erract (EA_ERROR)
	iferr (imout = immap (output, NEW_COPY, imin)) {
	    call imunmap (imin)
	    call erract (EA_ERROR)
	}

	# Initiliaze counters
	call amovkl (long(1), vin,  IM_MAXDIM)
	call amovkl (long(1), vout, IM_MAXDIM)

	# Copy image lines
	switch (imgeti (imin, "i_pixtype")) {
	case TY_SHORT, TY_USHORT:
	    while (imgnls (imin, inline, vin) != EOF) {
		npix = impnls (imout, outline, vout)
		call amovs (Mems[inline], Mems[outline], npix)
	    }
	case TY_INT:
	    while (imgnli (imin, inline, vin) != EOF) {
		npix = impnli (imout, outline, vout)
		call amovi (Memi[inline], Memi[outline], npix)
	    }
	case TY_LONG:
	    while (imgnll (imin, inline, vin) != EOF) {
		npix = impnll (imout, outline, vout)
		call amovl (Meml[inline], Meml[outline], npix)
	    }
	case TY_REAL:
	    while (imgnlr (imin, inline, vin) != EOF) {
		npix = impnlr (imout, outline, vout)
		call amovr (Memr[inline], Memr[outline], npix)
	    }
	case TY_DOUBLE:
	    while (imgnld (imin, inline, vin) != EOF) {
		npix = impnld (imout, outline, vout)
		call amovd (Memd[inline], Memd[outline], npix)
	    }
	case TY_COMPLEX:
	    while (imgnlx (imin, inline, vin) != EOF) {
		npix = impnlx (imout, outline, vout)
		call amovx (Memx[inline], Memx[outline], npix)
	    }
	default:
	    call error (0, "Unsupported pixel type")
	}

	# Close images
	call imunmap (imin)
	call imunmap (imout)
end
