include	<imhdr.h>
include	<imio.h>
include	<pkg/gtools.h>
include	"ecidentify.h"

# EC_GDATA -- Get image data.

procedure ec_gdata (ec)

pointer	ec				# ID pointer

int	i, j
pointer	im, sp, str1, str2

int	imaccf()
pointer	immap(), imgl2d()
errchk	immap, imgl2d

begin
	# Map the image.  Abort if the image is not two dimensional.
	im = immap (Memc[EC_IMAGE(ec)], READ_ONLY, 0)

	if (IM_NDIM(im) != 2) {
	    call imunmap (im)
	    call error (0, "Image is not two dimensional")
	}

	EC_NPTS(ec) = IM_LEN(im, 1)
	EC_NLINES(ec) = IM_LEN(im, 2)

	# Set graph title.
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	call sprintf (Memc[str1], SZ_LINE, "ecidentify %s: %s")
	    call pargstr (Memc[EC_IMAGE(ec)])
	    call pargstr (IM_TITLE(im))
	call gt_sets (EC_GT(ec), GTTITLE, Memc[str1])

	# Get header parameters.
	if (imaccf (im, "ctype1") == YES) {
	    call imgstr (im, "ctype1", Memc[str1], SZ_LINE)
	    call gt_sets (EC_GT(ec), GTXLABEL, Memc[str1])
	}
	if (imaccf (im, "cunit1") == YES) {
	    call imgstr (im, "cunit1", Memc[str1], SZ_LINE)
	    call gt_sets (EC_GT(ec), GTXUNITS, Memc[str1])
	}

	# Free previous data vectors and allocate new vectors.
	call mfree (EC_APS(ec), TY_INT)
	call mfree (EC_ORDERS(ec), TY_INT)
	call mfree (EC_CRVAL(ec), TY_DOUBLE)
	call mfree (EC_CDELT(ec), TY_DOUBLE)
	call mfree (EC_PIXDATA(ec), TY_DOUBLE)
	call mfree (EC_IMDATA(ec), TY_DOUBLE)
	call malloc (EC_APS(ec), EC_NLINES(ec), TY_INT)
	call malloc (EC_ORDERS(ec), EC_NLINES(ec), TY_INT)
	call malloc (EC_CRVAL(ec), EC_NLINES(ec), TY_DOUBLE)
	call malloc (EC_CDELT(ec), EC_NLINES(ec), TY_DOUBLE)
	call malloc (EC_PIXDATA(ec), EC_NPTS(ec)*EC_NLINES(ec), TY_DOUBLE)
	call malloc (EC_IMDATA(ec), EC_NPTS(ec)*EC_NLINES(ec), TY_DOUBLE)

	# Set the coordinates.
	do j = 1, EC_NLINES(ec) {
	    call sprintf (Memc[str1], SZ_LINE, "APNUM%d")
		call pargi (j)
	    call imgstr (im, Memc[str1], Memc[str2], SZ_LINE)
	    call sscan (Memc[str2])
	    call gargi (APS(ec,j))
	    call gargi (ORDERS(ec,j))
	    call gargd (CRVAL(ec,j))
	    call gargd (CDELT(ec,j))

	    EC_LINE(ec) = j
	    call ec_gline (ec, EC_LINE(ec))
	    do i = 1, EC_NPTS(ec)
	        PIXDATA(ec,i) = i
	    call amovd (Memd[imgl2d(im,j)], IMDATA(ec,1), EC_NPTS(ec))
	}
	EC_LINE(ec) = 1
	EC_AP(ec) = APS(ec,EC_LINE(ec))
	call ec_gline (ec, EC_LINE(ec))

	call imunmap (im)
	call sfree (sp)
end
