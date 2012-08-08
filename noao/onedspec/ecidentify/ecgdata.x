include	<imhdr.h>
include	<imio.h>
include	<pkg/gtools.h>
include	<smw.h>
include	<units.h>
include	"ecidentify.h"

# EC_GDATA -- Get image data.

procedure ec_gdata (ec)

pointer	ec				# ID pointer

int	i, j
pointer	im, mw, sh, sp, str1, str2

double	smw_c1trand()
pointer	immap(), smw_openim(), smw_sctran()
errchk	immap, smw_openim, shdr_open

begin
	# Map the image.
	im = immap (Memc[EC_IMAGE(ec)], READ_ONLY, 0)

	# Free previous data
	do i = 1, EC_NLINES(ec)
	    call shdr_close (SH(ec,i))
	call mfree (EC_SHS(ec), TY_POINTER)
	call mfree (EC_PIXDATA(ec), TY_DOUBLE)

	# Set MWCS
	mw = smw_openim (im)
	EC_LP(ec) = smw_sctran (mw, "logical", "physical", 1)
	EC_PL(ec) = smw_sctran (mw, "physical", "logical", 1)

	# Allocate new vectors.
	EC_NCOLS(ec) = IM_LEN(im, 1)
	EC_NLINES(ec) = IM_LEN(im, 2)
	call calloc (EC_SHS(ec), EC_NLINES(ec), TY_POINTER)
	call malloc (EC_PIXDATA(ec), EC_NCOLS(ec)*EC_NLINES(ec), TY_DOUBLE)

	# Set the coordinates.
	sh = NULL
	do j = 1, EC_NLINES(ec) {
	    call shdr_open (im, mw, j, 1, INDEFI, SHDATA, sh)
	    if (EC_UN(ec) != NULL)
		iferr (call shdr_units (sh, UN_UNITS(EC_UN(ec))))
		    ;
	    if (j != EC_NLINES(ec))
		call shdr_copy (sh, SH(ec,j), NO)
	    else
		SH(ec,j) = sh
	    call ec_gline (ec, j)
	    do i = 1, EC_NPTS(ec)
	        PIXDATA(ec,i) = smw_c1trand (EC_LP(ec), double(i))
	}
	EC_LINE(ec) = 1
	call ec_gline (ec, EC_LINE(ec))
	EC_AP(ec) = APS(ec,EC_LINE(ec))
	EC_ORDER(ec) = ORDERS(ec,EC_LINE(ec))

	# Set graph title.
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	call sprintf (Memc[str1], SZ_LINE, "ecidentify %s: %s")
	    call pargstr (Memc[EC_IMAGE(ec)])
	    call pargstr (IM_TITLE(im))
	call gt_sets (EC_GT(ec), GTTITLE, Memc[str1])

	call imunmap (im)
	call sfree (sp)
end
