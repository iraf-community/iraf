include <imio.h>
include	<imhdr.h>
include <syserr.h>
include "gf.h"

#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types
#* B.Simon	19-Nov-99	Added extension header fixup code
#* B.Simon	19-Dec-00	Added call to gi_upuser

# GF_OPENGR -- Open another group in an already open image
	    
procedure gf_opengr (im, gnum, datamin, datamax, oldim)

pointer	im		# u: image descriptor
int	gnum		# i: group number to skip to
real	datamin		# u: image minimun value
real	datamax		# u: image maximum value
pointer oldim		# i: image template descriptor (NEW_COPY only)
#--
int	gn, code, mode, lastim, pixtype, ext, prim, hist
pointer	sp, db, fullname, extra

int	gf_find_db(), gf_gstfval(), gf_imtype(), gfhist()
pointer	immap()

errchk	gi_opengr, immap

begin
	# Avoid opening the same group twice

	db = gf_find_db (im, PARAM_DB)

	if (db  == NULL) {
	    gn = gf_gstfval (im, "GROUP")
	} else {
	    gn = gf_find_db (im, PARAM_GN)
	}

	if (gn == gnum)
	    return

	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (fullname, SZ_PATHNAME, TY_CHAR)
	call salloc (extra, SZ_SHORTSTR, TY_CHAR)

	pixtype = IM_PIXTYPE(im)

	code = gf_imtype (im)
	mode = IM_ACMODE(im)

	if (code == GEIS_FMT) {
	    # If this is a geis file, call the geis optimized version

	    call gi_opengr (im, gnum, datamin, datamax, oldim)
	    call sfree (sp)
	    return

	} else if (code != FITS_FMT) {
	    # Only geis and extended fits files support multiple groups

	    if (gnum > 1) {
		call sprintf (Memc[fullname], SZ_PATHNAME, "%s[%d]")
		call pargstr (IM_HDRFILE(im))
		call pargi (gnum)

		call syserrs (SYS_IKIOPEN, Memc[fullname])
	    }

	    call sfree (sp)
	    return
	}

	# For fits files, emulate the open group call with an immap call
	# The parameters are determined by the access mode

	if (gf_find_db (im, PARAM_INHERIT) == YES) {
	    call strcpy ("inherit", Memc[extra], SZ_SHORTSTR)
	} else {
	    Memc[extra] = EOS
	}

	call gf_imname (IM_HDRFILE(im), Memc[extra], mode, gnum, 
			Memc[fullname], SZ_PATHNAME)

	# Update the old extension and close it

	lastim = im
	call gf_split_ua (im, db, prim, ext)
	call gf_upfits (im, prim, ext)

	if (gn > 0) {
		hist = gfhist(Memc[fullname])
        } else {
		hist = -1
	}

	# Open the new extension
	im = immap (Memc[fullname], mode, oldim)

	# Reset the database cache
	call gf_reset_db (lastim, im, gnum, hist)

	# Fix the user area if the image is opened in new copy mode
	#   kernel re-writes primary for each NEW_COPY extension. undo this.
	if (mode == NEW_COPY) {
	    call gf_newcopy (im, prim)
	}

	# If this is a new image, set the pixel code to that of the 
	# previous group. You can override this by explicitly setting
	# IM_PIXCODE in your own code after calling this function.

	if (mode != READ_ONLY && mode != READ_WRITE)
	    IM_PIXTYPE(im) = pixtype

	# Retrieve the min and max pixel values (needed for compatibility
	# with gilib version of this call)

	datamin = IM_MIN(im)
	datamax = IM_MAX(im)

	if (prim != NULL)
	    call close (prim)

	if (ext != NULL)
	    call close (ext)

	call sfree (sp)
end
