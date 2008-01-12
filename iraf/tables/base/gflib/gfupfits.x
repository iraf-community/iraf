include <syserr.h>
include <imhdr.h>
include <imio.h>
include "gf.h"

#* HISTORY *
#* B.Simon	19-Nov-99	Original code

# GF_UPFITS -- Update a fits extension in a file. Closes the extension

procedure gf_upfits (im, prim, ext)

pointer	im		# i: image descriptor 
int	prim		# i: spool file with primary header keywords
int	ext		# i: spool file with extension header keywords
#--
int	acmode, gn
pointer	sp, primary

int	gf_find_db()
pointer	immap()

begin
	# Allocate temporary strings

	call smark (sp)
	call salloc (primary, SZ_PATHNAME, TY_CHAR)

	# Construct the primary header name

	gn = 0
	call gf_imname (IM_HDRFILE(im), "", READ_WRITE, gn,
			Memc[primary], SZ_FNAME)

	# If inherit is off, the primary header only
	# contains changes and should be appended

	if (gf_find_db (im, PARAM_INHERIT) == NO) {
	    acmode = APPEND
	} else {
	    acmode = WRITE_ONLY
	}

	# Copy the records in the extension spool file back to the
	# extension header

	call gf_upuser (im, ext, WRITE_ONLY)
	# Close the fits extension so we can open primary header

	call imunmap (im)

	# If the flag is set, also update the primary header

	if (prim != NULL) {
	    im = immap (Memc[primary], READ_WRITE, NULL)
	    call gf_upuser (im, prim, acmode)
	    call imunmap (im)
	}

	call sfree (sp)
end

