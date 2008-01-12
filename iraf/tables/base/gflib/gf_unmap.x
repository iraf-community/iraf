include	"gf.h"

include <imhdr.h>

#* HISTORY *
#* B.Simon	19-Nov-99	Original code

# GF_UNMAP -- Unmap the image after handling fits extension keywords

procedure gf_unmap (im)

pointer	im		# i: image descriptor
#--
int	oldim, prim, ext
pointer	db

int	gf_find_db()

begin
	# Update the extension header

	oldim = im
	db = gf_find_db (im, PARAM_DB)

	call gf_split_ua (im, db, prim, ext)
	call gf_upfits (im, prim, ext)

	if (prim != NULL)
	    call close (prim)

	if (ext != NULL)
	    call close (ext)

	# Free the database that tells us what is what

	call gf_free_db (oldim)

end

