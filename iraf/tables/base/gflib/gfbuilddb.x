include	<imhdr.h>
include "gf.h"

#* HISTORY *
#* B.Simon	19-Nov-99	Original code
#* B.Simon	08-Nov-00	Do not close and reopen template
#* I.Busko	19-Oct-01	Add ksection to build FITS extension name

# GF_BUILD_DB -- Create a database of header keyword names

procedure gf_build_db (image, im, db)

char	image[ARB]	# i: image name
pointer	im		# i: image descriptor
pointer	db		# o: database descriptor
#--
int	gn, gcount
pointer	sp, cluster, ksection, section, fullname, tempim

bool	streq()
pointer	immap()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (ksection, SZ_SHORTSTR, TY_CHAR)
	call salloc (section, SZ_SHORTSTR, TY_CHAR)
	call salloc (fullname, SZ_FNAME, TY_CHAR)

	# Open the image noinherit to get the extension keywords only

	call imparse (image, Memc[cluster], SZ_FNAME, 
		      Memc[ksection], SZ_SHORTSTR, 
		      Memc[section], SZ_SHORTSTR, 
		      gn, gcount)

	# ksection is required when the FITS extension spec is of the
	# form [extname,extver] (IB, 10/19/01)  

	call strcat (Memc[ksection], Memc[cluster], SZ_FNAME)

	call gf_imname (Memc[cluster], "noinherit", READ_ONLY, gn, 
			Memc[fullname], SZ_FNAME)

	# Check new name against old to stop needless open

	if (streq (image, Memc[fullname]) && im != NULL) {
	    # Construct database from template image

	    call gf_make_db (im, db)

	} else {
	    # Open the extension with noinherit and create the database

	    ifnoerr {
		tempim = immap (Memc[fullname], READ_ONLY, NULL)
	    } then {
		call gf_make_db (tempim, db)
		call imunmap (tempim)
	    } else {
		db = NULL
	    }
	}

	# Close image and free memory

	call sfree (sp)

end
