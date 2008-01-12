include <imhdr.h>
include <imio.h>
include "gf.h"

#* HISTORY *
#* B.Simon	19-Nov-99	Original code
#* B.Simon	08-Nov-00	Do not close and reopen template
#* B.Simon	20-Nov-00	get default extension with gf_getext

# GF_PRIMARY -- Copy the primary header of one fits file to another

procedure gf_primary (image, oldim)

char	image[ARB]	# i: new image name
pointer	oldim		# i: image template descriptor
#--
int	code, oldcode, gn, gcount
pointer	im, db, prim, ext, tempim
pointer	sp, cluster, ksection, section, primary, fullname

int	imaccf(), open(), imgeti(), gf_filetype(), gf_imtype()
pointer	immap()

begin
	# Can't copy primary image if old image is NULL

	if (oldim == NULL)
	    return

	# Allocate memory for primary header file name

	call smark (sp)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (ksection, SZ_SHORTSTR, TY_CHAR)
	call salloc (section, SZ_SHORTSTR, TY_CHAR)
	call salloc (primary, SZ_PATHNAME, TY_CHAR)
	call salloc (fullname, SZ_PATHNAME, TY_CHAR)

	# Only copy the primary header if the image is the first extension
	# of a fits file

	call imparse (image, Memc[cluster], SZ_FNAME, 
		      Memc[ksection], SZ_SHORTSTR, 
		      Memc[section], SZ_SHORTSTR, 
		      gn, gcount)

	code = gf_filetype (Memc[cluster], NEW_COPY)
	
	if (code == FITS_FMT && (gn == -1 || gcount != -1)) {
	    # Build name of primary header in new image

	    gn = 0
	    call gf_imname (Memc[cluster], "", NEW_COPY, gn,
			    Memc[fullname], SZ_PATHNAME)

	    # Determine template image type

	    oldcode = gf_imtype (oldim)

	    if (oldcode == FITS_FMT) {
		# Open the template image's primary header without inherit

		gn = 0
		call gf_imname (IM_HDRFILE(oldim), "", READ_ONLY, gn,
			    Memc[primary], SZ_PATHNAME)

		tempim = immap (Memc[primary], READ_ONLY, NULL)

		im = immap (Memc[fullname], NEW_COPY, tempim)

		call imunmap (im)
		call imunmap (tempim)

	    } else if (oldcode == GEIS_FMT) {
		# Open a temporary file to hold the primary header values

		prim = open ("primary", READ_WRITE, SPOOL_FILE)

		# Get primary header keywords from old image

		call gf_make_db (oldim, db)
		call gf_split_ua (oldim, db, prim, ext)

		call mfree (db, TY_CHAR)

		# Open the primary header of the new image

		im = immap (Memc[fullname], NEW_COPY, oldim)

		# Overwrite the user area with correct set of header keywords

		call gf_upuser (im, prim, WRITE_ONLY)

		if (prim != NULL)
		    call close (prim)

		if (ext != NULL)
		    call close (ext)

                # Set EXTEND and NEXTEND in the primary header

		call imaddb (im, "EXTEND", true)

                if (gcount > 0) {
                    call imaddi (im, "NEXTEND", gcount)

                } else if (imaccf (oldim, "GCOUNT") == YES) {
                    gcount = imgeti (oldim, "GCOUNT")
                    call imaddi (im, "NEXTEND", gcount)
                }

 		call imunmap (im)
		call close (prim)

	    } else {
		# All keywords are primary keywords, so this is a simple copy

		im = immap (Memc[fullname], NEW_COPY, oldim)
		call imunmap (im)
	    }
	}

	call sfree (sp)
end
