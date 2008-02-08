include	<imhdr.h> 
include "gf.h"

#* HISTORY *
#* B.Simon	19-Nov-99	Original code
#* B.Simon	20-Nov-00	get default extension with gf_getext

# GF_EXTEND -- Determine if fits file has extensions and inherits keywords

procedure gf_extend (image, acmode, oldim, extend, inherit)

char	image[ARB]	# i: image name
int	acmode		# i: image access mode
pointer	oldim		# i: image template descriptor
int	extend		# o: does fits file have extensions?
int	inherit		# o: do extensions inherit primary header keywords?
#--
int	code, gn, gcount
pointer	sp, im, cluster, ksection, section, primary, fullname

int	btoi(), gf_find_db_i(), gf_filetype(), gi_gfitval()
pointer	immap(), gf_find_db_p()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (ksection, SZ_SHORTSTR, TY_CHAR)
	call salloc (section, SZ_SHORTSTR, TY_CHAR)
	call salloc (primary, SZ_PATHNAME, TY_CHAR)
	call salloc (fullname, SZ_PATHNAME, TY_CHAR)

	# Determine image type

	call imparse (image, Memc[cluster], SZ_FNAME, 
		      Memc[ksection], SZ_SHORTSTR, 
		      Memc[section], SZ_SHORTSTR, 
		      gn, gcount)

	code = gf_filetype (Memc[cluster], acmode)
	
	# Only fits files have extensions

	if (code != FITS_FMT) {
	    extend = NO
	    inherit = NO

	} else if (oldim == NULL) {
	    # Build name of first extension

	    gn = 1
	    call gf_imname (Memc[cluster], "", READ_ONLY, gn,
			    Memc[fullname], SZ_PATHNAME)

	    # If open fails, extend is false

	    iferr {
		im = immap (Memc[fullname], READ_ONLY, NULL)
	    } then {
		extend = NO
		inherit = NO
	    } else {
		extend = YES
		inherit = gi_gfitval (im, "INHERIT") 

		call imunmap (im)
	    }

	} else {
	    if (gf_find_db_p (oldim, PARAM_DB) == NULL) {
		# Only copies of geis templates have extensions

		code = gf_filetype (IM_HDRFILE(oldim))
		extend = btoi (code == GEIS_FMT)
		inherit = extend

	    } else {
		extend = gf_find_db_i (oldim, PARAM_EXTEND)
		inherit = gf_find_db_i (oldim, PARAM_INHERIT)

		if (extend == NO) {
		    # Explicit copy into an extension of an image 
		    # sets extend to yes

		    call imparse (image, Memc[cluster], SZ_FNAME, 
				  Memc[ksection], SZ_SHORTSTR, 
				  Memc[section], SZ_SHORTSTR, 
				  gn, gcount)

		    if ((gn != -1 && gcount == -1) || Memc[ksection] != EOS) {
			extend = YES
			inherit = YES
		    }
		}
	    }
	}

	call sfree (sp)
end

