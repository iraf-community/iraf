include <imio.h>
include	<imhdr.h>
include "gf.h"

#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types
#* B.Simon	18-Dec-98	Reworked to call immap instead of hash code

# GF_GFIND -- Return YES if a keyword is from the GPB or extension header

int procedure gf_gfind (im, keyword)

pointer im              # i: Image descriptor
char    keyword[ARB]    # i: Group parameter keyword name
#--
include "gf_gfind.com"
data	gf_init	  / NO /

int	nc, type, num, ingroup
pointer	sp, val1, val2

extern	gf_gfind_close

bool	strne()
int	btoi(), gstrcpy(), imaccf(), gf_imtype(), gi_gfind() 
pointer	immap()

begin
	# Initialize common block at first call

	if (gf_init == NO) {
	    gf_init = YES
	    imhead = NULL
	    primary[1] = EOS

	    call onexit (gf_gfind_close)
	}

	# Determine image type

	type = gf_imtype (im)

	if (type == GEIS_FMT) {
	    # Call geis specific function if geis file
	    num = gi_gfind (im,keyword)
	    return (btoi (num > 0))

	} else if (type != FITS_FMT) {
	    return (NO)
	}

	call smark (sp)
	call salloc (val1, SZ_FNAME, TY_CHAR)
	call salloc (val2, SZ_FNAME, TY_CHAR)

	# Check to see if we are using the header of the right image

	if (strne (IM_HDRFILE(im), primary)) {

	    # Release image descriptor for old image

	    call gf_gfind_close ()

	    # Read primary header for new image

	    nc = gstrcpy (IM_HDRFILE(im), primary, SZ_PATHNAME)
	    call strcat ("[0]", primary, SZ_PATHNAME)

	    imhead = immap (primary, READ_ONLY, NULL)
	    primary[nc+1] = EOS
	}

	# Check user area for keyword

	if (imaccf (im, keyword) == NO) {
	    ingroup = NO

	} else if (imaccf (imhead, keyword) == NO) {
	    ingroup = YES

	} else {
	    # Compare values if found in primary header and extension

	    call imgstr (im, keyword, Memc[val1], SZ_FNAME)
	    call imgstr (imhead, keyword, Memc[val2], SZ_FNAME)

	    ingroup = btoi (strne (Memc[val1], Memc[val2]))
	}

	call sfree (sp)
	return (ingroup)
end

# GF_GFIND_CLOSE -- Close any header that might still be open

procedure gf_gfind_close ()

#--
include	"gf_gfind.com"

begin
	if (imhead != NULL) {
	    call imunmap (imhead)
	    primary[1] = EOS
	    imhead = NULL
	}
end
