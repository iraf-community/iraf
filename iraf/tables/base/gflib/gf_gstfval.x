include <syserr.h>
include <imhdr.h>
include	<mach.h>
include "gf.h"

#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types

# GF_GSTFVAL -- Get an integer value stored in the stf descriptor

int procedure gf_gstfval (im, keyword)

pointer	im		# i: image descriptor
char	keyword[ARB]	# i: keyword to retrieve
#--
int	code, val, index
pointer	sp, temp

string	keylist  KEYSTRING

errchk	gi_gstfval, gf_gcount, gf_group

int	strdic(), sizeof()
int	gi_gstfval(), gi_gfitval(), gf_imtype(), gf_gcount()

begin
	# Retrieve value from stf kernel if geis format image

	code = gf_imtype (im)

	if (code == GEIS_FMT) {
	    val = gi_gstfval (im, keyword)
	    return (val)
	}

	# If not found in either place, emulate the value
	# Most GPB parameters can be set to zero. The group
	# count is determined by trying to open groups 
	# in the image until failure, an expensive operation.

	call smark (sp)
	call salloc (temp, SZ_KEYWORD, TY_CHAR)

	call strcpy (keyword, Memc[temp], SZ_KEYWORD)
	call strlwr (Memc[temp])

	index = strdic (Memc[temp], Memc[temp], SZ_KEYWORD, keylist)
	switch (index) {
	case GCOUNT_KEY:
	    val = gf_gcount (im)
	case PSIZE_KEY:
	    val = 0
	case PCOUNT_KEY:
	    val = 0
	case GROUP_KEY:
	    if (code == FITS_FMT) {
		val = gi_gfitval (im, "GROUP")
	    } else {
		val = 1
	    }
	case BITPIX_KEY:
	    val = sizeof (IM_PIXTYPE(im)) * SZB_CHAR * NBITS_BYTE
	case SZGROUP_KEY:
	    val = 0
	default:
            call syserrs (SYS_IDBKEYNF, keyword)
	}

	call sfree (sp)
	return (val)
end
