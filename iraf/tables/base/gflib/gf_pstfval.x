include <syserr.h>
include "gf.h"

#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types
#* B.Simon	16-Dec-98	Modified to use list of keywords in gf.h

# GF_PSTFVAL -- Change an integer value in the stf descriptor

procedure gf_pstfval (im, keyword, value)

pointer	im		# i: image descriptor
char	keyword[ARB]	# i: keyword name
int	value		# i: keyword value
#--
int	type, index
pointer	sp, temp

string	keylist  KEYSTRING

int	strdic(), gf_imtype ()

begin
	type = gf_imtype (im)

	if (type == GEIS_FMT) {
	    # Call geis function if this is a geis file
	    call gi_pstfval (im, keyword, value)
	    return
	}

	# Determine which keyword is to be updated

	call smark (sp)
	call salloc (temp, SZ_KEYWORD, TY_CHAR)

	call strcpy (keyword, Memc[temp], SZ_KEYWORD)
	call strlwr (Memc[temp])

	index = strdic (Memc[temp], Memc[temp], SZ_KEYWORD, keylist)

	# For now, only allow gcount to be updated

	if (index == GCOUNT_KEY) {

	    if (type == FITS_FMT) {
		call imaddi (im, "NEXTEND", value)
	    } else {
		call imaddi (im, "GCOUNT", value)
	    }

	} else {
	    call syserrs (SYS_IDBKEYNF, keyword)
	}

	call sfree (sp)
end

