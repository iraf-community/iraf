include	"gf.h"

#* HISTORY *
#* B.Simon	09-Nov-99	Original code

# GF_CHK_KEY -- Check a keyword to see if it is in the primary header

procedure gf_chk_key (im, keyword)

pointer	im		# i: image descriptor
char	keyword[ARB]	# i: keyword name
#--
pointer	db

int	gf_find_db(), gf_findhash()

begin
	# Nothing to check if image is not in database cache

	db = gf_find_db (im, PARAM_DB) 
	if (db == NULL)
	    return

	# If the keyword is not in the extension header it must be in the
	# primary header, so we must update it

	if (gf_findhash (db, keyword) != NOT_FOUND) {
	    call gf_force_db (im)
	}
end
