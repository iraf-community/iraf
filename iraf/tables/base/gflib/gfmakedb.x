include <imhdr.h>
include "gf.h"

# GF_MAKE_DB -- Create a database of extension keyword names

procedure gf_make_db (im, db)

pointer	im		# i: image descriptor
pointer	db		# o: database descriptor
#--
char	nl, eq
int	ic, jc, nc, ikey, nkey, line_len, db_len
pointer	sp, ua, keyword

data	nl  / '\n' /
data	eq  / '=' /

int	stridx()
int	gf_inithash(), gf_imtype(), gi_gstfval(), gi_ggpn()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (keyword, SZ_SHORTSTR, TY_CHAR)

	# Determine type of template image

	switch (gf_imtype (im)) {
	case FITS_FMT:
	    # Allocate database structure

	    db = gf_inithash (0)

	    # Get user area and its field widths

	    ua = IM_USERAREA(im)
	    line_len = stridx (nl, Memc[ua])

	    # Copy keyword names into the database

	    for (ic = 0; Memc[ua+ic] != EOS; ic = ic + line_len) {
		call gf_trimhash (Memc[ua+ic], Memc[keyword], SZ_SHORTSTR)
		call gf_addhash (db, Memc[keyword])
	    }

	case GEIS_FMT:
	    # Allocate database structure

	    db = gf_inithash (0)

	    # Get the number of keywords 

	    nkey = gi_gstfval (im, "PCOUNT")

	    # Allocate space for database

	    db_len = nkey * SZ_KEYWORD
	    call malloc (db, db_len, TY_CHAR)

	    # Copy keyword names from stf descriptor

	    jc = 0
	    do ikey = 1, nkey {
		# Get i-th keyowrd

		nc = gi_ggpn (im, ikey, Memc[keyword], SZ_SHORTSTR)

		# Add keyword to the database

		call gf_addhash (db, Memc[keyword])
	    }

	    Memc[db+jc] = EOS

	default:
	    # No extension keywords unless geis or fits file
	    db = NULL
	}

	call sfree (sp)
end
