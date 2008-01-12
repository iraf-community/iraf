include <mach.h>
include <imhdr.h>
include "gf.h"

#* HISTORY *
#* B.Simon	29-Nov-99	Original code

# GF_MERGE_GPB -- Merge the keywords in the database into the gpb

procedure gf_merge_gpb (im, oldim, db)

pointer	im		# i: image descriptor
pointer	oldim		# i: image template descriptor
pointer	db		# i: database of extension keywords
#--
char	blank
int	ic, jc, npix, dtype, dlen, pixsize
long	one
pointer	sp, keyword, keyword2, value, vector, buffer

data	blank / ' ' /
data	one   / 1 /

string	dict  "|PCOUNT|GCOUNT|ORIGIN|INHERIT|DATE|IRAF-TLM|EXTNAME|EXTVER|"

bool	streq()
int	sizeof(), stridx(), strlen(), strdic(), imgftype()
int	impnls(), impnll(), impnlr(), impnld()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (keyword, SZ_KEYWORD, TY_CHAR)
	call salloc (keyword2, SZ_KEYWORD, TY_CHAR)
	call salloc (value, SZ_LINE, TY_CHAR)
	call salloc (vector, IM_MAXDIM, TY_LONG)

	# Get pixel size in bytes

	pixsize = sizeof (IM_PIXTYPE(im)) * SZB_CHAR

	# Dummy write to force creation of GPB

	call amovkl (one, Meml[vector], IM_MAXDIM)

	switch (IM_PIXTYPE(im)) {
	case TY_SHORT:
	    npix = impnls (im, buffer, Meml[vector])
	case TY_USHORT,TY_INT,TY_LONG:
	    npix = impnll (im, buffer, Meml[vector])
	case TY_REAL:
	    npix = impnlr (im, buffer, Meml[vector])
	case TY_DOUBLE:
	    npix = impnld (im, buffer, Meml[vector])
	default:
	    call error (1, "gf_merge_gpb: unknown pixel datatype")
	}

	# Add each parameter in database to gpb

	for (ic = 0; Memc[db+ic] != EOS; ic = ic + SZ_KEYWORD) {
	    # Copy keyword from database

	    call strcpy (Memc[db+ic], Memc[keyword], SZ_KEYWORD)
	    jc = stridx (blank, Memc[keyword])

	    if (jc > 0)
		Memc[keyword+jc-1] = EOS

	    # Skip keywords automatically added to fits files

	    if (strdic (Memc[keyword], Memc[keyword2], SZ_KEYWORD, dict) > 0)
		if (streq (Memc[keyword], Memc[keyword2]))
		    next

	    # Get type, length, and value from template image

	    dtype = imgftype (oldim, Memc[keyword]) 
	    call imgstr (oldim, Memc[keyword], Memc[value], SZ_LINE)

	    if (dtype != TY_CHAR) {
		dlen = 1
	    } else {
		dlen = pixsize * ((strlen (Memc[value]) / pixsize) + 1)
	    }

	    # Add keyword to group parameter block

	    call gi_addpar (im, Memc[keyword], dtype, dlen, Memc[value], "")
	}

	call sfree (sp)
end
