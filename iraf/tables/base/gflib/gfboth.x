include <imhdr.h>
include "gf.h"

#* HISTORY *
#* B.Simon	13-Nov-00	Original code

# GF_BOTH -- Create a hash of keywords in both primary and extension headers

procedure gf_both (im, both)

pointer	im		# i: image descriptor
pointer both		# o: hash table descriptor
#--
char	nl
int	ic, line_len, status
pointer	ua, sp, keyword, once

data	nl  / '\n' /

int	stridx(), gf_sizehash(), gf_inithash(), gf_findhash()

begin
	# Allocate memory for temporary string

	call smark (sp)
	call salloc (keyword, SZ_KEYWORD, TY_CHAR)

	# Create hashes to store all keywords in user area and keywords
	# appearing more than once

	once = gf_inithash (gf_sizehash (im))
	both = gf_inithash (0)

	# Keep two hashes. The first for keywords already seen and the
	# second for keywords seen more than once. Return the second and
	# free the first after we are done.

	ua = IM_USERAREA(im)
	line_len = stridx (nl, Memc[ua])

	for (ic = 0; Memc[ua+ic] != EOS; ic = ic + line_len) {
	    call gf_trimhash (Memc[ua+ic], Memc[keyword], SZ_KEYWORD)
	    status = gf_findhash (once, Memc[ua+ic])

	    if (status == NOT_FOUND) {
		call gf_addhash (once, Memc[keyword])

	    } else if (status == IS_FOUND) {
		call gf_addhash (both, Memc[keyword])
	    }
	}

	call gf_freehash (once)
	call sfree (sp)
end

