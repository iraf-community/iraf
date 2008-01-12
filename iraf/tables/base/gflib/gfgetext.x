include "gf.h"

#* HISTORY *
#* B.Simon	20-Nov-00	Original code

# GF_GETEXT -- Extract the extension from an image name

procedure gf_getext (image, acmode, ext, maxch)

char	image[ARB]	# i: image name
int	acmode		# i: image access mode
char	ext[ARB]	# o: extension
int	maxch		# i: max length of extension
#--
int	nc, gn, gcount
pointer	sp, cluster, root, ksection, section

string	ambiguous  " Ambiguous image name, extension required"

int	strlen(), fnextn(), envgets(), iki_access()

begin
	# Allocate memory for strings

	call smark (sp)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (ksection, SZ_SHORTSTR, TY_CHAR)
	call salloc (section, SZ_SHORTSTR, TY_CHAR)

	# Parse image name into parts

	call imparse (image, Memc[cluster], SZ_FNAME, Memc[ksection], 
		      SZ_SHORTSTR, Memc[section], SZ_SHORTSTR, gn, gcount)

	# Get the image extension from the image name

	nc = fnextn (Memc[cluster], ext, maxch)
	nc = strlen (image)

	# Get default extension if not in image name

	if (Memc[cluster+nc-1] != '.' && ext[1] == EOS) {
	    if (acmode == NEW_COPY || acmode == NEW_FILE) {
		nc = envgets ("imtype", ext, maxch)

	    } else {
		if (iki_access (image, Memc[root], ext, acmode) == ERR)
		    call error (1, ambiguous)
	    }
	}

	call sfree (sp)
end

