include "rfits.h"

# CHK_ASCNAME -- Return YES if filename has an ascii extension

int procedure chk_ascname (irafname, extname)

char	irafname[ARB]	# i: Filename as stored in primary header
char	extname[ARB]	# i: Filename as stored in first extension
#--
int	status

string	asclist  TEXT_EXTLIST

bool	streq()
int	chk_list()

begin
	# If the filename in the primary header is "null_image"
	# check the filename in the first extension

	if (streq (irafname, "null_image")) {
	    status = chk_list (extname, asclist, NO)
	} else {
	    status = chk_list (irafname, asclist, YES)
	}

	return (status)
end

# CHK_TABNAME -- Return YES if filename has a table extension

int procedure chk_tabname (fname)

char	fname[ARB]	# i: file name to be checked
#--
string	tablist  TEXT_EXTLIST

bool	streq()
int	chk_list()

begin
	# The name of "null_image" is a special case, written by
	# stwfits to signal that the file was originally a table
	# In this case, the actual filename is stored in the first
	# extension

	if (streq (fname, "null_image")) 
	    return (YES)

	return (chk_list (fname, tablist, YES))
end

# CHK_LIST -- Return YES if filename extension is found in a list

int procedure chk_list (fname, list, infits)

char	fname[ARB]	# i: file name to be checked
char	list[ARB]	# i: list of extensions to check
bool	infits		# i: look for extension inside fits filename?
#--
size_t	sz_val
int	nc, ic, status
pointer	sp, root, extn, match

bool	streq()
int	fnextn(), strdic(), btoi()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (root, sz_val, TY_CHAR)
	call salloc (extn, sz_val, TY_CHAR)
	call salloc (match, sz_val, TY_CHAR)

	# If the filename has a fits extension, the original extension
	# may have been written as a suffix to the root separated
	# by an underscore

	if (infits) {
	    call gen_fname (fname, "", Memc[root], Memc[extn], SZ_FNAME)

	} else {
	    nc = fnextn (fname, Memc[extn], SZ_FNAME)
	}

	# We do the extra check with streq because strdic 
	# will match abbreviations

	call strlwr (Memc[extn])
	ic = strdic (Memc[extn], Memc[match], SZ_FNAME, list)

	if (ic > 0) {
	    status = btoi (streq (Memc[extn], Memc[match]))
	} else {
	    status = NO
	}

	call sfree (sp)
	return (status)
end
