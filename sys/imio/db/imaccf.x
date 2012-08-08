# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMACCF -- Test if the named field exists.  NO is returned if the key is not
# found, YES otherwise.

int procedure imaccf (im, key)

pointer	im			# image descriptor
char	key[ARB]		# name of the new parameter
int	idb_kwlookup(), idb_findrecord()
pointer	rp

begin
	if ((idb_kwlookup (key) > 0) || (idb_findrecord (im, key, rp) > 0))
	    return (YES)
	else
	    return (NO)
end
