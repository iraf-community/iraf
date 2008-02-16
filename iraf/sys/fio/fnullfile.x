# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FNULLFILE -- Determine if the named file is the null file.

bool procedure fnullfile (fname)

char	fname[ARB]		# null file candidate

pointer	sp, osfn
bool	first_time, bval
char	nullpath[SZ_FNAME]
int	strmatch()
bool	streq()

data	first_time /true/
string	nullfile "dev$null"

begin
	# Some simple, fast tests first.
	if (streq (fname, nullfile))
	    return (true)
	else if (strmatch (fname, "{null}") == 0)
	    return (false)

	call smark (sp)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)

	if (first_time) {
	    call fpathname (nullfile, nullpath, SZ_FNAME)
	    first_time = false
	}

	call fpathname (fname, Memc[osfn], SZ_PATHNAME)
	bval = streq (Memc[osfn], nullpath)

	call sfree (sp)
	return (bval)
end
