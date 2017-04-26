# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<finfo.h>

# ISDIRECTORY -- Test whether the named file is a directory.  Check first to
# see if it is a subdirectory of the current directory; otherwise look in
# the environment to see if it is a logical directory.  If VFN is a directory,
# return the OS pathname of the directory in pathname, and the number of
# chars in the pathname as the function value.  Otherwise return 0.

int procedure isdirectory (vfn, pathname, maxch)

char	vfn[ARB]		# name to be tested
char	pathname[ARB]		# receives path of directory
int	maxch			# max chars out

bool	isdir
pointer	sp, fname, op
int	ip, fd, nchars, ch
long	file_info[LEN_FINFO]
int	finfo(), diropen(), gstrcpy(), strlen()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Copy the VFN string, minus any whitespace on either end.
	op = fname
	for (ip=1;  vfn[ip] != EOS;  ip=ip+1) {
	    ch = vfn[ip]
	    if (!IS_WHITE (ch)) {
		Memc[op] = ch
		op = op + 1
	    }
	}
	Memc[op] = EOS

	isdir = false
	if (finfo (Memc[fname], file_info) != ERR) {
	    isdir = (FI_TYPE(file_info) == FI_DIRECTORY)

	    if (isdir) {
 		# Ensure a trailing '/' to the pathname.
		nchars = strlen (Memc[fname])
		if (Memc[fname+nchars-1] != '/') {
	    	    Memc[fname+nchars] = '/'
	    	    Memc[fname+nchars+1] = EOS
		}

		call aclrc (pathname, maxch)
		call fdirname (Memc[fname], pathname, maxch)
		nchars = strlen (pathname)
	    }

	} else {
	    # If we get here, either VFN is a logical directory (with the
	    # $ omitted), or it is the name of a new file.

	    Memc[op] = '$'
	    Memc[op+1] = EOS
	    ifnoerr (fd = diropen (Memc[fname], 0)) {
		call close (fd)
		isdir = true
	    }

	    nchars = gstrcpy (Memc[fname], pathname, maxch)
	}

	call sfree (sp)
	if (isdir)
	    return (nchars)
	else {
	    pathname[1] = EOS
	    return (0)
	}
end
