# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<knet.h>

# FDIRNAME -- Return the concatenatable directory prefix for the named
# directory.  If no vfn is given (null string), a path to the current
# directory is returned.

procedure fdirname (vfn, path, maxch)

char	vfn[ARB]		# VFN of directory
char	path[ARB]		# unpacked path to directory
int	maxch

int	len1, len2, ch
int	gstrcpy()

begin
	if (vfn[1] == EOS) {
	    # Null vfn; return current directory.
	    call strcpy ("./", path, maxch)
	    return
	}

	# Do we have an OS directory reference?
	call zfxdir (vfn, path, maxch, len2)
	ch = path[len2]
	if (len2 > 0 && !(IS_ALNUM(ch) || ch == '_'))
	    return

	# Do we have an "ldir$" or "subdir/"?  If so, quit, else if the last
	# char is a normal identifier class filename char, assume that we have
	# a VFN and add the / delimiter.  This technique is not infallible,
	# and a better solution would be to have ZFXDIR or FDIRNAME itself
	# execute on the remote node.

	len1 = gstrcpy (vfn, path, maxch)
	ch = path[len1]
	if (ch == '$' || ch == '/')
	    return

	# Must be a subdirectory of the form "subdir".  Add the /.
	path[len1+1] = '/'
	path[len1+2] = EOS
end
