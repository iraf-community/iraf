# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FNLDIR -- Extract the logical directory prefix from a virtual file name,
# e.g., the "ldir$" field in  "ldir$root.extn",  or the field "ldir$a/b/"
# in the vfn  "ldir$a/b/file.xtn".  Both logical and OS dependent directory
# prefixes are successfully extracted.  The prefix returned is the (logical
# or explicit) file name of the directory containing the named file.  If the
# VFN contains no logical directory field, the null string is returned,
# signifying the current directory.  The number of chars in the directory
# prefix is returned as the function value.

int procedure fnldir (vfn, outstr, maxch)

char	vfn[ARB], outstr[maxch]
int	maxch
int	root_offset, extn_offset
int	gstrcpy()

begin
	call zfnbrk (vfn, root_offset, extn_offset)
	return (gstrcpy (vfn[1], outstr, min (maxch, root_offset-1)))
end
