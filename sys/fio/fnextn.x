# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FNEXTN -- Extract the file name extension from a virtual file name (or a
# machine dependent file name.  If the VFN contains no extension field, the
# null string is returned.  The number of chars in the extension string is
# returned as the function value.

int procedure fnextn (vfn, outstr, maxch)

char	vfn[ARB], outstr[maxch]
int	maxch
int	root_offset, extn_offset
int	gstrcpy()

begin
	call zfnbrk (vfn, root_offset, extn_offset)
	if (vfn[extn_offset] != EOS)
	    extn_offset = extn_offset + 1

	return (gstrcpy (vfn[extn_offset], outstr, maxch))
end
