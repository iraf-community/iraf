# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FNROOT -- Extract the root file name from a virtual file name (or from a
# machine dependent filename.  If the VFN contains no root name, the null
# string is returned.  This occurs when the VFN refers to a directory or
# device, or when the VFN string is a null string.  The number of chars in
# the root file name is returned as the function value.

int procedure fnroot (vfn, outstr, maxch)

char	vfn[ARB], outstr[maxch]
int	maxch
int	root_offset, extn_offset, nchars_root
int	gstrcpy()

begin
	call zfnbrk (vfn, root_offset, extn_offset)
	nchars_root = max(0, min(maxch, extn_offset - root_offset))

	return (gstrcpy (vfn[root_offset], outstr, nchars_root))
end
