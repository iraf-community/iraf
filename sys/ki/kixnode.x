# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# KI_XNODE -- Transfer the node prefix, if any, from one resource string to
# another.  If the output resource already has a node prefix it is replaced
# by the new one.  The output resource string is modified in place.  If the
# output string is the null string the node prefix from the input string
# is returned.  If the input string is the null string the node prefix from
# the output string is deleted, leaving only the resource name.

procedure ki_xnode (r1, r2, maxch)

char	r1[ARB]			#I input resource with optional node prefix
char	r2[ARB]			#U output resource to append node! to
int	maxch			#I max chars out

pointer	sp, rt
int	ip, nchars
int	ki_extnode()

begin
	call smark (sp)
	call salloc (rt, SZ_FNAME, TY_CHAR)

	ip = ki_extnode (r2, Memc[rt], SZ_FNAME, nchars) + 1
	call strcpy (r2[ip], Memc[rt], SZ_FNAME)
	ip = ki_extnode (r1, r2, maxch, nchars)
	call strcpy (Memc[rt], r2[nchars+1], maxch-nchars)

	call sfree (sp)
end
