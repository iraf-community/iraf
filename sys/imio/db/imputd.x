# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# IMPUTD -- Put an image header parameter of type double.

procedure imputd (im, key, dval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
double	dval			# double precision value

pointer	sp, sval
int	i, strlen()

begin
	call smark (sp)
	call salloc (sval, SZ_FNAME, TY_CHAR)

	# Reduce the precision of the encoded value if necessary to fit in
	# the FITS value field.

	for (i=NDIGITS_DP;  i >= NDIGITS_RP;  i=i-1) {
	    call sprintf (Memc[sval], SZ_FNAME, "%0.*g")
		call pargi (i)
		call pargd (dval)
	    if (strlen (Memc[sval]) < 20)
		break
	}

	# Write the new value to the header.
	call impstr (im, key, Memc[sval])

	call sfree (sp)
end
