# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imset.h>
include	<imio.h>

# IMSTATS -- Get an IMIO option of type string.

procedure imstats (im, option, outstr, maxch)

pointer	im			# image descriptor
int	option			# imset option being queried
char	outstr[ARB]		# output string
int	maxch

begin
	switch (option) {
	case IM_IMAGENAME:
	    call strcpy (IM_NAME(im), outstr, maxch)
	default:
	    call imerr (IM_NAME(im), SYS_IMSTATUNKPAR)
	}
end
