# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include <imhdr.h>
include <imio.h>
include	"fxf.h"


# FXF_GETHDREXTN -- Get the default header file extension.

procedure fxf_gethdrextn (im, o_im, acmode, outstr, maxch)

pointer im, o_im		#I image descriptors
int	acmode			#I image access mode
char	outstr[ARB]		#O receives header extension
int	maxch			#I max chars out

bool	inherit
int	kernel, old_kernel
int	fnextn(), iki_getextn(), iki_getpar()

begin
	# Use the same extension as the input file if this is a new copy
	# image of the same type as the input and inherit is enabled.
	# If we have to get the extension using iki_getextn, the default
	# extension for a new image is the first extension defined (index=1).

	kernel = IM_KERNEL(im)

	old_kernel = 0
	if (acmode == NEW_COPY && o_im != NULL)
	    old_kernel = IM_KERNEL(o_im)

	inherit = (iki_getpar ("inherit") == YES)
	if (inherit && acmode == NEW_COPY && kernel == old_kernel) {
	    if (fnextn (IM_HDRFILE(im), outstr, maxch) <= 0)
		call strcpy (DEF_HDREXTN, outstr, maxch)
	} else if (iki_getextn (kernel, 1, outstr, maxch) < 0)
	    call strcpy (DEF_HDREXTN, outstr, maxch)
end
