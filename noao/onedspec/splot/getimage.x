include	<error.h>
include	<imhdr.h>
include	<pkg/gtools.h>
include "../shdr.h"

# GETIMAGE -- Read new image pixels.

procedure getimage (image, nline, nband, wave_scl, units, im, mw, sh, gt)

char	image[ARB]
int	i, nline, nband
bool	wave_scl
char	units[ARB]
pointer	im, mw, sh, gt

int	clgeti()
pointer	immap(), smw_openim()
errchk	immap, shdr_open, shdr_system, un_changer

begin
	# Map the image if necessary.
	if (im == NULL) {
	    im = immap (image, READ_ONLY, 0)
	    mw = smw_openim (im)
	}

	# Get header info.
	if (IM_NDIM(im) > 1) {
	    if (IM_LEN(im,2) > 1) {
		if (nline == 0) 
	            nline = clgeti ("line")
	    } else
		nline = 1
	    if (IM_LEN(im,3) > 1) {
		if (nband == 0) 
		    nband = max (1, min (IM_LEN(im,3), clgeti ("band")))
	    } else
		nband = 0
	} else {
	    nline = 0
	    nband = 0
	}
	i = nline
	call shdr_open (im, mw, nline, nband, i, SHDATA, sh)

	# Cancel wavelength coordinates if not desired or set units.
	if (!wave_scl)
	    call shdr_system (sh, "physical")
	else {
	    iferr (call un_changer (UN(sh), units, Memr[SX(sh)], SN(sh), YES))
		;
	}

	# Make a title.
	call mktitle (sh, gt)
end
