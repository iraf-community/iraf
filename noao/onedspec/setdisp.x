include	<imhdr.h>
include	<error.h>

# SETDISP -- Set dispersion parameters in image headers.

procedure t_setdisp ()

int	list			# List of images
int	dispaxis		# Dispersion axis
char	disptype[SZ_LINE]	# Dispersion type
char	dispunit[SZ_LINE]	# Dispersion unit

char	str[SZ_LINE], image[SZ_FNAME]
pointer	im

int	imtopen(), imtgetim(), clgeti()
pointer	immap()

errchk	imtopen, immap, imaddi, imastr, imunmap

begin
	# Get parameters.
	call clgstr ("images", str, SZ_LINE)
	list = imtopen (str)

	dispaxis = clgeti ("dispaxis")
	call clgstr ("disptype", disptype, SZ_LINE)
	call clgstr ("dispunit", dispunit, SZ_LINE)

	while (imtgetim (list, image, SZ_FNAME) != EOF) {
	    iferr (im = immap (image, READ_WRITE, 0)) {
	       call erract (EA_WARN)
	       next
	    }

	    iferr {
		if (IM_NDIM(im) < dispaxis)
		    call error (0, "Dispersion axis exceeds image dimension")

		call imaddi (im, "DISPAXIS", dispaxis)
		if (disptype[1] != EOS) {
		    call sprintf (str, SZ_LINE, "CTYPE%d")
			call pargi (dispaxis)
		    call imastr (im, str, disptype)
		}
		if (dispunit[1] != EOS) {
		    call sprintf (str, SZ_LINE, "CUNIT%d")
			call pargi (dispaxis)
		    call imastr (im, str, dispunit)
		}
	    } then
		call erract (EA_WARN)

	    call imunmap (im)
	}

	call imtclose (list)
end
