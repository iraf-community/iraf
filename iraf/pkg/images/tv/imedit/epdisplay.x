include	<imhdr.h>
include	"epix.h"
 
# EP_DISPLAY -- Display an image using the specified command.
# This is a temporary image display interface using CLCMDW to call
# the standard display task.  Image sections and the fill option
# can be used to simulate zoom.  One complication is that we have to
# close the image to avoid multiple access to the image.  This
# requires saving the original input subraster to allow undoing
# a change after display.
 
procedure ep_display (ep, image, erase)
 
pointer	ep		# EPIX structure
char	image[ARB]	# Image
bool	erase		# Erase
 
pointer	temp, immap(), imgs2r(), imps2r()
 
begin
	# If the output has been modified save and restore the original
	# input subraster for later undoing.
 
	if (EP_OUTDATA(ep) != NULL) {
	    call malloc (temp, EP_NPTS(ep), TY_REAL)
	    call amovr (Memr[EP_INDATA(ep)], Memr[temp], EP_NPTS(ep))
	    call imunmap (EP_IM(ep))
	    call ep_command (ep, image, erase)
	    erase = false
	    EP_IM(ep) = immap (image, READ_WRITE, 0)
	    EP_OUTDATA(ep) = imps2r (EP_IM(ep), EP_X1(ep),
		EP_X2(ep), EP_Y1(ep), EP_Y2(ep))
	    EP_INDATA(ep) = imgs2r (EP_IM(ep), EP_X1(ep),
		EP_X2(ep), EP_Y1(ep), EP_Y2(ep))
	    call amovr (Memr[EP_INDATA(ep)], Memr[EP_OUTDATA(ep)],
		EP_NPTS(ep))
	    call amovr (Memr[temp], Memr[EP_INDATA(ep)], EP_NPTS(ep))
	    call mfree (temp, TY_REAL)
	} else {
	    call imunmap (EP_IM(ep))
	    call ep_command (ep, image, erase)
	    erase = false
	    EP_IM(ep) = immap (image, READ_WRITE, 0)
	}
end
 
 
define	PARAMS	"|$image|$erase|"
define	IMAGE	1
define	ERASE	2
 
# EP_COMMAND -- Format a command with argument substitution.  This
# technique allows use of some other display command (such as CONTOUR).
 
procedure ep_command (ep, image, erase)
 
pointer	ep			# EPIX structure
char	image[ARB]		# Image name
bool	erase			# Erase?
 
int	i, j, k, nscan(), strdic(), stridxs()
pointer	sp, cmd, word
 
begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (word, SZ_LINE, TY_CHAR)
 
	call sscan (EP_COMMAND(ep))
 
	Memc[cmd] = EOS
	do i = 1, 100 {
	    call gargwrd (Memc[word], SZ_LINE)
	    if (nscan() != i)
		break
	    j = stridxs ("$", Memc[word]) - 1
	    if (j >= 0) {
	        k = strdic (Memc[word+j], Memc[word+j], SZ_LINE, PARAMS)
	        switch (k) {
	        case IMAGE:
		    call sprintf (Memc[word+j], SZ_LINE-j, "%s%s")
			call pargstr (image)
			call pargstr (EP_SECTION(ep))
		case ERASE:
		    call sprintf (Memc[word+j], SZ_LINE-j, "%b")
			call pargb (erase)
	        }
	    }
	    call strcat (Memc[word], Memc[cmd], SZ_LINE)
	    call strcat (" ", Memc[cmd], SZ_LINE)
	}
 
	if (i > 1) {
	    call clcmdw (Memc[cmd])
	    erase = false
	}
 
	call sfree (sp)
end
 
 
# EP_ZOOM -- Set an image section centered on the cursor for possible zooming.
# Zoom is simulated by loading a subraster of the image.  If the image display
# supports fill the frame this will give the effect of a zoom.
 
procedure ep_zoom (ep, xa, ya, xb, yb, key, erase)
 
pointer	ep		# EPIX structure
int	xa, ya		# Cursor
int	xb, yb		# Cursor
int	key		# Cursor key
bool	erase		# Erase?
 
real	zoom
int	nc, nl, nx, ny, zx, zy, x1, x2, y1, y2
data	zoom/1./
 
begin
	erase = true
 
	switch (key) {
	case '0':
	    zoom = 1.
	case 'E':
	    nc = IM_LEN(EP_IM(ep),1)
	    nl = IM_LEN(EP_IM(ep),2)
	    nx = abs (xa - xb) + 1
	    ny = abs (ya - yb) + 1
	    zoom = max (1., min (nc / real (nx), nl / real (ny)))
	    zx = (xa + xb) / 2.
	    zy = (ya + yb) / 2.
	case 'P':
	    zoom = max (1., zoom / 2)
	    zx = xa
	    zy = ya
	case 'Z':
	    zoom = 2 * zoom
	    zx = xa
	    zy = ya
	}
 
	if (zoom == 1.) {
	    EP_SECTION(ep) = EOS
	    return
	}
 
	nc = IM_LEN(EP_IM(ep),1)
	nl = IM_LEN(EP_IM(ep),2)
	nx = nc / zoom
	ny = nl / zoom
 
	switch (key) {
	case '1':
	    zx = zx + .4 * nx
	    zy = zy + .4 * ny
	case '2':
	    zy = zy + .4 * ny
	case '3':
	    zx = zx - .4 * nx
	    zy = zy + .4 * ny
	case '4':
	    zx = zx + .4 * nx
	case '5', 'r', 'R':
	    erase = false
	case '6':
	    zx = zx - .4 * nx
	case '7':
	    zx = zx + .4 * nx
	    zy = zy - .4 * ny
	case '8':
	    zy = zy - .4 * ny
	case '9':
	    zx = zx - .4 * nx
	    zy = zy - .4 * ny
	}
 
	# Insure the section is in bounds.
	x1 = max (1, zx - nx / 2)
	x2 = min (nc, x1 + nx)
	x1 = max (1, x2 - nx)
	y1 = max (1, zy - ny / 2)
	y2 = min (nl, y1 + ny)
	y1 = max (1, y2 - ny)
 
	zx = (x1 + x2) / 2
	zy = (y1 + y2) / 2
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
 
	# Format the image section.
	call sprintf (EP_SECTION(ep), EP_SZFNAME, "[%d:%d,%d:%d]")
	    call pargi (x1)
	    call pargi (x2)
	    call pargi (y1)
	    call pargi (y2)
end
