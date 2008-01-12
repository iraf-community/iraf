include <gset.h>

procedure axmarg (gd, margin, xlabel, ylabel, title)

pointer	gd				# Graphics descriptor
real	margin				# Margin between curve and axis
					# (fraction of window)
char	xlabel[ARB], ylabel[ARB]	# Axis labels
char	title[ARB]			# Plot title

real	left, right, bottom, top	# Window
real	dd
int	xtran, ytran

int	gstati()

errchk	glabax

begin
	if (margin > 0.0) {
	    call ggwind (gd, left, right, bottom, top)

	    xtran = gstati (gd, G_XTRAN)
	    ytran = gstati (gd, G_YTRAN)

	    if (xtran == GW_LINEAR) {
		dd = margin * (right - left)
	 	left = left - dd
		right = right + dd
	    }

	    if (ytran == GW_LINEAR) {
		dd = margin * (top - bottom)
		bottom = bottom - dd
		top = top + dd
	    }

	    call gswind (gd, left, right, bottom, top)
	}

	call glabax (gd, xlabel, ylabel, title)
end
