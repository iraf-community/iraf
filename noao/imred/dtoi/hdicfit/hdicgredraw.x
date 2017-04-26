include	<gset.h>

define	MSIZE	2.0

# HD_REDRAW -- Redraw a data point.  The old position marker is erased,
# the new marker drawn, and the cursor moved to the new location.

procedure hd_redraw (gp, oldx, oldy, newx, newy)

pointer	gp		# Pointer to graphics stream
real	oldx		# Old x coordinate
real	oldy		# Old y coordinate
real	newx		# New x coordinate
real	newy		# New y coordinate

begin
	call gseti (gp, G_PMLTYPE, GL_CLEAR)
	call gmark (gp, oldx, oldy, GM_PLUS, MSIZE, MSIZE)
	call gseti (gp, G_PMLTYPE, GL_SOLID)
	call gmark (gp, newx, newy, GM_PLUS, MSIZE, MSIZE)
	call gscur (gp, newx, newy)
end
