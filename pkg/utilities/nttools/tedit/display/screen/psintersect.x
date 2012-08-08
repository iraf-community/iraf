include	"../curses.h"

# PS_INTERSECT -- Intersection between two rectangles
#
# B.Simon	18-Jan-89	Original

bool procedure ps_intersect (source, maxrow, maxcol, dest)

int	source[RSIZE]	# i: Source rectangle
int	maxrow		# i: Max row of clipping rectangle
int	maxcol		# i: Max column of clipping rectangle
int	dest[RSIZE]	# o: Destination rectangle
#--

begin
	# Clip source rectangle to (1,1) and (maxrow,maxcol)

	RTOP(dest) = max (1, RTOP(source))
	RLEFT(dest) = max (1, RLEFT(source))
	RBOT(dest) = min (maxrow, RBOT(source))
	RRIGHT(dest) = min (maxcol, RRIGHT(source))

	# Return true if intersection is non-empty

	return (RTOP(dest) <= RBOT(dest) && RLEFT(dest) <= RRIGHT(dest))
end
	
