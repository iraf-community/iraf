include	<gset.h>
include	"sensfunc.h"


# SF_COLORS -- Set colors.

procedure sf_colors (gp, colors)

pointer	gp
char	colors[ARB]

int	i, nscan()

begin
	call sscan (colors)
	call gargi (i)
	if (nscan() == 1)
	    GP_PLCOLOR(gp) = i
	call gargi (i)
	if (nscan() == 2)
	    GP_CMARK(gp) = i
	call gargi (i)
	if (nscan() == 3)
	    GP_CDEL(gp) = i
	call gargi (i)
	if (nscan() == 4)
	    GP_CADD(gp) = i
end
