include	"identify.h"

# ID_DOSHIFT -- Minimize residuals by constant shift.

procedure id_doshift (id, interactive)

pointer	id			# ID pointer
int	interactive		# Called interactively?

int	i, j
double	shft, delta, rms, id_fitpt()

begin
	shft = 0.
	rms = 0.
	j = 0
	for (i=1; i <= ID_NFEATURES(id); i = i + 1) {
	    if (IS_INDEFD (USER(id,i)) || WTS(id,i) == 0.)
		next
	    delta = USER(id,i) - id_fitpt (id, PIX(id,i))
	    shft = shft + delta
	    rms = rms + delta * delta
	    j = j + 1
	}

	if (j > 0) {
	    shft = shft / j
	    rms = rms / j
	    if (interactive == YES) {
	        call printf ("%s%s: Coordinate shift=%5f, rms=%5f, npts=%3d\n")
		    call pargstr (ID_IMAGE(id))
		    call pargstr (ID_SECTION(id))
		    call pargd (shft)
		    call pargd (sqrt (rms - shft ** 2))
		    call pargi (j)
	    }
	    ID_SHIFT(id) = ID_SHIFT(id) + shft
	    ID_NEWCV(id) = YES
	    ID_NEWGRAPH(id) = YES
	}
end
