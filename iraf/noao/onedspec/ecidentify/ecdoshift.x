include	"ecidentify.h"

# EC_DOSHIFT -- Minimize residuals by constant shift.

procedure ec_doshift (ec, interactive)

pointer	ec			# ID pointer
int	interactive		# Called interactively?

int	i, j
double	shft, delta, rms, ec_fitpt(), ecf_getd()

begin
	shft = 0.
	rms = 0.
	j = 0
	for (i=1; i <= EC_NFEATURES(ec); i = i + 1) {
	    if (IS_INDEFD (USER(ec,i)))
		next
	    delta = USER(ec,i) - ec_fitpt (ec, APN(ec,i), PIX(ec,i))
	    delta = delta * ORDER(ec,i)
	    shft = shft + delta
	    rms = rms + delta * delta
	    j = j + 1
	}

	if (j > 0) {
	    shft = shft / j
	    rms = rms / j
	    if (interactive == YES) {
		i = EC_ORDER(ec)
	        call printf ("Coordinate shift=%5f, rms=%5f")
		    call pargd (shft / i)
		    if (j == 1)
			call pargd (INDEFD)
		    else
		        call pargd (sqrt (rms - shft ** 2) / i)
	    }
	    shft = shft + ecf_getd ("shift")
	    call ecf_setd ("shift", shft)
	    EC_NEWECF(ec) = YES
	    EC_NEWGRAPH(ec) = YES
	}
end
