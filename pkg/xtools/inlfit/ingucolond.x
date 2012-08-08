# ING_UCOLON -- User default colon commands

procedure ing_ucolond (in, gp, gt, nl, x, y, wts, npts, nvars, newgraph)

pointer	in			# INLFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
pointer	nl			# NLFIT pointer
double	x[ARB]			# Independent variables
double	y[npts]			# Dependent variables
double	wts[npts]		# Weights
int	npts			# Number of points
int	nvars			# Number of variables
int	newgraph		# New graph ? (output)

begin
	# Ring bell
	call printf ("\07\n")
end
